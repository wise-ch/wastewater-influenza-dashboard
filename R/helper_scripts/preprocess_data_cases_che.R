#### Laboratory-confirmed case data provided by FOPH ####

library(dplyr)
library(tidyr)

#' Get newest data file from FOPH.
#' @param path_to_data The directory where FOPH data is stored.
#' @return Path to newest data file.
get_newest_data <- function(path_to_data = "data/raw_data/foph_case_data") {
  files <- list.files(path = path_to_data, pattern = ".*Influenza_daten_nach_plz.xlsx$", full.names = T)
  newest_data_file <- sort(files)[length(files)]
  print(paste("Newest file found is:", newest_data_file))
  newest_data <- readxl::read_xlsx("data/raw_data/foph_case_data/2022-11-15_Influenza_daten_nach_plz.xlsx", sheet = 1) %>% 
    dplyr::rename_all(~make.names(.))
  return(newest_data)
}

print("Loading newest data from FOPH")
case_data_raw <- get_newest_data()
catchment_data <- readxl::read_xlsx("data/raw_data/plz_list.xlsx", sheet = 1) %>% 
  dplyr::rename_all(~make.names(.))

# Wrangle data
all_ww_plz <- unique(catchment_data$PLZ)
setdiff(all_ww_plz, case_data_raw$ptplz)  # These PLZ don't have reported cases

# Convert to normal dates
case_data <- case_data_raw %>%
    mutate(year_week_day = paste(meldejahr, meldewoche, "0", sep = "-")) %>%
    mutate(date = as.Date(year_week_day, format = "%Y-%U-%w") + 4.5) %>%   # Weeks go from M-Su, I take the midpoint of each week (Th)
    select(-jahrwoche, year_week_day)

case_data_clean <- case_data %>%
    filter(!is.na(typ)) %>%  # remove uncharacterized virus cases
    filter(typ %in% c("A", "B"))  # remove H1 and H3 type virus cases

# Aggregate data by catchment area
case_data_by_catchment <- case_data_clean %>%
    left_join(catchment_data, by = c("ptplz" = "PLZ")) %>%
    mutate(scaled_cases = count * `WEIGHT..as.percentage.` / 100) %>%
    group_by(typ, date, ARA.Name) %>%
    summarize(total_cases = sum(scaled_cases), .groups = "drop")

# Aggregate data across all provided PLZ (take the most data possible)
case_data_all_provided <- case_data_clean %>%
  group_by(typ, date) %>%
  summarize(total_cases = sum(count), .groups = "drop") %>%
  mutate(ARA.Name = "All provided data")

case_data_all <- rbind(
  case_data_by_catchment,
  case_data_all_provided
)

# Fill missing weeks with 0 (according to FOPH, no entry for a postal code means no cases)
all_dates <- seq.Date(
      min(case_data_all$date),
      max(case_data_all$date),
      by = "weeks")
all_wwtps <- unique(case_data_all$ARA.Name)

is_first <- T
for (wwtp in all_wwtps) {
  if (is_first) {
    full_df <- data.frame(date = all_dates, ARA.Name = wwtp)
    is_first <- F
  } else {
    full_df <- rbind(
      full_df,
      data.frame(date = all_dates, ARA.Name = wwtp)
    )
  }
}
full_df_w_typ <- rbind(full_df %>% mutate(typ = "A"), full_df %>% mutate(typ = "B"))

if (any(full_df_w_typ %>% duplicated())) {
    stop("Daily date data frame has duplicate entries, rethink fill missing values strategy!")
}
  
case_data_all_complete <- case_data_all %>%
    right_join(full_df_w_typ, by = c("date", "ARA.Name", "typ")) %>%
    mutate(total_cases = replace_na(total_cases, 0))

# Annotate different measuring periods (Re estimated for each separately)
case_data_all_complete <- case_data_all_complete %>% mutate(
  measuring_period = case_when(
    date <= as.Date("2022-05-01") ~ "2021/22",
    date >= as.Date("2022-10-01") & date <= as.Date("2023-05-01") ~ "2022/23",
    T ~ "Outside of measuring period"
  )
)
if (any(case_data_all_complete$measuring_period == "Outside of measuring period")) {
  warning("Some data is outside of a known measuring period, have you started monitoring a new season? Add the date range to code if so. Currently filtering these data out.")
  case_data_all_complete <- case_data_all_complete %>% filter(measuring_period != "Outside of measuring period")
}

# Write out cleaned data for re estimation
case_data_all_complete_renamed <- case_data_all_complete %>%
    rename("influenza_type" = "typ", "wwtp" = "ARA.Name")

write.csv(case_data_all_complete_renamed, "data/clean_data_cases_che.csv", row.names = F)

# Plot data
ggplot(data = case_data_all_complete_renamed, aes(x = date, y = total_cases)) + 
    geom_point() +
    facet_grid(wwtp + measuring_period ~ influenza_type) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b %y")

ggsave("figures/cases_by_catchment.png", width = 9, height = 9, units = "in")
