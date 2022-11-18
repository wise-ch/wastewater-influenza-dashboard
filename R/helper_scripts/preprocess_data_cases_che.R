#### Laboratory-confirmed case data provided by FOPH ####

library(xlsx)
library(dplyr)
library(tidyr)

case_data_raw <- read.xlsx("data/raw_data/foph_case_data/2022-11-15_Influenza_daten_nach_plz.xlsx", sheetIndex = 1)
catchment_data <- read.xlsx("data/raw_data/plz_list.xlsx", sheetIndex = 1)

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
    summarize(total_scaled_cases = sum(scaled_cases), .groups = "drop")

# Fill missing weeks with 0 (according to FOPH, no entry for a postal code means no cases)
all_dates <- seq.Date(
      min(case_data_by_catchment$date),
      max(case_data_by_catchment$date),
      by = "weeks")
all_wwtps <- unique(case_data_by_catchment$ARA.Name)

full_df <- data.frame(
    date = rep(all_dates, length(all_wwtps)),
    ARA.Name = rep(all_wwtps, length(all_dates))
  ) %>% arrange(date, ARA.Name)
full_df_w_typ <- rbind(full_df %>% mutate(typ = "A"), full_df %>% mutate(typ = "B"))

if (any(full_df_w_typ %>% duplicated())) {
    stop("Daily date data frame has duplicate entries, rethink fill missing values strategy!")
}
  
case_data_by_catchment_complete <- case_data_by_catchment %>%
    right_join(full_df_w_typ, by = c("date", "ARA.Name", "typ")) %>%
    mutate(total_scaled_cases = replace_na(total_scaled_cases, 0))

# Write out cleaned data for re estimation
case_data_by_catchment_complete_renamed <- case_data_by_catchment_complete %>%
    rename("influenza_type" = "typ", "wwtp" = "ARA.Name")

write.csv(case_data_by_catchment_complete_renamed, "data/clean_data_cases_che.csv", row.names = F)

# Plot data
ggplot(data = case_data_by_catchment_complete, aes(x = date, y = total_scaled_cases)) + 
    geom_point() +
    facet_grid(ARA.Name ~ typ) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b %y")

ggsave("figures/cases_by_catchment.png", width = 9, height = 9, units = "in")
