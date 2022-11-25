##### EAWAG data: Geneva and Zurich #####

library(ggplot2)
library(dplyr)
library(tidyr)

#' Get newest data file from EAWAG.
#' @param path_to_data The directory where EAWAG data is stored.
#' @return Path to newest data file.
get_newest_data <- function(path_to_data = "data/raw_data/eawag_data") {
  files <- list.files(path = path_to_data, pattern = "^LatestFluData.*csv$", full.names = T)
  newest_data_file <- sort(files)[length(files)]
  print(paste("Newest file found is:", newest_data_file))
  newest_data <- read.csv(newest_data_file) %>% mutate(sample_date = as.Date(sample_date))
  return(newest_data)
}

print("Loading newest data from EAWAG")
data_eawag <- get_newest_data()

print(paste("Loading flow data from online EAWAG sensors"))
flow_data_ge<- read.table(
  "https://sensors-eawag.ch/sars/__data__/processed_normed_data_geneve_v2.csv",
  sep = ";",
  header = T,
  check.names = F
)
flow_data_zh <- read.table(
  "https://sensors-eawag.ch/sars/__data__/processed_normed_data_zurich_v2.csv",
  sep = ";",
  header = T,
  check.names = F
)

# Wrangle data
control_data_eawag <- data_eawag %>% filter(sample_type %in% c("pos", "ntc"))

control_data_long_eawag <- control_data_eawag %>%
  pivot_longer(cols = c(
    IAV.M_.gc.mLWW.,
    IBV.M_.gc.mLWW.,
    SARS.N1_.gc.mLWW.,
    SARS.N2_.gc.mLWW.
  ))

clean_data_eawag <- data_eawag %>%
  filter(TotalDroplets >= 15000, sample_type == "ww") %>%
  rename(IAV_gc_per_mL_WW = IAV.M_.gc.mLWW.) %>%
  rename(IBV_gc_per_mL_WW = IBV.M_.gc.mLWW.) %>%
  rename(SARS2_N1_gc_per_mL_WW = SARS.N1_.gc.mLWW.) %>%
  rename(SARS2_N2_gc_per_mL_WW = SARS.N2_.gc.mLWW.)

# Annotate different measuring periods (Re estimated for each separately)
clean_data_eawag <- clean_data_eawag %>% mutate(
  measuring_period = case_when(
    sample_date <= as.Date("2022-05-01") ~ "2021/22",
    sample_date >= as.Date("2022-09-01") ~ "2022/23",
    T ~ "Outside of measuring period"
  )
)
if (any(clean_data_eawag$measuring_period == "Outside of measuring period")) {
  warning("Some data is outside of a known measuring period, have you started monitoring a new season? Add the date range to code if so.")
}

flow_data_ge_clean <- flow_data_ge %>%
  rename("date" = "") %>%
  mutate(wwtp = "STEP Aire") %>%
  mutate(date = as.Date(date)) %>%
  select(wwtp, date, `flow [m^3/d]`)
flow_data_zh_clean <- flow_data_zh %>%
  rename("date" = "") %>%
  mutate(wwtp = "ARA Werdhölzli") %>%
  mutate(date = as.Date(date)) %>%
  select(wwtp, date, `flow [m^3/d]`)

ml_per_l <- 1000
l_per_m3 <- 1000

clean_data_ge <- clean_data_eawag %>%
  filter(wwtp == "STEP Aire") %>%
  left_join(flow_data_ge_clean, by = c("sample_date" = "date", "wwtp")) %>%
  mutate("IBV_gc_per_day" = IBV_gc_per_mL_WW * `flow [m^3/d]` * ml_per_l * l_per_m3) %>%
  mutate("IAV_gc_per_day" = IAV_gc_per_mL_WW * `flow [m^3/d]` * ml_per_l * l_per_m3)
clean_data_zh <- clean_data_eawag %>%
  filter(wwtp == "ARA Werdhölzli") %>%
  left_join(flow_data_zh_clean, by = c("sample_date" = "date", "wwtp")) %>%
  mutate("IBV_gc_per_day" = IBV_gc_per_mL_WW * `flow [m^3/d]` * ml_per_l * l_per_m3) %>%
  mutate("IAV_gc_per_day" = IAV_gc_per_mL_WW * `flow [m^3/d]` * ml_per_l * l_per_m3)

clean_data_long_eawag <- rbind(clean_data_ge, clean_data_zh) %>%
  select(
    sample_date,
    wwtp,
    measuring_period,
    target,
    IAV_gc_per_mL_WW,
    IBV_gc_per_mL_WW,
    SARS2_N1_gc_per_mL_WW,
    SARS2_N2_gc_per_mL_WW,
    IAV_gc_per_day,
    IBV_gc_per_day) %>%
  pivot_longer(cols = c(
    IAV_gc_per_mL_WW,
    IBV_gc_per_mL_WW,
    SARS2_N1_gc_per_mL_WW,
    SARS2_N2_gc_per_mL_WW,
    IAV_gc_per_day,
    IBV_gc_per_day),
    names_to = "measurement_type")

clean_data_long_means_eawag <- clean_data_long_eawag %>%
  group_by(sample_date, measuring_period, wwtp, target, measurement_type) %>%
  summarize(
    mean = mean(value),
    min = min(value),
    max = max(value),
    n_measurements = n(),
    .groups = "drop")

# Plot controls (manually inspect)
ggplot(data = control_data_long_eawag, aes(x = target, y = value)) +
  geom_boxplot(aes(color = target)) +
  facet_grid(name ~ sample_type, scales = "free_y") +
  labs(x = "Target", y = "Measurement (genome copies per mL wastewater)")

ggsave("figures/all_controls_eawag.png", width = 7, height = 7, units = "in")

# Plot all data
ggplot(data = clean_data_long_means_eawag,
       aes(x = as.Date(sample_date), y = mean, color = target)) +
  geom_point() +
  geom_errorbar(aes(ymin = min, ymax = max), width = 5) +
  facet_grid(measurement_type ~ wwtp + measuring_period, scales = "free") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  theme(legend.position = "bottom") +
  labs(x = element_blank(), y = "Mean and min/max measurement across replicates")

ggsave("figures/all_data_eawag.png", width = 9, height = 9, units = "in")

# Select data for influenza analysis
clean_data_long_eawag_for_analysis <- clean_data_long_eawag %>%
  filter(!(target %in% c("sn1flu", "sn1(2x)flu"))) %>%  # remove data from earlier assays with different primer and/or probe concentrations
  filter(!(measurement_type %in% c("IBV_gc_per_mL_WW", "IBV_gc_per_day") & target == "iabv")) %>%  # remove IABV assay data for IBV due to poor separation between positive and negative droplets in controls
  filter(!(sample_date <= as.Date("2021-12-07") & measurement_type %in% c("IBV_gc_per_mL_WW", "IBV_gc_per_day") & wwtp == "ARA Werdhölzli")) %>%  # single non-zero measurement followed by many 0 measurements for IBV in Zurich cause estimateR bug where minimum incidence not met, this is a crude solution to remove early measurements
  filter(!(measurement_type %in% c("SARS2_N1_gc_per_mL_WW", "SARS2_N2_gc_per_mL_WW")))  # don't analyze SARS-CoV-2

# write.csv(clean_data_long_eawag_for_analysis, "data/data_used_in_manuscript/unnaggregated_data_ge_zh.csv", row.names = F)

clean_data_long_means_eawag_for_analysis <- clean_data_long_eawag_for_analysis %>%
  group_by(sample_date, wwtp, measuring_period, measurement_type) %>%
  summarize(
    mean = mean(value),
    min = min(value),
    max = max(value),
    n_measurements = n(),
    .groups = "drop")

# Plot data used in analysis
ggplot(data = clean_data_long_means_eawag_for_analysis,
       aes(x = as.Date(sample_date), y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = min, ymax = max), width = 5) +
  facet_grid(measurement_type ~ wwtp + measuring_period, scales = "free") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  theme(legend.position = "bottom") +
  labs(x = element_blank(), y = "Mean and min/max measurement across analyzed replicates")

ggsave("figures/analyzed_data_eawag.png", width = 9, height = 9, units = "in")

write.csv(clean_data_long_means_eawag_for_analysis, "data/clean_data_ge_zh.csv", row.names = F)
