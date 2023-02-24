##### Canton Basel-Stadt data: Basel #####

library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

data_bs <- readxl::read_xlsx(here::here("data/raw_data/basel_wwtp_data/Basel CoroWWmonitoring_Influenza_2023-02-20.xlsx"),sheet = 1,skip = 2)

clean_data_bs <- data_bs %>%
  filter((!is.na(`InfA (gc/L)`)) | (!is.na(`InfB (gc/L)`)), !is.na(Datum)) %>%
  transmute(
    sample_date = as.Date(Datum),
    flow_ProRheno_L = `Flussmenge ProRheno (LITER) (Column AH)`,
    IAV_gc_per_mL_WW = `InfA (gc/L)` / 1000,
    IBV_gc_per_mL_WW = `InfB (gc/L)` / 1000,
    RSV_gc_per_mL_WW = NA) %>%
  mutate(
    IAV_gc_per_day = IAV_gc_per_mL_WW * 1000 * flow_ProRheno_L,
    IBV_gc_per_day = IBV_gc_per_mL_WW * 1000 * flow_ProRheno_L,
    RSV_gc_per_day = RSV_gc_per_mL_WW * 1000 * flow_ProRheno_L)  # measurements to daily loads

# Annotate different measuring periods (Re estimated for each separately)
clean_data_bs <- clean_data_bs %>% mutate(
  measuring_period = case_when(
    sample_date <= as.Date("2022-07-01") ~ "2021/22",
    sample_date >= as.Date("2022-09-01") ~ "2022/23",
    T ~ "Outside of measuring period"
  ),
  wwtp = "ARA Basel"
)
if (any(clean_data_bs$measuring_period == "Outside of measuring period")) {
  warning("Some data is outside of a known measuring period, have you started monitoring a new season? Add the date range to code if so.")
}

clean_data_long_bs <- clean_data_bs %>%
  select(sample_date, wwtp, measuring_period, IAV_gc_per_mL_WW, IAV_gc_per_day, IBV_gc_per_mL_WW, IBV_gc_per_day, RSV_gc_per_mL_WW, RSV_gc_per_day) %>%
  pivot_longer(cols = c(IAV_gc_per_mL_WW, IAV_gc_per_day, IBV_gc_per_mL_WW, IBV_gc_per_day, RSV_gc_per_mL_WW, RSV_gc_per_day), names_to = "measurement_type")

#write.csv(clean_data_long_bs, "data/data_used_in_manuscript/unnaggregated_data_bs.csv", row.names = F)

clean_data_long_means_bs <- clean_data_long_bs %>%
  group_by(sample_date, wwtp, measuring_period, measurement_type) %>%
  summarize(
    mean = mean(value),
    min = min(value),
    max = max(value),
    n_measurements = n(),
    .groups = "drop")

write.csv(clean_data_long_means_bs, "data/clean_data_bs.csv", row.names = F)
