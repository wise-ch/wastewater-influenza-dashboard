##### Canton Basel-Stadt data: Basel #####

library(ggplot2)
library(dplyr)
library(tidyr)

data_bs <- read.csv("data/raw_data/Basel CoroWWmonitoring_Influenza copy.csv")

clean_data_bs <- data_bs %>%
  filter(!(is.na(`InfA..gc..L.`) & is.na(`Inf.B..gc..L.`))) %>%  # remove days without measurement
  mutate(sample_date = as.Date(Datum, format = "%d.%m.%y")) %>%
  mutate(wwtp = "ARA Basel") %>%
  mutate(IAV_gc_per_mL_WW = `InfA..gc..L.` / 1000) %>%
  mutate(IBV_gc_per_mL_WW = `Inf.B..gc..L.` / 1000) %>%  # per L to per mL WW
  mutate(IAV_gc_per_day = `InfA..gc..L.` * Inflow.ProRheno..LITER...Column.AH.) %>%
  mutate(IBV_gc_per_day = `Inf.B..gc..L.` * Inflow.ProRheno..LITER...Column.AH.)  # measurements to daily loads

clean_data_long_bs <- clean_data_bs %>%
    select(sample_date, wwtp, IAV_gc_per_mL_WW, IBV_gc_per_mL_WW, IAV_gc_per_day, IBV_gc_per_day) %>%
    pivot_longer(cols = c(IAV_gc_per_mL_WW, IBV_gc_per_mL_WW, IAV_gc_per_day, IBV_gc_per_day), names_to = "measurement_type")

clean_data_long_means_bs <- clean_data_long_bs %>%
  group_by(sample_date, wwtp, measurement_type) %>%
  summarize(
    mean = mean(value), 
    min = min(value), 
    max = max(value), 
    n_measurements = n(), 
    .groups = "drop")

write.csv(clean_data_long_means_bs, "data/clean_data_bs.csv", row.names = F)
