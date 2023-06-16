# This script is to estimate Re from ILI data corrected for percent positivity for influenza.
# It's based on JH's script https://github.com/JSHuisman/wastewaterRe/blob/main/code/wastewater_Zurich.R.

# Load dependencies
library(dplyr)
library(pracma)  # for cubic spline interpolation
library(tidyr)
library(readr)
library(estimateR)

source("R/helper_scripts/functions.R")
source("R/helper_scripts/parameters.R")

# Set variables
delay_dist_info <- influenza_distribution_infection_to_symptoms_moments
mean_serial_interval <- influenza_mean_serial_interval_days
std_serial_interval <- influenza_std_serial_interval_days
estimation_window <- 3  # 3 is EpiEstim default
minimum_cumul_incidence <- 12  # minimum cumulative number of infections for Re to be estimated, EstimateR default is 12
date_range <- c(as.Date("2021-10-01"), as.Date("2022-05-01"))  # 2021/22 season studied
measuring_period <- "2021/22"
observation_type <- "All Switzerland"
influenza_type <- "ILI corrected for percent positivity"
n_bootstrap_reps <- 500

# Import data
ili_data <- read.csv("data/data_used_in_manuscript/ili_2021_2022.csv")
ili_percent_positivity <- read.csv("data/data_used_in_manuscript/sentinella_percent_positivity.csv")

# Clean data
ili_data_clean <- ili_data %>%
  full_join(y = ili_percent_positivity, by = c("Jahr" = "year", "Sentinella.Woche" = "week")) %>%
  mutate(percent_pos_influenza = case_when(
    percent.positivity.from.plot < 0 ~ 0,
    T ~ percent.positivity.from.plot)) %>%  # plot digitization error - percent pos should be strictly >= 0
  mutate(est_influenza_cases = Konsultationen.pro.100.000.Einwohner * percent_pos_influenza / 100) %>%
  mutate(year_week_day = paste(Jahr, Sentinella.Woche, "0", sep = "-")) %>%
  mutate(first_day_of_week = as.Date(year_week_day, format = "%Y-%U-%w") - 1) %>%  # Sentinella weeks are Sat-Fri
  mutate(date = first_day_of_week + 3) %>%  # date by midpoint of week (Tues)
  filter(date >= date_range[[1]], date <= date_range[[2]]) %>%
  arrange(date)

# Estimate Re

# Interpolate weekly data to daily data (linear interpolation)
ili_data_interpolated <- interpolate_measurements_cubic_spline(
  data_frame = ili_data_clean %>% mutate(daily_cases = est_influenza_cases / 7),
  date_col = "date",
  measurement_cols = "daily_cases")

measurements = list(
  values = ili_data_interpolated$daily_cases,
  index_offset = 0)

# Try to estimate Re (handling case where not enough incidence observed to calculate)
estimates_bootstrap <- get_block_bootstrapped_estimate(
    measurements$values,
    N_bootstrap_replicates = n_bootstrap_reps,
    smoothing_method = "LOESS",
    deconvolution_method = "Richardson-Lucy delay distribution",
    estimation_method = "EpiEstim sliding window",
    uncertainty_summary_method = "original estimate - CI from bootstrap estimates",
    minimum_cumul_incidence = minimum_cumul_incidence,
    combine_bootstrap_and_estimation_uncertainties = TRUE,
    delay = delay_dist_info,
    estimation_window = estimation_window,
    mean_serial_interval = mean_serial_interval,
    std_serial_interval = std_serial_interval,
    ref_date = min(ili_data_clean$date),
    time_step = "day",
    output_Re_only = F) %>%
    mutate(observation_type = observation_type, influenza_type = influenza_type, measuring_period = measuring_period)

# Write out data used for Re inference and results
write.csv(
  x = ili_data_interpolated,
  file = "data/data_used_in_manuscript/corrected_ili.csv"
)

write.csv(
  x = estimates_bootstrap,
  file = "data/data_used_in_manuscript/corrected_ili_re_estimates.csv",
  row.names = F
)
