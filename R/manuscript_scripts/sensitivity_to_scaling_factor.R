# This script is to investigate how sensitive wastewater Re estimation is to the scaling factor applied to bring wastewater measurements on the same scale as case data

# Load dependencies
library(dplyr)
library(pracma)  # for cubic spline interpolation
library(tidyr)
library(readr)
library(estimateR)

source("R/helper_scripts/functions.R")
source("R/helper_scripts/parameters.R")

# Set variables
delay_dist_info <- influenza_distribution_infection_to_shedding_carrat2008_moments
mean_serial_interval <- influenza_mean_serial_interval_days
std_serial_interval <- influenza_std_serial_interval_days
estimation_window <- 3  # 3 is EpiEstim default
minimum_cumul_incidence <- 12  # minimum cumulative number of infections for Re to be estimated, EstimateR default is 12
n_bootstrap_reps <- 500

# Import data
ww_data_zh <- read_csv("data/clean_data_ge_zh.csv", col_types = cols(sample_date = "D")) %>%
  filter(wwtp == "ARA Werdhölzli") %>%
  pivot_wider(
    id_cols = c("sample_date", "measuring_period", "wwtp"),
    names_from = "measurement_type",
    values_from = "mean")

normalization_factor_zh <- min(
  ww_data_zh$IAV_gc_per_day[ww_data_zh$IAV_gc_per_day > 0],
  ww_data_zh$IBV_gc_per_day[ww_data_zh$IBV_gc_per_day > 0],
  na.rm = T
)

normalization_factors <- list(
  "Minimum measurement is 1 case" = normalization_factor_zh,  # scaling used for main analysis
  "Minimum measurement is 10 cases" = normalization_factor_zh / 10,
  "Minimum measurement is 100 cases" = normalization_factor_zh / 100,
  "Minimum measurement is 1000 cases" = normalization_factor_zh / 1000
)

# Estimate Re for each data stream
is_first <- T
for (i in 1:length(normalization_factors)) {

  ww_data_zh_norm <- ww_data_zh %>%
    mutate(IAV_gc_per_day_norm = IAV_gc_per_day / normalization_factors[[i]])

  data_long <- ww_data_zh_norm %>% pivot_longer(
    cols = c(IAV_gc_per_day, IAV_gc_per_day_norm),
    values_to = "observation",
    names_to = c("influenza_type", "observation_units"),
    names_pattern = "([A-Z]{3})_(.*)"
  )
  wwtp_i <- "ARA Werdhölzli"

  for (influenza_type_j in "IAV") {
    for (measuring_period_k in "2021/22") {
      writeLines(paste("\nEstimating Re for", wwtp_i, "influenza", influenza_type_j, "in period", measuring_period_k, "with scaling", normalization_factors[[i]]))

      # Get appropriate data
      data_filtered <- data_long %>%
        filter(influenza_type == influenza_type_j, measuring_period == measuring_period_k, observation_units == "gc_per_day_norm") %>%
        filter(!is.na(observation)) %>%  # this is just to remove leading NA measurements for ZH IBV
        arrange(sample_date)

      # Interpolate measurements to daily values
      data_interpolated <- interpolate_measurements(
        data_frame = data_filtered,
        date_col = "sample_date",
        measurement_cols = c("observation")
      )

      measurements = list(
        values = data_interpolated$observation,
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
        ref_date = min(data_filtered$sample_date),
        time_step = "day",
        output_Re_only = F) %>%
        mutate(observation_type = wwtp_i, influenza_type = influenza_type_j, measuring_period = measuring_period_k) %>%
        mutate(
          max_incidence = round(max(measurements$values), digits = 0),
          scaling_type = names(normalization_factors)[i])

      # Aggregate data and Re estimates
      if (is_first) {
        estimates_bootstrap_all <- estimates_bootstrap
        is_first <- F
      } else {
        estimates_bootstrap_all <- rbind(estimates_bootstrap_all, estimates_bootstrap)
      }
    }
  }
}

# Write out results
write.csv(
  x = estimates_bootstrap_all,
  file = "data/data_used_in_manuscript/ww_re_scaling_sensitivity.csv"
)
