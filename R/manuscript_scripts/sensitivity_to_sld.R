# This script is to estimate Re from WW influenza concentrations.
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
output_suffix <- "_fecal_sld" # Dashboard uses data with default output name, i.e. output_suffix = ""
mean_serial_interval <- influenza_mean_serial_interval_days
std_serial_interval <- influenza_std_serial_interval_days
estimation_window <- 3  # 3 is EpiEstim default
minimum_cumul_incidence <- 12  # minimum cumulative number of infections for Re to be estimated, EstimateR default is 12
seasons_to_calculate <- c("2021/22")  # list of seasons to calculate estimates for
n_bootstrap_reps <- 500

# Import data
ww_data_ge <- read_csv("data/clean_data_ge_zh.csv", col_types = cols(sample_date = "D")) %>%
  filter(measuring_period %in% seasons_to_calculate) %>%
  filter(wwtp == "STEP Aire") %>%
  pivot_wider(
    id_cols = c("sample_date", "measuring_period", "wwtp"),
    names_from = "measurement_type",
    values_from = "mean")
ww_data_zh <- read_csv("data/clean_data_ge_zh.csv", col_types = cols(sample_date = "D")) %>%
  filter(measuring_period %in% seasons_to_calculate) %>%
  filter(wwtp == "ARA Werdhölzli") %>%
  pivot_wider(
    id_cols = c("sample_date", "measuring_period", "wwtp"),
    names_from = "measurement_type",
    values_from = "mean")

# Normalize daily copies by minimum value to get observations on the same scale as case data
# See Huisman et al. 2022 Environmental Health Perspectives - deconvolution algorithm seems to struggle when observations are too peaked
# TODO: using lowest non-zero measurement - any other strategy to consider?
normalization_factor_ge <- min(
  ww_data_ge$IAV_gc_per_day[ww_data_ge$IAV_gc_per_day > 0],
  ww_data_ge$IBV_gc_per_day[ww_data_ge$IBV_gc_per_day > 0],
  na.rm = T
)
normalization_factor_zh <- min(
  ww_data_zh$IAV_gc_per_day[ww_data_zh$IAV_gc_per_day > 0],
  ww_data_zh$IBV_gc_per_day[ww_data_zh$IBV_gc_per_day > 0],
  na.rm = T
)
ww_data_ge <- ww_data_ge %>%
  mutate(IAV_gc_per_day_norm = IAV_gc_per_day / normalization_factor_ge) %>%
  mutate(IBV_gc_per_day_norm = IBV_gc_per_day / normalization_factor_ge)
ww_data_zh <- ww_data_zh %>%
  mutate(IAV_gc_per_day_norm = IAV_gc_per_day / normalization_factor_zh) %>%
  mutate(IBV_gc_per_day_norm = IBV_gc_per_day / normalization_factor_zh)

# Estimate Re for each data stream
is_first <- T
for (data in list(ww_data_ge, ww_data_zh)) {

  data_long <- data %>% pivot_longer(
    cols = c(IAV_gc_per_day, IBV_gc_per_day, IAV_gc_per_day_norm, IBV_gc_per_day_norm),
    values_to = "observation",
    names_to = c("influenza_type", "observation_units"),
    names_pattern = "([A-Z]{3})_(.*)"
  )
  wwtp_i <- unique(data$wwtp)  # should be single plant

  for (influenza_type_j in unique(data_long$influenza_type)) {
    for (measuring_period_k in unique(data_long$measuring_period)) {
      writeLines(paste("\nEstimating Re for", wwtp_i, "influenza", influenza_type_j, "in period", measuring_period_k))

      # Get appropriate data
      data_filtered <- data_long %>%
        filter(influenza_type == influenza_type_j, measuring_period == measuring_period_k, observation_units == "gc_per_day_norm") %>%
        filter(!is.na(observation)) %>%  # this is just to remove leading NA measurements for ZH IBV
        arrange(sample_date)

      # Interpolate measurements to daily values
      data_interpolated <- interpolate_measurements_cubic_spline(
        data_frame = data_filtered,
        date_col = "sample_date",
        measurement_cols = c("observation")
      )

      measurements = list(
        values = data_interpolated$observation,
        index_offset = 0)

      # Try to estimate Re (handling case where not enough incidence observed to calculate)
      estimates_bootstrap <- tryCatch({
        get_block_bootstrapped_estimate(
          measurements$values,
          N_bootstrap_replicates = n_bootstrap_reps,
          smoothing_method = "LOESS",
          deconvolution_method = "Richardson-Lucy delay distribution",
          estimation_method = "EpiEstim sliding window",
          uncertainty_summary_method = "original estimate - CI from bootstrap estimates",
          minimum_cumul_incidence = minimum_cumul_incidence,
          combine_bootstrap_and_estimation_uncertainties = TRUE,
          delay = influenza_distribution_infection_to_shedding_fecal_moments,
          estimation_window = estimation_window,
          mean_serial_interval = mean_serial_interval,
          std_serial_interval = std_serial_interval,
          ref_date = min(data_filtered$sample_date),
          time_step = "day",
          output_Re_only = F) %>%
          mutate(observation_type = wwtp_i, influenza_type = influenza_type_j, measuring_period = measuring_period_k)
      },
      error = function(cond) {
        message(paste("Couldn't calculate Re for", wwtp_i, "influenza", influenza_type_j))
        message(cond)
        # Make observation data frame anyways
        return(data.frame(
          date = data_filtered$sample_date,
          observed_incidence = data_filtered$observation,
          CI_down_observed_incidence = NA,
          CI_up_observed_incidence = NA,
          smoothed_incidence = NA,
          CI_down_smoothed_incidence = NA,
          CI_up_smoothed_incidence = NA,
          deconvolved_incidence = NA,
          CI_down_deconvolved_incidence = NA,
          CI_up_deconvolved_incidence = NA,
          Re_estimate = NA,
          CI_down_Re_estimate = NA,
          CI_up_Re_estimate = NA,
          Re_highHPD = NA,
          Re_lowHPD = NA,
          bootstrapped_CI_down_Re_estimate = NA,
          bootstrapped_CI_up_Re_estimate = NA,
          observation_type = wwtp_i,
          influenza_type = influenza_type_j,
          measuring_period = measuring_period_k
        ))
      })

      # Aggregate data and Re estimates
      if (is_first) {
        data_all <- data_interpolated
        estimates_bootstrap_all <- estimates_bootstrap
        is_first <- F
      } else {
        data_all <- rbind(data_all, data_interpolated)
        estimates_bootstrap_all <- rbind(estimates_bootstrap_all, estimates_bootstrap)
      }
    }
  }
}

# Write out data used for Re inference and results
write.csv(
  x = data_all,
  file = paste0("data/data_used_in_manuscript/ww_loads", output_suffix, ".csv")
)

write.csv(
  x = estimates_bootstrap_all,
  file = paste0("data/data_used_in_manuscript/ww_re_estimates", output_suffix, ".csv")
)

