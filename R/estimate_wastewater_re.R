# This script is to estimate Re from WW influenza concentrations.
# It's based on JH's script https://github.com/JSHuisman/wastewaterRe/blob/main/code/wastewater_Zurich.R.

# Load dependencies
library(dplyr)
library(tidyr)
library(readr)
library(estimateR)

source("R/helper_scripts/functions.R")
source("R/helper_scripts/parameters.R")

# Set variables
delay_dist_info <- influenza_distribution_infection_to_shedding_fecal_moments
mean_serial_interval <- influenza_mean_serial_interval_days
std_serial_interval <- influenza_std_serial_interval_days
estimation_window <- 3  # 3 is EpiEstim default
n_bootstrap_reps <- 50  # TODO: increase

# Import data
ww_data_bs <- read_csv("data/clean_data_bs.csv", col_types = cols(sample_date = "D")) %>%
  pivot_wider(
    id_cols = c("sample_date", "wwtp", "n_measurements"), 
    names_from = "measurement_type", 
    values_from = "mean")
ww_data_ge <- read_csv("data/clean_data_ge_zh.csv", col_types = cols(sample_date = "D")) %>%
  filter(wwtp == "STEP Aire") %>%
  pivot_wider(
    id_cols = c("sample_date", "wwtp", "n_measurements"), 
    names_from = "measurement_type", 
    values_from = "mean")
ww_data_zh <- read_csv("data/clean_data_ge_zh.csv", col_types = cols(sample_date = "D")) %>%
  filter(wwtp == "ARA Werdhölzli") %>%
  pivot_wider(
    id_cols = c("sample_date", "wwtp", "n_measurements"), 
    names_from = "measurement_type", 
    values_from = "mean")

# Normalize daily copies by minimum value to get observations on the same scale as case data
# See Huisman et al. 2022 Environmental Health Perspectives - deconvolution algorithm seems to struggle when observations are too peaked
# TODO: using lowest non-zero measurement - any other strategy to consider?
normalization_factor_bs <- min(
  ww_data_bs$IAV_gc_per_day[ww_data_bs$IAV_gc_per_day > 0],
  ww_data_bs$IBV_gc_per_day[ww_data_bs$IBV_gc_per_day > 0]
)
normalization_factor_ge <- min(
  ww_data_ge$IAV_gc_per_day[ww_data_ge$IAV_gc_per_day > 0],
  ww_data_ge$IBV_gc_per_day[ww_data_ge$IBV_gc_per_day > 0]
)
normalization_factor_zh <- min(
  ww_data_zh$IAV_gc_per_day[ww_data_zh$IAV_gc_per_day > 0],
  ww_data_zh$IBV_gc_per_day[ww_data_zh$IBV_gc_per_day > 0]
)
ww_data_bs <- ww_data_bs %>%
  mutate(IAV_gc_per_day_norm = IAV_gc_per_day / normalization_factor_bs) %>%
  mutate(IBV_gc_per_day_norm = IBV_gc_per_day / normalization_factor_bs)
ww_data_ge <- ww_data_ge %>%
  mutate(IAV_gc_per_day_norm = IAV_gc_per_day / normalization_factor_ge) %>%
  mutate(IBV_gc_per_day_norm = IBV_gc_per_day / normalization_factor_ge)
ww_data_zh <- ww_data_zh %>%
  mutate(IAV_gc_per_day_norm = IAV_gc_per_day / normalization_factor_zh) %>%
  mutate(IBV_gc_per_day_norm = IBV_gc_per_day / normalization_factor_zh)

# Interpolate measurements to daily values
ww_data_interp_bs <- interpolate_measurements(
  data_frame = ww_data_bs,
  date_col = "sample_date",
  measurement_cols = c("IAV_gc_per_day", "IAV_gc_per_mL_WW", "IBV_gc_per_day", "IBV_gc_per_mL_WW", "IAV_gc_per_day_norm", "IBV_gc_per_day_norm")
)
ww_data_interp_ge <- interpolate_measurements(
  data_frame = ww_data_ge,
  date_col = "sample_date",
  measurement_cols = c("IAV_gc_per_day", "IAV_gc_per_mL_WW", "IBV_gc_per_day", "IBV_gc_per_mL_WW", "IAV_gc_per_day_norm", "IBV_gc_per_day_norm")
)
ww_data_interp_zh <- interpolate_measurements(
  data_frame = ww_data_zh,
  date_col = "sample_date",
  measurement_cols = c("IAV_gc_per_day", "IAV_gc_per_mL_WW", "IBV_gc_per_day", "IBV_gc_per_mL_WW", "IAV_gc_per_day_norm", "IBV_gc_per_day_norm")
)

# Put data in long format
data_all <- rbind(
  ww_data_interp_bs %>% mutate(wwtp = "ARA Basel"),
  ww_data_interp_ge %>% mutate(wwtp = "STEP Aire"),
  ww_data_interp_zh %>% mutate(wwtp = "ARA Werdhölzli")
) %>%
  pivot_longer(
  cols = c(IAV_gc_per_day, IBV_gc_per_day, IAV_gc_per_day_norm, IBV_gc_per_day_norm),
  values_to = "observation",
  names_to = c("influenza_type", "observation_units"),
  names_pattern = "([A-Z]{3})_(.*)"
) %>% select(sample_date, wwtp, n_measurements, is_observation, influenza_type, observation, observation_units)

# Write out data used for Re inference
write.csv(x = data_all, file = "app/data/ww_loads.csv")

# Estimate Re for each data stream
is_first <- T
for (wwtp_i in unique(data_all$wwtp)) {
  for (influenza_type_j in unique(data_all$influenza_type)) {
    writeLines(paste("\nEstimating Re for", wwtp_i, "influenza", influenza_type_j))
    
    # Get appropriate data
    data_filtered <- data_all %>%
      filter(wwtp == wwtp_i, influenza_type == influenza_type_j, observation_units == "gc_per_day_norm") %>%
      arrange(sample_date)
    
    measurements = list(
      values = data_filtered$observation,
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
        combine_bootstrap_and_estimation_uncertainties = TRUE,
        delay = delay_dist_info,
        estimation_window = estimation_window,
        mean_serial_interval = mean_serial_interval,
        std_serial_interval = std_serial_interval,
        ref_date = min(data_filtered$sample_date),
        time_step = "day",
        output_Re_only = F) %>% 
        mutate(observation_type = wwtp_i, influenza_type = influenza_type_j)
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
        influenza_type = influenza_type_j
      ))
    })
    
    # Aggregate Re estimates
    if (is_first) {
      estimates_bootstrap_all <- estimates_bootstrap
      is_first <- F
    } else {
      estimates_bootstrap_all <- rbind(estimates_bootstrap_all, estimates_bootstrap)
    } 
  }
}

write.csv(
  x = estimates_bootstrap_all,
  file = "app/data/ww_re_estimates.csv"
)
