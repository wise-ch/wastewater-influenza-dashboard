# This script is to estimate Re from sentinella ili consulations and test data.
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
seasons_to_calculate <- c("2021/22","2022/23")  # list of seasons to calculate estimates for (2021/22 is cached)
n_bootstrap_reps <- 50


cnslt_data <- read_csv("data/clean_data_sentinella.csv", col_types = cols(date = "D")) %>%
  filter(measuring_period %in% seasons_to_calculate)

cnslt_data = cnslt_data %>% filter(pathogen %in% c('RSV', 'IVA'))


# Estimate Re for each data stream
is_first <- T
for (region in unique(cnslt_data$region_nr)) {
  for (pathogen_x in unique(cnslt_data$pathogen)) {
    for (measuring_period_k in unique(cnslt_data$measuring_period)) {
      writeLines(paste("\nEstimating Re for", pathogen_x, "in sentinella region", region, "for", measuring_period_k, "season"))
      
      # Get appropriate data
      cnslt_data_filtered <- cnslt_data %>%
        filter(region_nr == region, pathogen == pathogen_x, measuring_period == measuring_period_k) %>%
        arrange(date)
      
      # Interpolate weekly data to daily data (linear interpolation)
      cnslt_data_interpolated <- interpolate_measurements_cubic_spline(
        data_frame = cnslt_data_filtered,
            date_col = 'date',
            measurement_cols = 'total_cnslt') %>% 
        mutate(region_nr = region, 
               measuring_period = measuring_period_k, 
               pathogen = pathogen_x)
      
      measurements = list(
        values = cnslt_data_interpolated$total_cnslt,
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
          delay = delay_dist_info,
          estimation_window = estimation_window,
          mean_serial_interval = mean_serial_interval,
          std_serial_interval = std_serial_interval,
          ref_date = min(cnslt_data_filtered$date),
          time_step = "week",
          output_Re_only = F) %>%
          mutate(observation_type = region, pathogen = pathogen_x, measuring_period = measuring_period_k)
      },
      error = function(cond) {
        message(paste("Couldn't calculate Re for", wwtp_i, "influenza", influenza_type_j))
        message(cond)
        # Make observation data frame anyways
        return(data.frame(
          date = cnslt_data_filtered$date,
          observed_incidence = cnslt_data_filtered$total_cnslt,
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
          observation_type = region,
          influenza_type = pathogen_x,
          measuring_period = measuring_period_k
        ))
      })
      
      # Aggregate Re estimates
      if (is_first) {
        data_all <- cnslt_data_interpolated
        estimates_bootstrap_all <- estimates_bootstrap
        is_first <- F
      } else {
        data_all <- rbind(data_all, cnslt_data_interpolated)
        estimates_bootstrap_all <- rbind(estimates_bootstrap_all, estimates_bootstrap)
      }
    }
  }
}

# Write out data used for Re inference and results
write.csv(
  x = data_all,
  file = "app/data/sentinella_consultations.csv"
)

write.csv(
  x = estimates_bootstrap_all,
  file = "app/data/confirmed_cnslt_re_estimates.csv",
  row.names = F
)


