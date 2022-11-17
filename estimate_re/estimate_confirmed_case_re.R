# This script is to estimate Re from case data on influenza infections.
# It's based on JH's script https://github.com/JSHuisman/wastewaterRe/blob/main/code/wastewater_Zurich.R.

# Load dependencies
library(dplyr)
library(tidyr)
library(readr)
library(estimateR)

source("estimate_re/helper_scripts/functions.R")
source("estimate_re/helper_scripts/parameters.R")

# Set variables
delay_dist_info <- influenza_distribution_infection_to_symptoms_moments
mean_serial_interval <- influenza_mean_serial_interval_days / 7
std_serial_interval <- influenza_std_serial_interval_days / 7
estimation_window <- 3  # 3 is EpiEstim default
n_bootstrap_reps <- 100  # TODO: increase?

# Import data
case_data <- read_csv("data/clean_data_cases_che.csv", col_types = cols(date = "D"))

# Estimate Re for each data stream
is_first <- T
for (wwtp_i in unique(case_data$wwtp)) {
    for (influenza_type_j in unique(case_data$influenza_type)) {
        writeLines(paste("\nEstimating Re for", wwtp_i, "influenza", influenza_type_j))
        
        # Get appropriate data
        case_data_filtered <- case_data %>%
            filter(wwtp == wwtp_i, influenza_type == influenza_type_j) %>%
            arrange(date)

        measurements = list(
            values = case_data_filtered$total_scaled_cases,
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
                ref_date = min(case_data_filtered$date),
                time_step = "week",
                output_Re_only = F) %>% 
                mutate(observation_type = wwtp_i, influenza_type = influenza_type_j)
        },
        error = function(cond) {
            message(paste("Couldn't calculate Re for", wwtp_i, "influenza", influenza_type_j))
            message(cond)
            # Make observation data frame anyways
            return(data.frame(
                date = case_data_filtered$date,
                observed_incidence = case_data_filtered$total_scaled_cases,
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

# Write out estimates
write.csv(
  x = estimates_bootstrap_all,
  file = "app/data/confirmed_case_re_estimates.csv",
  row.names = F
)
