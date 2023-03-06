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
output_suffix <- "" # Dashboard uses data with default output name, i.e. output_suffix = ""
delay_dist_info <- influenza_distribution_infection_to_shedding_carrat2008_moments
mean_serial_interval <- influenza_mean_serial_interval_days
std_serial_interval <- influenza_std_serial_interval_days
estimation_window <- 3  # 3 is EpiEstim default
minimum_cumul_incidence <- 12  # minimum cumulative number of infections for Re to be estimated, EstimateR default is 12
seasons_to_calculate <- c("2022/23")  # list of seasons to calculate estimates for (2021/22 is cached)
n_bootstrap_reps <- 50

# Import data
ww_data_bs <- read_csv("data/clean_data_bs.csv", col_types = cols(sample_date = "D")) %>%
  filter(measuring_period %in% seasons_to_calculate) %>%
  pivot_wider(
    id_cols = c("sample_date", "measuring_period", "wwtp"),
    names_from = "measurement_type",
    values_from = "mean")
ww_data_gr <- read_csv("data/clean_data_eawag.csv", col_types = cols(sample_date = "D")) %>%
  filter(measuring_period %in% seasons_to_calculate) %>%
  filter(wwtp == "ARA Chur") %>%
  pivot_wider(
    id_cols = c("sample_date", "measuring_period", "wwtp"),
    names_from = "measurement_type",
    values_from = "mean")
ww_data_sg <- read_csv("data/clean_data_eawag.csv", col_types = cols(sample_date = "D")) %>%
  filter(measuring_period %in% seasons_to_calculate) %>%
  filter(wwtp == "ARA Altenrhein") %>%
  pivot_wider(
    id_cols = c("sample_date", "measuring_period", "wwtp"),
    names_from = "measurement_type",
    values_from = "mean")
ww_data_fr <- read_csv("data/clean_data_eawag.csv", col_types = cols(sample_date = "D")) %>%
  filter(measuring_period %in% seasons_to_calculate) %>%
  filter(wwtp == "ARA Sensetal") %>%
  pivot_wider(
    id_cols = c("sample_date", "measuring_period", "wwtp"),
    names_from = "measurement_type",
    values_from = "mean")
ww_data_ti <- read_csv("data/clean_data_eawag.csv", col_types = cols(sample_date = "D")) %>%
  filter(measuring_period %in% seasons_to_calculate) %>%
  filter(wwtp == "CDA Lugano") %>%
  pivot_wider(
    id_cols = c("sample_date", "measuring_period", "wwtp"),
    names_from = "measurement_type",
    values_from = "mean")
ww_data_ge <- read_csv("data/clean_data_eawag.csv", col_types = cols(sample_date = "D")) %>%
  filter(measuring_period %in% seasons_to_calculate) %>%
  filter(wwtp == "STEP Aire") %>%
  pivot_wider(
    id_cols = c("sample_date", "measuring_period", "wwtp"),
    names_from = "measurement_type",
    values_from = "mean")
ww_data_zh <- read_csv("data/clean_data_eawag.csv", col_types = cols(sample_date = "D")) %>%
  filter(measuring_period %in% seasons_to_calculate) %>%
  filter(wwtp == "ARA Werdhölzli") %>%
  pivot_wider(
    id_cols = c("sample_date", "measuring_period", "wwtp"),
    names_from = "measurement_type",
    values_from = "mean")

wwtp_data_all <- list(
  "Basel" = ww_data_bs,
  "Chur" = ww_data_gr,
  "Altenrhein" = ww_data_sg,
  "Laupen" = ww_data_fr,
  "Lugano" = ww_data_ti,
  "Geneva" = ww_data_ge,
  "Zurich" = ww_data_zh
)

ww_data_all_combined <- bind_rows(wwtp_data_all) %>%
  group_by(sample_date, measuring_period) %>% 
  summarize(across(-wwtp,mean,na.rm=T), .groups = "drop") %>% 
  mutate(wwtp = "All provided data", .after = measuring_period)

wwtp_data_all <- c(wwtp_data_all, list("All provided data" = ww_data_all_combined))

# Estimate Re for each data stream
is_first <- T
for (wwtp_name in names(wwtp_data_all)) {

  df <- wwtp_data_all[[wwtp_name]]
  wwtp_i <- unique(df$wwtp)  # should be single plant

  # Normalize daily copies by minimum value to get observations on the same scale as case data
  # See Huisman et al. 2022 Environmental Health Perspectives -
  # deconvolution algorithm seems to struggle when observations are too peaked
  data_normalized <- tryCatch(
    {
      suppressWarnings(normalization_factor <- min(
        df$IAV_gc_per_day[df$IAV_gc_per_day > 0],
        df$IBV_gc_per_day[df$IBV_gc_per_day > 0],
        na.rm = T))
      suppressWarnings(df %>%
        mutate(IAV_gc_per_day_norm = IAV_gc_per_day / normalization_factor) %>%
        mutate(IBV_gc_per_day_norm = IBV_gc_per_day / normalization_factor))
    },
    error = function(cond) {
      message(paste("Couldn't calculate Re for", wwtp_name,
                    "because normalization failed (probably due to no measurements this season.)"))
      return(cond)
    }
  )

  if(inherits(data_normalized, "error")) {
    next  # if normalization fails, skip this wwtp
  }

  data_long <- data_normalized %>% pivot_longer(
    cols = c(IAV_gc_per_day, IBV_gc_per_day, IAV_gc_per_day_norm, IBV_gc_per_day_norm),
    values_to = "observation",
    names_to = c("influenza_type", "observation_units"),
    names_pattern = "([A-Z]{3})_(.*)"
  )

  # Iterate over influenza types and seasons (if multiple)
  for (influenza_type_j in unique(data_long$influenza_type)) {
    for (measuring_period_k in unique(data_long$measuring_period)) {
      writeLines(paste("\nEstimating Re for", wwtp_i, "influenza", influenza_type_j,
                       "in period", measuring_period_k))

      # Get appropriate data
      data_filtered <- data_long %>%
        filter(
          influenza_type == influenza_type_j,
          measuring_period == measuring_period_k,
          observation_units == "gc_per_day_norm") %>%
        filter(!is.na(observation)) %>%  # this is just to remove leading NA measurements for ZH IBV
        arrange(sample_date)

      # Try to estimate Re (handling case where not enough incidence observed to calculate)
      estimates_bootstrap <- tryCatch({

        # Interpolate measurements to daily values
        data_interpolated <- interpolate_measurements(
          data_frame = data_filtered,
          date_col = "sample_date",
          measurement_cols = c("observation")
        )

        measurements <- list(
          values = data_interpolated$observation,
          index_offset = 0)

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
          ref_date = min(data_filtered$sample_date),
          time_step = "day",
          output_Re_only = F) %>%
          mutate(observation_type = wwtp_i, influenza_type = influenza_type_j, measuring_period = measuring_period_k)
      },
      error = function(cond) {
        message(paste("Couldn't calculate Re for", wwtp_i, "influenza", influenza_type_j))
        message(cond)
        # Make observation data frame anyways
        if (nrow(data_filtered) == 0) {
          warning("No measurements found")
          dates <- NA
          observations <- NA
        } else {
          dates <- data_filtered$sample_date
          observations <- data_filtered$observation
        }
        return(data.frame(
          date = dates,
          observed_incidence = observations,
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
  file = paste0("app/data/ww_loads", output_suffix, ".csv")
)

write.csv(
  x = estimates_bootstrap_all,
  file = paste0("app/data/ww_re_estimates", output_suffix, ".csv")
)
