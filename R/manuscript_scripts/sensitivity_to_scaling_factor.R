# This script is to investigate how sensitive wastewater Re estimation is to the scaling factor applied to bring wastewater measurements on the same scale as case data

library(dplyr)
library(estimateR)
library(ggplot2)
library(tidyr)

source("code/R/helper_scripts/functions.R")

shedding_dist <- read.csv("data/shedding_profile_fit_fecal.csv")  # TODO: CHECK THIS BEFORE RUNNING!
shedding_dist_to_use <- shedding_dist %>% filter(method == "Moments")
shedding_dist_info <- list(name = "gamma", shape = shedding_dist_to_use$shape, scale = shedding_dist_to_use$scale)  # Shedding period (infection to viral shedding)

# Load influenza wastewater and case data
ww_data_zh <- read.csv("tmp/clean_data_ge_zh.csv") %>%
  filter(wwtp == "ARA Werdhölzli") %>%
  pivot_wider(
    id_cols = c("sample_date", "wwtp", "n_measurements"), 
    names_from = "measurement_type", 
    values_from = "mean") %>%
    filter(sample_date >= as.Date("2021-12-01"), sample_date < as.Date("2022-05-01"))

case_data_zh <- read.csv("tmp/clean_data_cases_che.csv") %>%
    filter(wwtp == "ZUERICH(WERDHOELZLI)") %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= as.Date("2021-12-01"), date < as.Date("2022-05-01")) %>%
    filter(influenza_type == "A")

ww_data_interp_zh <- interpolate_measurements(
  data_frame = ww_data_zh %>% mutate(sample_date = as.Date(sample_date)),
  date_col = "sample_date",
  measurement_cols = c("IAV_gc_per_day", "IBV_gc_per_day")
)

# Define scaling to try
min_incidence <- min(case_data_zh$total_scaled_cases)  # 0, won't base anything on case data

normalization_factor_zh <- min(
  ww_data_zh$IAV_gc_per_day[ww_data_zh$IAV_gc_per_day > 0],
  ww_data_zh$IBV_gc_per_day[ww_data_zh$IBV_gc_per_day > 0]
)

normalization_factors <- list(
    # "No scaling" = 1,
    "Minimum measurement is 1 case" = normalization_factor_zh,  # scaling used for main analysis
    "Minimum measurement is 10 cases" = normalization_factor_zh / 10,
    "Minimum measurement is 100 cases" = normalization_factor_zh / 100
    # "Minimum measurement is 1000 cases" = normalization_factor_zh / 1000
)

# Set methodological parameters for Re estimation
estimation_window = 3 # 3-day sliding window for the Re estimation
N_bootstrap_replicates = 50 # we take this many replicates in the bootstrapping procedure

# Do Re estimation
is_first <- T
for (i in 1:length(normalization_factors)) {
    norm_factor <- normalization_factors[[i]]
    ww_data_scaled <- ww_data_interp_zh %>%
        mutate(obs_scaled = IAV_gc_per_day / norm_factor)

    ref_date = min(ww_data_scaled$date, na.rm = T)

    ww_estimates <- get_block_bootstrapped_estimate(
        incidence_data = ww_data_scaled$obs_scaled,
        N_bootstrap_replicates = N_bootstrap_replicates,
        delay = list(shedding_dist_info),
        estimation_window = estimation_window,
        mean_serial_interval = 2.6,
        std_serial_interval = 1.5,
        ref_date = ref_date,
        time_step = "day",
        combine_bootstrap_and_estimation_uncertainties = TRUE,
        output_Re_only = F
    ) %>% mutate(
        observation_type = "Zurich",
        influenza_type = "IAV",
        data_type = "Wastewater",
        max_incidence = round(max(ww_data_scaled$obs_scaled), digits = 0),
        max_est_incidence = round(max(deconvolved_incidence, na.rm = T), digits = 0),
        scaling_type = names(normalization_factors)[i]
    )

    if (is_first) {
        ww_estimates_all <- ww_estimates
        is_first <- F
    } else {
        ww_estimates_all <- bind_rows(ww_estimates_all, ww_estimates)
    }

    ww_data_scaled <- ww_data_interp_zh %>%
        mutate(obs_scaled = IBV_gc_per_day / norm_factor)

    ww_estimates <- get_block_bootstrapped_estimate(
        incidence_data = ww_data_scaled$obs_scaled[18:length(ww_data_scaled$obs_scaled)],
        N_bootstrap_replicates = N_bootstrap_replicates,
        delay = list(shedding_dist_info),
        estimation_window = estimation_window,
        mean_serial_interval = 2.6,
        std_serial_interval = 1.5,
        ref_date = ref_date + 17,
        time_step = "day",
        combine_bootstrap_and_estimation_uncertainties = TRUE,
        output_Re_only = F
    ) %>% mutate(
        observation_type = "Zurich",
        influenza_type = "IBV",
        data_type = "Wastewater",
        max_incidence = round(max(ww_data_scaled$obs_scaled), digits = 0),
        max_est_incidence = round(max(deconvolved_incidence, na.rm = T), digits = 0),
        scaling_type = names(normalization_factors)[i]
    )
    ww_estimates_all <- bind_rows(ww_estimates_all, ww_estimates)
}

# Write out results
write.csv(ww_estimates_all, "results/ww_re_scaling_sensitivity.csv", row.names = F)

# # Plot results
# ggplot(
#     data = ww_estimates_all,
#     aes(x = date, y = Re_estimate)
# ) + facet_grid(scaling_type ~ influenza_type, labeller = label_both) +
#     geom_line() +
#     geom_ribbon(aes(ymax = CI_up_Re_estimate, ymin = CI_down_Re_estimate), alpha = 0.45, colour = NA) +
#     scale_x_date(date_breaks = "1 month",
#                 date_labels = '%b\n%y') +
#     labs(y = "Reproductive number", x = element_blank()) +
#     scale_y_continuous(limits = c(0, 3), oob = oob_squish, breaks = c(0, 1, 2, 3)) +
#     theme_bw()

# ggsave("figures/sensitivity_analysis_scaling.png", width = 7, height = 7, units = "in")
