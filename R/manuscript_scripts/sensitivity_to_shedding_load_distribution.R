# This script is to investigate how sensitive influenza wastewater Re estimation is to the assumed shedding load distribution

library(dplyr)
library(estimateR)
library(ggplot2)

source("code/R/helper_scripts/parameters.R")

# Load interpolated influenza measurement data
influenza_data <- read.csv("results/ww_loads.csv") %>%
    filter(observation_type == "ARA Werdhölzli")

# Define different shedding load distributions to test
distributions <- list(
    "Carrat 2008 moments" = influenza_distribution_infection_to_shedding_carrat2008_moments,  # assumed for main results
    "Carrat 2008 MLE" = influenza_distribution_infection_to_shedding_carrat2008_mle,
    "Fecal data" = influenza_distribution_infection_to_shedding_fecal_moments
)

# Set methodological parameters for Re estimation
estimation_window = 3 # 3-day sliding window for the Re estimation
minimum_cumul_incidence = 100 # we start estimating Re after at least 100 cases have been recorded
N_bootstrap_replicates = 50 # we take this many replicates in the bootstrapping procedure

# Do Re estimation
is_first <- T
for (i in 1:length(distributions)) {
    distribution <- distributions[[i]]

    ref_date = as.Date(min(influenza_data$sample_date))

    WW_estimates <- get_block_bootstrapped_estimate(
        incidence_data = influenza_data$`IAV_.gc.day._normalized`,
        N_bootstrap_replicates = N_bootstrap_replicates,
        delay = distribution,
        estimation_window = estimation_window,
        minimum_cumul_incidence = minimum_cumul_incidence,
        mean_serial_interval = influenza_mean_serial_interval,
        std_serial_interval = influenza_std_serial_interval,
        ref_date = ref_date,
        time_step = "day",
        combine_bootstrap_and_estimation_uncertainties = TRUE
    ) %>% mutate(
        pathogen = "IAV",
        delay_type = names(distributions)[i]
    )

    if (is_first) {
        WW_estimates_all <- WW_estimates
        is_first <- F
    } else {
        WW_estimates_all <- bind_rows(WW_estimates_all, WW_estimates)
    }
}

# Plot results
ggplot(
    data = WW_estimates_all,
    aes(x = date, y = Re_estimate, color = delay_type, fill = delay_type)
) + 
    # facet_wrap(delay_type ~ ., labeller = label_both) +
    geom_line() +
    geom_ribbon(aes(ymax = CI_up_Re_estimate, ymin = CI_down_Re_estimate), alpha = 0.45, colour = NA) +
    scale_x_date(date_breaks = "1 month",
                date_labels = '%b\n%y') +
    labs(y = "Reproductive number", x = element_blank()) +
    coord_cartesian(ylim = c(0.5, 2)) +
    geom_vline(aes(xintercept = as.Date("2021-12-06"), linetype = "Work from home & 3G certificates implemented")) +
    geom_vline(aes(xintercept = as.Date("2021-12-20"), linetype = "2G certificates implemented")) +
    geom_vline(aes(xintercept = as.Date("2022-02-03"), linetype = "Work from home lifted")) +
    geom_vline(aes(xintercept = as.Date("2022-02-17"), linetype = "2G certificates lifted")) +
    theme_bw()

ggsave("figures/influenza_sld_sensitivity.png", width = 7, height = 3, units = "in")
