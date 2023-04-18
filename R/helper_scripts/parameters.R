# This script stores parameters for different parameterizations of wastewater Re estimation, depending on the pathogen

## SARS-CoV-2  Delay between infection and onset of symptoms (incubation period) in days
# Ref: Linton et al., Journal of Clinical Medicine, 2020
sars_cov_2_distribution_incubation <- list(
    name = "gamma",
    shape = 3.2,
    scale = 1.3)


## SARS-CoV-2 Delay between onset of symptoms and case confirmation in days
# Ref: Bi et al., The Lancet Infectious Diseases, 2020
sars_cov_2_distribution_onset_to_confirmation <- list(
    name = "gamma",
    shape = 2.1,
    scale = 2.6)

## SARS-CoV-2 Delay between onset of symptoms and shedding into wastewater in days
# Ref: Benefield et al., medRxiv, 2020
sars_cov_2_distribution_onset_to_shedding <- list(
    name = "gamma",
    shape = 0.929639,
    scale = 7.241397)

## SARS-CoV-2 Serial interval (for Re estimation) in days
# Ref: Nishiura et al.,International Journal of Infectious Diseases, 2020
sars_cov_2_mean_serial_interval_days <- 4.8
sars_cov_2_std_serial_interval_days <- 2.3

## Influenza Delay between infection and shedding into wastewater in days
# Ref: Carrat et al., American Journal of Epidemiology, 2008
influenza_distribution_infection_to_shedding_carrat2008_mle <- list(
    name = "gamma",
    shape = 8.63,
    scale = 0.29)

influenza_distribution_infection_to_shedding_carrat2008_moments <- list(
    name = "gamma",
    shape = 6.153341704,
    scale = 0.404855929)

## Influenza Delay between infection and shedding into wastewater in days
# Ref: get_shedding_load_distribution_fecal.R
influenza_distribution_infection_to_shedding_fecal_moments <- list(
    name = "gamma",
    shape = 2.541929,
    scale = 4.785657)

## Influenza Delay between infection and symptom score
# Ref: get_symptom_score_distribution.R
influenza_distribution_infection_to_symptoms_moments <- list(
  name = "gamma",
  shape = 4.471365702,
  scale = 0.822408011)

influenza_mean_serial_interval_days <- 2.6  # EpiEstim example (from Ferguson et al. 2005, Nature)
influenza_std_serial_interval_days <- 1.5  # EpiEstim example (from Ferguson et al. 2005, Nature)


#########
# RSV Parameters

#RSV infection to shedding distribution taken from mean of posterior values in https://doi.org/10.1093/aje/kwab181 fig 2a
rsv_distribution_infection_to_shedding <- list(
  name = "gamma",
  shape = 8.8,
  scale = 0.769)


# RSV Serial interval taken from Vink et al. https://doi.org/10.1093/aje/kwu209 
# data from Crowcroft et al.https://doi.org/10.1007/s00431-007-0509-9 
rsv_mean_serial_interval_days <- 7.5 
rsv_std_serial_interval_days <- 2.1  
