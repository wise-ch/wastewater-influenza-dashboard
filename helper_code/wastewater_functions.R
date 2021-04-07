###########################################################
# Function to estimate Re from WW
# Author: JS Huisman
###########################################################

###########################################################
# Delay Distributions #####

# find gamma parameters from mean/sd of distribution
getGammaParams <- function(meanParam, sdParam){
  shapeParam <- meanParam^2 / (sdParam^2)
  scaleParam <- (sdParam^2) / meanParam
  return(list(shape = shapeParam, scale = scaleParam))
}

getCountParams <- function(obs_type){
  switch(obs_type,
         incubation = getGammaParams(5.3, 3.2),
         zero = list(shape = 0, scale = 0),
         death = getGammaParams(15.0, 6.9),
         death_zh = getGammaParams(14.2, 9.73),
         hospitalisation = getGammaParams(5.1, 4.2),
         hospitalised_zh = getGammaParams(6.51, 5.48),
         confirmed = getGammaParams(5.5, 3.8),
         confirmed_zh = getGammaParams(2.83, 2.96),
         confirmed_cali = getGammaParams(5.067, 5.64),
         han = getGammaParams(4.7, 1.7),
         benefield = list(shape = 0.929639, scale = 7.241397))
}

getInvGammaParams <- function(shapeParam, scaleParam){
  meanParam <- scaleParam * shapeParam
  sdParam <- sqrt(scaleParam^2 * shapeParam)
  return(list(mean = meanParam, sd = sdParam))
}

###########################################################
# Deconvolve #####

addUselessColumns <- function(df, inc_var = 'n1'){
  
  if (!'region' %in% colnames(df)){
    df <- df %>%
      mutate(region = 'CHE')
  }
  
  observation_df <- df %>%
    dplyr::select(date, region, value = inc_var) %>%
    mutate(data_type = inc_var,
           source = 'ETH',
           variable = 'incidence',
           country = 'Switzerland',
           date_type = 'report',
           local_infection = TRUE)
  
  observation_df <- observation_df %>%
    filter(!is.na(value))
  
  return(observation_df)
}


deconvolveIncidence <- function(df, incidence_var = 'n1',
                                IncubationParams, OnsetToCountParams,
                                smooth_param = FALSE, n_boot = 50){
  infection_df <- addUselessColumns(df, inc_var = incidence_var)
  
  constant_delay_distributions <- list("Simulated" = get_vector_constant_waiting_time_distr(
    IncubationParams$shape, IncubationParams$scale,
    OnsetToCountParams$shape, OnsetToCountParams$scale),
    "Symptoms" = get_vector_constant_waiting_time_distr(
      IncubationParams$shape, IncubationParams$scale,
      0, 0))
  
  estimatedInfections <- get_infection_incidence_by_deconvolution(
    infection_df,
    is_local_cases = T,
    constant_delay_distribution = constant_delay_distributions[['Simulated']],
    constant_delay_distribution_incubation = constant_delay_distributions[["Symptoms"]],
    max_iterations = 100,
    smooth_incidence = smooth_param,
    empirical_delays = tibble(),
    n_bootstrap = n_boot,
    verbose = FALSE)
  
  return(estimatedInfections)
}

###########################################################
getReBootstrap <- function(deconvoluted_data){
  
  all_delays <- lapply(unique(deconvoluted_data$data_type), function(x){ c(Cori = 0)})
  names(all_delays) <- unique(deconvoluted_data$data_type)
  
  truncations <- list(left = c(Cori = 5),
                      right = c(Cori = 0))
  
  rawReEstimates <- suppressWarnings(doAllReEstimations(
    deconvoluted_data,
    slidingWindow = 3,
    methods = c("Cori"),
    variationTypes = c("slidingWindow"),
    all_delays,
    truncations,
    interval_ends = list()) )
  
  cleanEstimates <- cleanCountryReEstimate(rawReEstimates, method = 'bootstrap',
                                           rename_types = F, alpha=0.95)
  
  return(cleanEstimates)
}
