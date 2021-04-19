#### Script which will run once daily (also calls in the reading in) ####
library(tidyverse)
library(lubridate)
#library(patchwork) - not on server
library(viridis)
library(EpiEstim)

source("helper_code/reading_in.R")

daily_re_helpers1 = "https://raw.githubusercontent.com/covid-19-Re/shiny-dailyRe/master/app/otherScripts/2_utils_getInfectionIncidence.R"
daily_re_helpers2 = "https://raw.githubusercontent.com/covid-19-Re/shiny-dailyRe/master/app/otherScripts/3_utils_doReEstimates.R"

source(daily_re_helpers1)
source(daily_re_helpers2)

source("helper_code/reading_in.R")
source("helper_code/wastewater_functions.R")


# function for Rww estimation ####

rww_estimation <- function(raw, canton) {
  # normalise
  norm_min <- min(raw_data[[canton]]$n1)
  ww_data <- raw_data[[canton]] %>%
    mutate(norm_n1 = n1/norm_min)
  
  # Deconvolve
  config_df = expand.grid("region" = c(canton),  
                          'incidence_var' = c('norm_n1'),
                          'GammaParams' = list(c('incubation', 'benefield')) )
  
  deconv_ww_data <- data.frame()
  Re_ww <- data.frame()
  
  row_i <- 1 
  
  # - set to run after data changes (TIME CONSUMING)
  new_deconv_data = deconvolveIncidence(ww_data %>% 
                                          filter(region == config_df[row_i, 'region']), 
                                        incidence_var = config_df[row_i, 'incidence_var'],
                                        getCountParams(unlist(config_df[row_i, 'GammaParams'])[1]), 
                                        getCountParams(unlist(config_df[row_i, 'GammaParams'])[2]),
                                        smooth_param = TRUE, n_boot = 100)
  
  new_deconv_data <- new_deconv_data %>%
    mutate(incidence_var = config_df[row_i, 'incidence_var'],
           incubationParams = unlist(config_df[row_i, 'GammaParams'])[1], 
           onsetToCountParams = unlist(config_df[row_i, 'GammaParams'])[2],
           GammaParams = paste0(incubationParams, '_', onsetToCountParams),
           source = GammaParams)
  
  
  # Rww estimates from pipeline ####
  
  new_Re_ww = getReBootstrap(new_deconv_data)
  new_Re_ww <- new_Re_ww %>%
    mutate(variable = config_df[row_i, 'incidence_var'],
           incubationParams = unlist(config_df[row_i, 'GammaParams'])[1], 
           onsetToCountParams = unlist(config_df[row_i, 'GammaParams'])[2],
           GammaParams = paste0(incubationParams, '_', onsetToCountParams),
           region = config_df[row_i, 'region'])
  
  deconv_ww_data <- bind_rows(deconv_ww_data, new_deconv_data)
  Re_ww = bind_rows(Re_ww, new_Re_ww) # currently nothing in Re_ww, as not being calculated
  
  Re_ww <- Re_ww %>%
    mutate(variable = recode(variable,
                             'norm_n1' = 'N1'))
  
  Re_ww_needed <- Re_ww %>% select(region, data_type, date, 
                   median_R_mean, median_R_highHPD, median_R_lowHPD)
  
  Re_ww_needed # returned
  
  
}


Rww_Re_compute <- function(Restimates_canton, raw_data, canton) {
  Restimates <- Restimates_canton %>%
    filter(region %in% c(canton),
           estimate_type == "Cori_slidingWindow",
           date >= as_date("2020-09-01"))
  
  Re_ww_needed <- rww_estimation(raw_data, canton)
  
  plotData <- Restimates %>%
    filter(region %in% c(canton),
           estimate_type == 'Cori_slidingWindow',
           data_type != 'Confirmed cases / tests',
           date >= as_date("2020-09-01") ) %>%
    dplyr::select(-estimate_type, -countryIso3, -country, -source) %>%
    bind_rows(Re_ww_needed) %>% mutate(data_type = factor(data_type)) %>% # the rww binded
    mutate(data_type = recode_factor(data_type, "infection_norm_n1" = "Wastewater"))
  
  plotData
}

plotData <- list()


plotData[["ZH"]] <- Rww_Re_compute(Restimates_canton, raw_data, "ZH")
plotData[["VD"]] <- Rww_Re_compute(Restimates_canton, raw_data, "VD")



# Restimates <- Restimates_canton %>%
#   filter(region %in% c('ZH'),
#          estimate_type == "Cori_slidingWindow",
#          date >= as_date("2020-09-01"))

# Re_ww_needed <- Re_ww %>% select(region, data_type, date, 
#                                  median_R_mean, median_R_highHPD, median_R_lowHPD)

# plotData <- Restimates %>%
#   filter(region %in% c('ZH'),
#          estimate_type == 'Cori_slidingWindow',
#          data_type != 'Confirmed cases / tests',
#          date >= as_date("2020-09-01") ) %>%
#   dplyr::select(-estimate_type, -countryIso3, -country, -source) %>%
#   bind_rows(Re_ww_needed) %>% mutate(data_type = factor(data_type)) %>% # the rww binded
#   mutate(data_type = recode_factor(data_type, "infection_norm_n1" = "Wastewater"))



# Lausanne ####
# # Normalise ####
# 
# norm_min_vd <- min(raw_data_VD$n1)
# 
# ww_data_vd = raw_data_VD  %>%
#   mutate(norm_n1 = n1/norm_min_vd)
# 
# # Deconvolve - Time consuming ####
# config_df_vd = expand.grid("region" = c('VD'),  
#                         'incidence_var' = c('norm_n1'),
#                         'GammaParams' = list(c('incubation', 'benefield')) )
# # the two parameters for the shedding distribution- Gamma. Incubation time and T2.
# 
# deconv_ww_data_vd <- data.frame()
# Re_ww_vd <- data.frame()
# 
# row_i <- 1
# 
# # - set to run after data changes (TIME CONSUMING)
# new_deconv_data_vd = deconvolveIncidence(ww_data_vd %>% 
#                                         filter(region == config_df_vd[row_i, 'region']), 
#                                       incidence_var = config_df_vd[row_i, 'incidence_var'],
#                                       getCountParams(unlist(config_df_vd[row_i, 'GammaParams'])[1]), 
#                                       getCountParams(unlist(config_df_vd[row_i, 'GammaParams'])[2]),
#                                       smooth_param = TRUE, n_boot = 100)
# 
# new_deconv_data_vd <- new_deconv_data_vd %>%
#   mutate(incidence_var = config_df_vd[row_i, 'incidence_var'],
#          incubationParams = unlist(config_df_vd[row_i, 'GammaParams'])[1], 
#          onsetToCountParams = unlist(config_df_vd[row_i, 'GammaParams'])[2],
#          GammaParams = paste0(incubationParams, '_', onsetToCountParams),
#          source = GammaParams)
# 
# 
# # Rww estimates from pipeline ####
# 
# new_Re_ww_vd = getReBootstrap(new_deconv_data_vd)
# new_Re_ww_vd <- new_Re_ww_vd %>%
#   mutate(variable = config_df_vd[row_i, 'incidence_var'],
#          incubationParams = unlist(config_df_vd[row_i, 'GammaParams'])[1], 
#          onsetToCountParams = unlist(config_df_vd[row_i, 'GammaParams'])[2],
#          GammaParams = paste0(incubationParams, '_', onsetToCountParams),
#          region = config_df_vd[row_i, 'region'])
# 
# deconv_ww_data_vd <- bind_rows(deconv_ww_data_vd, new_deconv_data_vd)
# Re_ww_vd = bind_rows(Re_ww_vd, new_Re_ww_vd) # currently nothing in Re_ww, as not being calculated
# 
# Re_ww_vd <- Re_ww_vd %>%
#   mutate(variable = recode(variable,
#                            'norm_n1' = 'N1'))
# 
# 
# 
# # All other Re estimates - Lausanne ####
# 
# Restimates_vd <- Restimates_canton %>%
#   filter(region %in% c('VD'),
#          estimate_type == "Cori_slidingWindow",
#          date >= as_date("2020-09-01"))
# 
# Re_ww_needed_vd <- Re_ww_vd %>% select(region, data_type, date, 
#                                  median_R_mean, median_R_highHPD, median_R_lowHPD)
# 
# plotData_vd <- Restimates_vd %>%
#   filter(region %in% c('VD'),
#          estimate_type == 'Cori_slidingWindow',
#          data_type != 'Confirmed cases / tests',
#          date >= as_date("2020-09-01") ) %>%
#   dplyr::select(-estimate_type, -countryIso3, -country, -source) %>%
#   bind_rows(Re_ww_needed_vd) %>% mutate(data_type = factor(data_type)) %>% # the rww binded
#   mutate(data_type = recode_factor(data_type, "infection_norm_n1" = "Wastewater"))
# 
# 
