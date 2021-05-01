# Rww estimation ####
# to be run with cronjobs - does the heavylifting then writes Rww for all regions to csv
# runs for all specified cantons
library(EpiEstim)

setwd("/home/tsinghal/ShinyApps/wastewater_re_shiny")

daily_re_helpers1 = "https://raw.githubusercontent.com/covid-19-Re/shiny-dailyRe/master/app/otherScripts/2_utils_getInfectionIncidence.R"
daily_re_helpers2 = "https://raw.githubusercontent.com/covid-19-Re/shiny-dailyRe/master/app/otherScripts/3_utils_doReEstimates.R"

source(daily_re_helpers1)
source(daily_re_helpers2)

source("helper_code/reading_in.R")
source("helper_code/wastewater_functions.R")

# Rww estimation for all regions - bootstrap, deconvolve, compute Re ####

config_df = expand.grid("region" = c('ZH', 'VD', 'SG', 'GR', 'FR', 'TI'),   # add regions as they come
                        'incidence_var' = c('norm_n1'),
                        'GammaParams' = list(c('incubation', 'benefield')) )


deconv_ww_data <- data.frame()
Re_ww <- data.frame()

for(row_i in 1:nrow(config_df)){
  new_deconv_data = deconvolveIncidence(ww_data %>% 
                                          filter(region == config_df[row_i, 'region']), 
                                        incidence_var = config_df[row_i, 'incidence_var'],
                                        getCountParams(unlist(config_df[row_i, 'GammaParams'])[1]), 
                                        getCountParams(unlist(config_df[row_i, 'GammaParams'])[2]),
                                        smooth_param = TRUE, n_boot = 50)
  
  new_deconv_data <- new_deconv_data %>%
    mutate(incidence_var = config_df[row_i, 'incidence_var'],
           incubationParams = unlist(config_df[row_i, 'GammaParams'])[1], 
           onsetToCountParams = unlist(config_df[row_i, 'GammaParams'])[2],
           GammaParams = paste0(incubationParams, '_', onsetToCountParams),
           source = GammaParams)
  
  ##### Get Re #####
  new_Re_ww = getReBootstrap(new_deconv_data)
  new_Re_ww <- new_Re_ww %>%
    mutate(variable = config_df[row_i, 'incidence_var'],
           incubationParams = unlist(config_df[row_i, 'GammaParams'])[1], 
           onsetToCountParams = unlist(config_df[row_i, 'GammaParams'])[2],
           GammaParams = paste0(incubationParams, '_', onsetToCountParams),
           region = config_df[row_i, 'region'])
  
  deconv_ww_data <- bind_rows(deconv_ww_data, new_deconv_data)
  Re_ww = bind_rows(Re_ww, new_Re_ww)
}


Re_ww_needed <- Re_ww %>% select(region, data_type, date, 
                                 median_R_mean, median_R_highHPD, median_R_lowHPD) # write to csv

write.csv(Re_ww_needed, "rww_data/Rww_cantonal.csv", row.names = F)
 

