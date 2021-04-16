#### Script which will run once daily (also calls in the reading in) ####
# Zurich only first
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


# ZURICH ####
# Normalise ####

norm_min <- min(raw_data_ZH$n1)

ww_data = raw_data_ZH  %>%
  mutate(norm_n1 = n1/norm_min)

# Deconvolve - Time consuming ####
config_df = expand.grid("region" = c('ZH'),  
                        'incidence_var' = c('norm_n1'),
                        'GammaParams' = list(c('incubation', 'benefield')) )
# the two parameters for the shedding distribution- Gamma. Incubation time and T2.

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


# All other Re estimates  ####

Restimates_url = "https://raw.githubusercontent.com/covid-19-Re/dailyRe-Data/master/CHE-estimates.csv"

Restimates_canton <- read_csv(Restimates_url,
                              col_names = c('country', 'region','source','data_type',
                                            'estimate_type','date','median_R_mean',
                                            'median_R_highHPD','median_R_lowHPD',
                                            'countryIso3'),
                              col_types = cols(date = col_date(format = '')),
                              skip = 1) 

Restimates <- Restimates_canton %>%
  filter(region %in% c('ZH'),
         estimate_type == "Cori_slidingWindow",
         date >= as_date("2020-09-01"))


date_ranges <- Re_ww %>%
  group_by(region) %>%
  summarise(min_date = min(date),
            max_date = max(date))

Re_ww_needed <- Re_ww %>% select(region, data_type, date, 
                                 median_R_mean, median_R_highHPD, median_R_lowHPD)

plotData <- Restimates %>%
  filter(region %in% c('ZH'),
         estimate_type == 'Cori_slidingWindow',
         data_type != 'Confirmed cases / tests',
         date >= date_ranges[date_ranges$region == 'ZH', ]$min_date,
         date <= date_ranges[date_ranges$region == 'ZH',]$max_date ) %>%
  dplyr::select(-estimate_type, -countryIso3, -country, -source) %>%
  bind_rows(Re_ww_needed) %>% mutate(data_type = factor(data_type)) %>% # the rww binded
  mutate(data_type = recode_factor(data_type, "infection_norm_n1" = "Wastewater"))


