###########################################################
# Analysis of Swiss wastewater data
# J.S. Huisman
###########################################################
library(tidyverse)
library(lubridate)
library(patchwork)
library(viridis)
library(EpiEstim)
# the code further requires zoo for data imputation

#app_location = '/Users/jana/Documents/PhD/covid/covid-19-re-shiny-app'
# my app_location (as from github repository - shiny-dailyRe)
app_location = '/Users/taru/Documents/shiny-dailyRe'



source(paste0(app_location,'/app/otherScripts/2_utils_getInfectionIncidence.R'))
source(paste0(app_location,'/app/otherScripts/3_utils_doReEstimates.R'))


helper_location = '/Users/taru/wastewater_re_shiny/helper_code'

source(paste0(helper_location, '/wastewater_functions.R'))


# theme_set(theme_minimal() +
#             theme(
#               strip.text = element_text(size=20),
#               axis.text= element_text(size=17),
#               axis.title =  element_text(size=20),
#               legend.text= element_text(size=17),
#               legend.title= element_text(size=20)
#             ))


### ZURICH ####
ZH_flow_url = "http://parsivel-eawag.ch/sarscov2/__data__/ARA%20Werdhoelzli_flow_cases.csv"
ZH_genes_url = "http://parsivel-eawag.ch/sarscov2/__data__/ARA%20Werdhoelzli_genes.csv"

raw_flow_data_ZH <- read_delim(ZH_flow_url, delim = ';',
                               col_names = c('date', 'cases', 'cases_smooth', 
                                             'flow', 'n1_smooth', 'n2_smooth'),
                               col_types = cols(date = col_date(format = '')),
                               skip = 1) 

raw_gene_data_ZH <- read_delim(ZH_genes_url, delim = ';',
                               col_names = c('date', 'n1', 'n2'),
                               col_types = cols(date = col_date(format = '')),
                               skip = 1) 

##TARU
## These are choices for the paper; but you'll rather want to start from 20.1.2021 onwards
## Linear interpolation (with zoo) is necessary because we don't expect 0 inbetween
# and otherwise our smoothing step makes the average too low

# We filter after Sept 1 because the data prior is often below LOQ
# We select data up to January 20th, because the sampling changed after
# Missing data is imputed with linear interpolation
raw_data_ZH <- raw_flow_data_ZH %>%
  left_join(raw_gene_data_ZH, c('date')) %>%
  filter(!is.na(n1),
         date >= as_date("2021-01-20")) %>%
  mutate(orig_data = TRUE) %>%
  complete(date = seq.Date(min(date), max(date), by = 'days')) %>%
  mutate(across(where(is.numeric), ~ zoo::na.approx(.x, na.rm = F) )) %>%
  mutate(region = 'ZH')

# drop n2 and n2_smooth as it is no longer recorded from February 2021
raw_data_ZH <- raw_data_ZH %>%
  select(-n2, -n2_smooth)

# Plot `raw' WW data #####

ggplot(raw_data_ZH, aes(x=date, y = n1)) +
  geom_point(colour = 'blue') +
  geom_line(colour = 'black', linetype = 'dashed') +
  geom_smooth(method = 'loess', colour = 'black',
              method.args = list(span = 0.05, degree = 1) ) +
  labs(x = 'Date' , y='N1 load')


##### Cantonal Level: Clinical Case Data #####
# orig_cases_canton <- readRDS(paste0(app_location, '/app/data/countryData/CHE-Data.rds'))
# 
# orig_cases <- orig_cases_canton %>%
#   filter(data_type %in% c('Confirmed cases', "Hospitalized patients"),
#          region == 'ZH',
#          date >= as_date("2020-08-15"),
#          date <= as_date("2021-01-20"),
#          date_type == 'report_plotting') %>%
#   select(date, data_type, value) %>%
#   pivot_wider(names_from = 'data_type', values_from = 'value') %>%
#   rename(confirmed = `Confirmed cases`, hospitalised = `Hospitalized patients`)
# 
# plot_orig_cases_canton <- orig_cases_canton %>%
#   filter(region == c('ZH'),
#          is.na(local_infection),
#          date >= as_date("2020-08-15"),
#          date <= as_date("2021-01-20"),
#           date_type == 'report_plotting',
#          data_type %in% c("Confirmed cases", "Hospitalized patients")) %>%
#          #data_type == "Hospitalized patients") %>%
#   select(date, region, value, name = data_type)
# 
# plot_orig_cases <- plot_orig_cases_canton
# 
# ## Deconvolved cases
# deconv_cases_canton <- readRDS(paste0(app_location, '/app/data/countryData/CHE-DeconvolutedData.rds'))
# 
# plot_deconv_cases <- deconv_cases_canton %>%
#   filter(region %in% c('ZH'),
#          date >= as_date("2020-08-15"),
#          date <= as_date("2021-01-20")) %>%
#   select(-local_infection, -country, -source) %>%
#   group_by(date, region, data_type) %>%
#   summarise(sd = sd(value),
#             value = mean(value),
#             .groups = 'drop') %>%
#   mutate(data_type = recode_factor(data_type,
#                                  "infection_Confirmed cases" = "Confirmed cases",
#                                  "infection_Hospitalized patients"= "Hospitalized patients",
#                                  "infection_Deaths" = "Deaths") ) %>%
#   filter(data_type %in% c("Confirmed cases", "Hospitalized patients"))

## Re estimates
#Restimates_canton <- readRDS(paste0(app_location, '/app/data/countryData/CHE-Estimates.rds'))

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
         date >= as_date("2021-01-20"))


# Plot Case incidence data #####

# plot_orig_cases <- orig_cases %>% 
#   pivot_longer(cols = c(-date)) %>%
#   filter(name != 'death')
#filter(name == 'hospitalised')

# case_data_plot <- ggplot(plot_orig_cases) +
#   geom_bar(aes(x=date, y= value, fill = name), alpha = 0.5,
#            position = 'identity', stat = 'identity', show.legend = F) +
#   labs(x = 'Date' , y='Cases per day') +
#   scale_x_date(limits = c(as_date('2020-08-15'), as_date('2021-01-20')) ) +
#   scale_fill_manual(values = c(viridis(4)[1:2])) + 
#   labs(colour = 'Variable') 

# case_data_plot
#ggsave(plot = case_data_plot, paste0(plot_dir, '/', 'Case_data_raw_canton.pdf'), height = 11, width = 9)


## Normalisation ####

# At LOQ we see 1 case # Min observed - shedding quantity for one infected individual
# only n1. See if this can be fixed plant-wise
# value to change if possible
norm_min <- min(raw_data_ZH$n1)

# LOQ: limit of quantification?
### min. LOQ in gene copies per day ###
# 1000 * flow [m^3/day] = total liters/day
# 25 * (80/5) = LOQ in gene copies per 50 mL
#norm_LOQ = min(raw_data_ZH$flow) * 1000 * 20 * 25 * (80/5)

# normalisation: take the minimum n1 of a specific wastewater plant, 
# then n1/norm_min = norm_n1 data


### ALL Normalised WW DATA ####
ww_data = raw_data_ZH  %>%
  mutate(norm_n1 = n1/norm_min)

# Plot normalised data #####
plot_raw_ww_data <- ww_data %>%
  dplyr::select(date, orig_data, region, N1 = n1) %>% # this was simply n1 in original code.
  # if plotting normalised data, should be norm_n1?
  pivot_longer(cols = c(N1)) %>%
  mutate(name_orig = ifelse(!is.na(orig_data), name, 'Imputed'))

ww_data_plot <- ggplot() +
  geom_point(data = plot_raw_ww_data, aes(x=date, y= value, colour = name_orig),
             size = 2, show.legend = F) +
  geom_line(data = plot_raw_ww_data %>% filter(orig_data), 
            aes(x=date, y= value,colour = name), linetype = 'dotted', show.legend = F) +
  labs(x = 'Date' , y='Gene copies per day') +
  scale_x_date(limits = c(as_date(min(plot_raw_ww_data[["date"]])), as_date(max(plot_raw_ww_data[["date"]])))) +
  scale_colour_manual(values = c(viridis(4)[3:4], 'lightgrey'), 
                      labels = c('N1', 'Imputed'),
                      breaks = c('N1', 'Imputed'),
                      name = 'Variable') + 
  labs(colour = 'Variable')  # 5 in green for imputed, only N1

ww_data_plot # gene copies - NOT NORMALISED!

#ggsave(plot = ww_data_plot, paste0(plot_dir, '/', 'Wastewater_data_raw_CH.pdf'), height = 11, width = 9)

# normalised data plotted:
plot_raw_ww_data_normalised <- ww_data %>%
  dplyr::select(date, orig_data, region, Norm_N1 = norm_n1) %>%
  pivot_longer(cols = c(Norm_N1)) %>%
  mutate(name_orig = ifelse(!is.na(orig_data), name, 'Imputed'))

ww_data_plot_normalised <- ggplot() +
  geom_point(data = plot_raw_ww_data_normalised, aes(x=date, y= value, colour = name_orig),
             size = 2, show.legend = F) +
  geom_line(data = plot_raw_ww_data_normalised %>% filter(orig_data), 
            aes(x=date, y= value,colour = name), linetype = 'dotted', show.legend = F) +
  labs(x = 'Date' , y='Normalised gene copies per day') + # is that what the normalised data is?
  scale_x_date(limits = c(as_date(min(plot_raw_ww_data[["date"]])), as_date(max(plot_raw_ww_data[["date"]])))) +
  scale_colour_manual(values = c(viridis(4)[3:4], 'lightgrey'), 
                      labels = c('Norm_N1', 'Imputed'),
                      breaks = c('Norm_N1', 'Imputed'),
                      name = 'Variable') + 
  labs(colour = 'Variable')  # 5 in green for imputed, only N1

ww_data_plot_normalised # normalised



##### Deconvolve and Estimate WW Re #####


## TARU: you will need only N1, since they discontinued measuring both
# you also don't need this loop over the config_df perse, unless
# we have data for multiple wastewater treatment plants
config_df = expand.grid("region" = c('ZH'),  
                        'incidence_var' = c('norm_n1'),
                        'GammaParams' = list(c('incubation', 'benefield')) )
# the two parameters for the shedding distribution- Gamma. Incubation time and T2.

deconv_ww_data <- data.frame()
Re_ww <- data.frame()

# for(row_i in 1:nrow(config_df)){
#   new_deconv_data = deconvolveIncidence(ww_data %>% filter(region == config_df[row_i, 'region']), 
#                                         incidence_var = config_df[row_i, 'incidence_var'],
#                                         getCountParams(unlist(config_df[row_i, 'GammaParams'])[1]), 
#                                         getCountParams(unlist(config_df[row_i, 'GammaParams'])[2]),
#                                         smooth_param = TRUE, n_boot = 100)
#   
#   new_deconv_data <- new_deconv_data %>%
#     mutate(incidence_var = config_df[row_i, 'incidence_var'],
#            incubationParams = unlist(config_df[row_i, 'GammaParams'])[1], 
#            onsetToCountParams = unlist(config_df[row_i, 'GammaParams'])[2],
#            GammaParams = paste0(incubationParams, '_', onsetToCountParams),
#            source = GammaParams)
#   
#   ##### Get Re
#   new_Re_ww = getReBootstrap(new_deconv_data)
#   new_Re_ww <- new_Re_ww %>%
#     mutate(variable = config_df[row_i, 'incidence_var'],
#            incubationParams = unlist(config_df[row_i, 'GammaParams'])[1], 
#            onsetToCountParams = unlist(config_df[row_i, 'GammaParams'])[2],
#            GammaParams = paste0(incubationParams, '_', onsetToCountParams),
#            region = config_df[row_i, 'region'])
#   
#   deconv_ww_data <- bind_rows(deconv_ww_data, new_deconv_data)
#   Re_ww = bind_rows(Re_ww, new_Re_ww)
# }

# currently we only have data from Zurich:

row_i <- 1

# Time consuming ####
# - set to run after data changes 
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

##### Get Re #####
new_Re_ww = getReBootstrap(new_deconv_data)
new_Re_ww <- new_Re_ww %>%
  mutate(variable = config_df[row_i, 'incidence_var'],
         incubationParams = unlist(config_df[row_i, 'GammaParams'])[1], 
         onsetToCountParams = unlist(config_df[row_i, 'GammaParams'])[2],
         GammaParams = paste0(incubationParams, '_', onsetToCountParams),
         region = config_df[row_i, 'region'])

deconv_ww_data <- bind_rows(deconv_ww_data, new_deconv_data)
Re_ww = bind_rows(Re_ww, new_Re_ww) # currently nothing in Re_ww, as not being calculated

##### Plot Deconvolved Cases #####
# this calculates the mean/sd over the bootstrap replicates
mean_deconv_data <- deconv_ww_data %>%
  group_by(date, region, country, source, data_type, incidence_var, 
           incubationParams, onsetToCountParams, GammaParams) %>%
  summarise(sd = sd(value),
            value = mean(value),
            .groups = 'drop')




deconv_plot <- ggplot() +
  geom_line(data = mean_deconv_data, aes(x = date, y = value)) +
  # geom_errorbar(data = plot_deconv_cases %>% rename(case_type = data_type),
  #               aes(x=date, ymin = (value -sd),  ymax = (value +sd),
  #                   colour = case_type), show.legend = F) +
  geom_errorbar(data = mean_deconv_data,
                aes(x=date, ymin = value -sd,  ymax = value +sd, colour = incidence_var),
                show.legend = T) +
  facet_wrap(vars(data_type), ncol = 1, scale = 'free_y') +
  labs(x = 'Date' , y='Devonvolved values') +
  scale_x_date(limits = c(as_date(min(mean_deconv_data[["date"]])), 
                          as_date(max(mean_deconv_data[["date"]]))) ) +
  scale_colour_manual(values = viridis(4),
                      breaks = c("norm_n1"),
                      labels = c("N1"),
                      name = 'Variable') +
  guides(color = guide_legend(override.aes = list(size=5))) +
  theme(
    legend.position = 'bottom',
    strip.text.x= element_blank(),
    panel.spacing.y = unit(2, "lines")
  )

deconv_plot

# cases - from catchment area plotted

cases_plot <- ggplot() +
  geom_point(data = raw_data_ZH, aes(x = date, y = cases)) +
  labs(x = 'Date' , y='Infections per day') +
  geom_line(colour = 'black', linetype = 'dashed') +
  geom_smooth(method = 'loess', colour = 'blue',
              method.args = list(span = 0.05, degree = 1) ) +
  theme(
    legend.position = 'bottom',
    strip.text.x= element_blank(),
    panel.spacing.y = unit(2, "lines")
  )

cases_plot <- ggplot(raw_data_ZH, aes(x=date, y = cases)) +
  geom_point(colour = 'blue') +
  geom_line(colour = 'black', linetype = 'dashed') +
  geom_smooth(method = 'loess', colour = 'black',
              method.args = list(span = 0.05, degree = 1) ) +
  labs(x = 'Date' , y='Confirmed cases per day') +
  ggtitle("Confirmed cases in the Werdhölzli (Zurich) catchment area") 

cases_plot # looks different as wastewater does not give EXACT cases, but
# is useful to see the changes of covid levels in the wastewater
# which helps compute Re
# the actual deconvolution values do not matter - they are not exactly cases

#ggsave(plot = deconv_plot, paste0(plot_dir, '/', 'Wastewater_data_deconv_CH.pdf'), height = 11, width = 16)
#ggsave(plot = deconv_plot, paste0(plot_dir, '/', 'Wastewater_data_deconv_CH.png'), height = 11, width = 16)


##### Plot Rww ####


# just simple Rww for now
Re_ww <- Re_ww %>%
  mutate(variable = recode(variable,
                           'norm_n1' = 'N1'))


Re_plot_ww <- ggplot() +
  geom_ribbon(data = Re_ww, aes(x = date, ymin = median_R_lowHPD,
                                   ymax = median_R_highHPD, fill = data_type),
              alpha = 0.2, show.legend = F) +
  geom_line(data = Re_ww,
            aes(x = date, y = median_R_mean, colour = data_type), 
            alpha = 0.7, show.legend = F) +
  scale_x_date(limits = c(as_date(min(Re_ww[["date"]])), 
                          as_date(max(Re_ww[["date"]])))
  ) +
  labs( x = 'Date', y = 'Estimated Re', 
        colour = 'Observation Type', fill = 'Observation Type')

Re_plot_ww


# Matching Re estimates from report data ####

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
  bind_rows(Re_ww_needed) %>% mutate(data_type = factor(data_type)) %>%
  mutate(data_type = recode_factor(data_type, "infection_norm_n1" = "Wastewater"))

  #%>%
  # mutate(source = 'Case data',
  #        plot_row = case_when(data_type == 'Confirmed cases' ~ 'top',
  #                             data_type == 'Hospitalized patients' ~ 'middle',
  #                             data_type == 'Deaths' ~ 'bottom'),
  #        plot_row = factor(plot_row, levels = c('top', 'middle', 'bottom')))
## consider binding rows with ww results?



## Plot Re for all ####

# Give the option to the user to select which one they want to see.

Re_plot <- ggplot(plotData) +
  geom_line(aes(x = date, y = median_R_mean, colour = data_type), 
            alpha = 0.7) +
  geom_ribbon(aes(x = date, ymin = median_R_lowHPD,
                                   ymax = median_R_highHPD, fill = data_type),
              alpha = 0.2, show.legend = F) +
  geom_hline(yintercept = 1) +
  scale_colour_viridis(discrete = T) +
  scale_fill_viridis(discrete = T) +
  scale_x_date(limits = c(as_date(min(plotData[["date"]])), 
                          as_date(max(plotData[["date"]])))
  ) +
  labs( x = 'Date', y = 'Estimated Re',
        colour = 'Type', fill = 'Type') +
  guides(color = guide_legend(override.aes = list(size=5))) +
  theme(
     panel.spacing.y = unit(2, "lines"),
     legend.position = 'bottom'
  )

Re_plot

#ggsave(plot = Re_plot, paste0(plot_dir, '/', 'Wastewater_Re_CH.pdf'), height = 12, width = 14)
#ggsave(plot = Re_plot, paste0(plot_dir, '/', 'Wastewater_Re_CH.png'), height = 12, width = 14)

