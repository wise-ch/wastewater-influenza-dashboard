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

app_location = '/Users/jana/Documents/PhD/covid/covid-19-re-shiny-app'
source(paste0(app_location,'/app/otherScripts/2_utils_getInfectionIncidence.R'))
source(paste0(app_location,'/app/otherScripts/3_utils_doReEstimates.R'))

source('wastewater_functions.R')

plot_dir = '../figures'

theme_set(theme_minimal() +
            theme(
              strip.text = element_text(size=20),
              axis.text= element_text(size=17),
              axis.title =  element_text(size=20),
              legend.text= element_text(size=17),
              legend.title= element_text(size=20)
            ))

###########################################################
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
         date >= as_date("2020-09-01"),
         date <= as_date("2021-01-20")) %>%
  mutate(orig_data = TRUE) %>%
  complete(date = seq.Date(min(date), max(date), by = 'days')) %>%
  mutate(across(where(is.numeric), ~ zoo::na.approx(.x, na.rm = F) )) %>%
  mutate(region = 'ZH')

# Plot `raw' WW data #####

ggplot(raw_data_ZH, aes(x=date, y = n2)) +
  geom_point(colour = 'blue') +
  geom_line(colour = 'black', linetype = 'dashed') +
  geom_smooth(method = 'loess', colour = 'black',
              method.args = list(span = 0.05, degree = 1) ) +
  labs(x = 'Date' , y='N2 load')

###########################################################
##### Cantonal Level: Clinical Case Data #####
orig_cases_canton <- readRDS(paste0(app_location, '/app/data/countryData/CHE-Data.rds'))

orig_cases <- orig_cases_canton %>%
  filter(data_type %in% c('Confirmed cases', "Hospitalized patients"),
         region == 'ZH',
         date >= as_date("2020-08-15"),
         date <= as_date("2021-01-20"),
         date_type == 'report_plotting') %>%
  select(date, data_type, value) %>%
  pivot_wider(names_from = 'data_type', values_from = 'value') %>%
  rename(confirmed = `Confirmed cases`, hospitalised = `Hospitalized patients`)

plot_orig_cases_canton <- orig_cases_canton %>%
  filter(region == c('ZH'),
         is.na(local_infection),
         date >= as_date("2020-08-15"),
         date <= as_date("2021-01-20"),
          date_type == 'report_plotting',
         data_type %in% c("Confirmed cases", "Hospitalized patients")) %>%
         #data_type == "Hospitalized patients") %>%
  select(date, region, value, name = data_type)

plot_orig_cases <- plot_orig_cases_canton

## Deconvolved cases
deconv_cases_canton <- readRDS(paste0(app_location, '/app/data/countryData/CHE-DeconvolutedData.rds'))

plot_deconv_cases <- deconv_cases_canton %>%
  filter(region %in% c('ZH'),
         date >= as_date("2020-08-15"),
         date <= as_date("2021-01-20")) %>%
  select(-local_infection, -country, -source) %>%
  group_by(date, region, data_type) %>%
  summarise(sd = sd(value),
            value = mean(value),
            .groups = 'drop') %>%
  mutate(data_type = recode_factor(data_type,
                                 "infection_Confirmed cases" = "Confirmed cases",
                                 "infection_Hospitalized patients"= "Hospitalized patients",
                                 "infection_Deaths" = "Deaths") ) %>%
  filter(data_type %in% c("Confirmed cases", "Hospitalized patients"))

## Re estimates
Restimates_canton <- readRDS(paste0(app_location, '/app/data/countryData/CHE-Estimates.rds'))

Restimates <- Restimates_canton %>%
  filter(region %in% c('ZH'),
         date >= as_date("2020-08-15"),
         date <= as_date("2021-01-20"),
         data_type %in% c("Confirmed cases", "Hospitalized patients"))


# Plot Case incidence data #####

plot_orig_cases <- orig_cases %>% 
  pivot_longer(cols = c(-date)) %>%
  filter(name != 'death')
#filter(name == 'hospitalised')

case_data_plot <- ggplot(plot_orig_cases) +
  geom_bar(aes(x=date, y= value, fill = name), alpha = 0.5,
           position = 'identity', stat = 'identity', show.legend = F) +
  labs(x = 'Date' , y='Cases per day') +
  scale_x_date(limits = c(as_date('2020-08-15'), as_date('2021-01-20')) ) +
  scale_fill_manual(values = c(viridis(4)[1:2])) + 
  labs(colour = 'Variable') 

case_data_plot
#ggsave(plot = case_data_plot, paste0(plot_dir, '/', 'Case_data_raw_canton.pdf'), height = 11, width = 9)

###########################################################
## Normalisation ####

# At LOQ we see 1 case # Min observed
norm_min <- min(raw_data_ZH$n1, raw_data_ZH$n2)

### min. LOQ in gene copies per day ###
# 1000 * flow [m^3/day] = total liters/day
# 25 * (80/5) = LOQ in gene copies per 50 mL
#norm_LOQ = min(raw_data_ZH$flow) * 1000 * 20 * 25 * (80/5)


### ALL Normalised WW DATA ####
ww_data = bind_rows(raw_data_ZH)  %>%
  mutate(norm_n1 = n1/norm_min,
         norm_n2 = n2/norm_min)

# Plot normalised data #####
plot_raw_ww_data <- ww_data %>%
  dplyr::select(date, orig_data, region, N1 = n1, N2 = n2) %>%
  pivot_longer(cols = c(N1, N2)) %>%
  mutate(name_orig = ifelse(!is.na(orig_data), name, 'Imputed'))

ww_data_plot <- ggplot() +
  geom_point(data = plot_raw_ww_data, aes(x=date, y= value, colour = name_orig),
             size = 2, show.legend = F) +
  geom_line(data = plot_raw_ww_data %>% filter(orig_data), 
            aes(x=date, y= value,colour = name), linetype = 'dotted', show.legend = F) +
  labs(x = 'Date' , y='Gene copies per day') +
  scale_x_date(limits = c(as_date('2020-08-15'), as_date('2021-01-20')) ) +
  scale_colour_manual(values = c(viridis(4)[3:4], 'lightgrey'), 
                      labels = c('N1', 'N2', 'Imputed'),
                      breaks = c('N1', 'N2', 'Imputed'),
                      name = 'Variable') + 
  labs(colour = 'Variable') 

ww_data_plot

ggsave(plot = ww_data_plot, paste0(plot_dir, '/', 'Wastewater_data_raw_CH.pdf'), height = 11, width = 9)


###########################################################
##### Deconvolve and Estimate WW Re #####


## TARU: you will need only N1, since they discontinued measuring both
# you also don't need this loop over the config_df perse, unless
# we have data for multiple wastewater treatment plants
config_df = expand.grid("region" = c('ZH'),  
                        'incidence_var' = c('norm_n1', 'norm_n2'),
                        'GammaParams' = list(c('incubation', 'benefield')) )


deconv_ww_data <- data.frame()
Re_ww <- data.frame()

for(row_i in 1:nrow(config_df)){
  new_deconv_data = deconvolveIncidence(ww_data %>% filter(region == config_df[row_i, 'region']), 
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

##### Plot Deconvolved Cases #####
# this calculates the mean/sd over the bootstrap replicates
mean_deconv_data <- deconv_ww_data %>%
  group_by(date, region, country, source, data_type, incidence_var, 
           incubationParams, onsetToCountParams, GammaParams) %>%
  summarise(sd = sd(value),
            value = mean(value),
            .groups = 'drop')

deconv_plot <- ggplot() +
  geom_errorbar(data = plot_deconv_cases %>% rename(case_type = data_type), 
                aes(x=date, ymin = (value -sd),  ymax = (value +sd),
                    colour = case_type), show.legend = F) +
  geom_errorbar(data = mean_deconv_data, 
                aes(x=date, ymin = value -sd,  ymax = value +sd, colour = incidence_var),
                show.legend = T) +
  facet_wrap(vars(case_type), ncol = 1, scale = 'free_y') +
  labs(x = 'Date' , y='Infections per day') +
  scale_x_date(limits = c(as_date('2020-08-15'), as_date('2021-01-20')) ) +
  scale_colour_manual(values = viridis(4), 
                      breaks = c("Confirmed cases","Hospitalized patients",
                                 "norm_n1", "norm_n2"),
                      labels = c("Confirmed cases", "Hospitalized patients", "N1", "N2"),
                      name = 'Variable') + 
  guides(color = guide_legend(override.aes = list(size=5))) +
  theme(
    legend.position = 'bottom',
    strip.text.x= element_blank(),
    panel.spacing.y = unit(2, "lines")
  )

deconv_plot

#ggsave(plot = deconv_plot, paste0(plot_dir, '/', 'Wastewater_data_deconv_CH.pdf'), height = 11, width = 16)
ggsave(plot = deconv_plot, paste0(plot_dir, '/', 'Wastewater_data_deconv_CH.png'), height = 11, width = 16)

###########################################################
##### Plot Re ####

# Matching Re estimates from report data ####

date_ranges <- Re_ww %>%
  group_by(region) %>%
  summarise(min_date = min(date),
            max_date = max(date)) 

plotData <- Restimates %>%
  filter(region %in% c('ZH', 'VD'),
         estimate_type == 'Cori_slidingWindow',
         #data_type != 'Deaths',
         date >= date_ranges[date_ranges$region == 'ZH', ]$min_date,
         date <= date_ranges[date_ranges$region == 'ZH',]$max_date ) %>%
  dplyr::select(-estimate_type, -countryIso3, -country, -source) %>%
  mutate(source = 'Case data',
         plot_row = case_when(data_type == 'Confirmed cases' ~ 'top',
                              data_type == 'Hospitalized patients' ~ 'middle',
                              data_type == 'Deaths' ~ 'bot'),
         plot_row = factor(plot_row, levels = c('top', 'middle', 'bottom')))
## consider binding rows with ww results?

## Plot Re ####
Re_ww <- Re_ww %>%
  mutate(variable = recode(variable,
                           'norm_n1' = 'N1',
                           'norm_n2' = 'N2'))

Re_plot <- ggplot() +
  geom_ribbon(data = plotData, aes(x = date, ymin = median_R_lowHPD,
                                   ymax = median_R_highHPD, fill = data_type),
              alpha = 0.2, show.legend = F) +
  geom_line(data = plotData,
            aes(x = date, y = median_R_mean, colour = data_type), alpha = 0.7, show.legend = F) +
  geom_ribbon(data = Re_ww, aes(x = date, ymin = median_R_lowHPD,
                                ymax = median_R_highHPD,  fill = variable), alpha = 0.2, show.legend = F) +
  geom_line(data = Re_ww, 
            aes(x = date, y = median_R_mean, colour = variable),alpha = 0.7, show.legend = F) +
  geom_hline(yintercept = 1) +
  facet_grid(cols = vars(region), rows = vars(plot_row), scale = 'free_x') +
  coord_cartesian(ylim = c(0, 2.5)) +
  scale_colour_viridis(discrete = T) + 
  scale_fill_viridis(discrete = T) + 
  scale_x_date(limits = c(as_date('2020-08-15'), as_date('2021-01-20'))
  ) +
  labs( x = 'Date', y = 'Estimated Re', 
        colour = 'Observation Type', fill = 'Observation Type') +
  #guides(color = guide_legend(override.aes = list(size=5))) +
  theme(
    panel.spacing.y = unit(2, "lines"),
    strip.text= element_blank(),
    #legend.position = 'bottom'
  )

Re_plot

#ggsave(plot = Re_plot, paste0(plot_dir, '/', 'Wastewater_Re_CH.pdf'), height = 12, width = 14)
ggsave(plot = Re_plot, paste0(plot_dir, '/', 'Wastewater_Re_CH.png'), height = 12, width = 14)

