# plots ####
library(tidyverse)
library(lubridate)
library(patchwork) 
library(viridis)
library(EpiEstim)


# cronjobs to do source work ####
source("helper_code/reading_in.R") # 
#source("helper_code/Rww_estimation.R") # will later no longer be needed

# Reading in cantonal Re estimates ####

Restimates_url = "https://raw.githubusercontent.com/covid-19-Re/dailyRe-Data/master/CHE-estimates.csv"

Restimates_canton <- read_csv(Restimates_url,
                              col_names = c('country', 'region','source','data_type',
                                            'estimate_type','date','median_R_mean',
                                            'median_R_highHPD','median_R_lowHPD',
                                            'countryIso3'),
                              col_types = cols(date = col_date(format = '')),
                              skip = 1) 

Restimates_canton <- Restimates_canton %>% filter(estimate_type == 'Cori_slidingWindow',
                                                  data_type != 'Confirmed cases / tests',
                                                  date >= as_date("2020-09-01")) %>%
  dplyr::select(-estimate_type, -countryIso3, -country, -source)



# Reading in cantonal Rww estimates ####

Re_ww_needed <- read_csv("rww_data/Rww_cantonal.csv")

# Binding the Re and Rww estimates ####

plotData <- Restimates_canton %>%
  bind_rows(Re_ww_needed) %>% mutate(data_type = factor(data_type)) %>% # the rww binded
  mutate(data_type = recode_factor(data_type, "infection_norm_n1" = "Wastewater"))

# Raw plots ####

# List of all raw ww plots.
all_raw_plots <- list()

date_range <- range((plotData %>% filter(data_type=="Wastewater") %>% select(date))[["date"]])

#### Wastewater raw  - Zurich ####

raw_plotter <- function(data = ww_data, canton) {
  ww_data %>% filter(region == canton) %>% 
    ggplot( ) +
    geom_point(aes(x=date, y = n1, colour = name_orig)) +
    scale_x_date(limits = c(date_range[1], date_range[2]), 
                 date_breaks = "months", date_labels = "%b") +
    scale_colour_manual(values = c(viridis(4)[1], "grey"), #'lightseagreen'
                        labels = c('N1', 'Imputed'),
                        breaks = c('N1', 'Imputed'),
                        name = 'Reading') +
    geom_line(data = ww_data %>% filter(region == canton) %>% filter(orig_data), 
              aes(x=date, y= n1,colour = name_orig), linetype = 'dashed', colour = "black") +
    labs(x = 'Date' , y='Gene copies per day') +
    ggtitle(paste0("SARS-CoV2-RNA copies in ", canton," Wastewater")) +
    theme_minimal() +
    theme(strip.text = element_text(size=20),
          axis.text= element_text(size=17),
          axis.title =  element_text(size=20),
          legend.text= element_text(size=17),
          legend.title= element_text(size=20),
          plot.title = element_text(size = 20),
          panel.spacing.y = unit(2, "lines"),
          legend.position = 'bottom')
}



# Re plots ####

re_plotter <- function(data = ww_data, source, canton) {
  plotData %>% filter(region == canton) %>%
    filter(data_type %in% source) %>%
    ggplot() +
    geom_line(aes(x = date, y = median_R_mean, colour = data_type), 
              alpha = 0.7) +
    geom_ribbon(aes(x = date, ymin = median_R_lowHPD,
                    ymax = median_R_highHPD, fill = data_type),
                alpha = 0.2, show.legend = F) +
    geom_hline(yintercept = 1) +
    scale_colour_manual(values = c(viridis(4)), #'lightseagreen'
                        labels = c('Wastewater', 'Confirmed cases', 'Deaths', 'Hospitalized patients'),
                        breaks = c('Wastewater', 'Confirmed cases', 'Deaths', 'Hospitalized patients')) +
    scale_fill_manual(values = c(viridis(4)), #'lightseagreen'
                       labels = c('Wastewater', 'Confirmed cases', 'Deaths', 'Hospitalized patients'),
                       breaks = c('Wastewater', 'Confirmed cases', 'Deaths', 'Hospitalized patients')) +
    scale_x_date(limits = c(date_range[1], date_range[2]), 
                 date_breaks = "months", date_labels = "%b") +
    coord_cartesian(ylim = c(0, 2)) +
    labs( x = 'Date', y = expression("Estimated R"["e"]),
          colour = 'Source', fill = 'Source') +
    guides(color = guide_legend(override.aes = list(size=5))) + 
    ggtitle(expression("Estimated R"["e"]*" using different data sources")) + 
    theme_minimal() +
    theme(strip.text = element_text(size=20),
          axis.text= element_text(size=17),
          axis.title =  element_text(size=20),
          legend.text= element_text(size=17),
          legend.title= element_text(size=20),
          plot.title = element_text(size = 20),
          panel.spacing.y = unit(2, "lines"),
          legend.position = 'bottom')
}




