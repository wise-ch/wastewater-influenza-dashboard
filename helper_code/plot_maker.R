# plots ####
library(tidyverse)
library(lubridate)
library(patchwork) 
library(viridis)


# cronjobs to do source work ####
source("helper_code/reading_in.R") # 

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
                                                  date >= as_date("2020-10-01")) %>%
  dplyr::select(-estimate_type, -countryIso3, -country, -source)



# Reading in cantonal Rww and catchment Rcc estimates ####

Re_ww_needed <- read_csv("rww_data/Rww_cantonal.csv")
Re_cc_needed <- read_csv("rww_data/Rcc_catchment.csv")

# Binding the Re and Rww estimates ####

plotData <- Restimates_canton %>%
  bind_rows(Re_ww_needed) %>% mutate(data_type = factor(data_type)) %>% # the rww binded
  mutate(data_type = recode_factor(data_type, "infection_norm_n1" = "Wastewater"))

plotData <- plotData %>%
  bind_rows(Re_cc_needed) %>% mutate(data_type = factor(data_type)) %>% # the rww binded
  mutate(data_type = recode_factor(data_type, "infection_cases" = "Confirmed (Catchment)"))

# Raw plots ####

case_plotter <- function(data, canton) {
  date_range <- range((plotData %>% filter(data_type=="Wastewater", region == canton) %>% select(date))[["date"]])
  ref <- c("ZH"="Zurich" ,  "VD"="Lausanne",
           "SG"="Altenrhein", "GR"="Chur",
           "FR"="Laupen", "TI"="Lugano")
  data %>% filter(region == canton) %>% 
    ggplot(aes(x=date, y = cases) ) +
    geom_bar(stat="identity", colour = viridis(4)[1], fill = viridis(4)[1], alpha = 0.7) +
    scale_x_date(limits = c(date_range[1], date_range[2]), 
                 date_breaks = "months", date_labels = "%b") +
    labs(x = 'Date' , y=expression("Confirmed Cases")) +
    ggtitle(paste0("Confirmed Cases in ", ref[[canton]], " Catchment Area")) +
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

raw_plotter <- function(data, canton) {
  date_range <- range((plotData %>% filter(data_type=="Wastewater", region == canton) %>% select(date))[["date"]])
  ref <- c("ZH"="Zurich" ,  "VD"="Lausanne",
            "SG"="Altenrhein", "GR"="Chur",
           "FR"="Laupen", "TI"="Lugano")
  data %>% filter(region == canton) %>% mutate(n1 = n1/10^13) %>%
    ggplot( ) +
    geom_point(aes(x=date, y = n1, colour = name_orig)) +
    scale_x_date(limits = c(date_range[1], date_range[2]), 
                 date_breaks = "months", date_labels = "%b") +
    scale_colour_manual(values = c(viridis(4)[1], "grey"), #'lightseagreen'
                        labels = c('N1', 'Imputed'),
                        breaks = c('N1', 'Imputed'),
                        name = 'Reading') +
    geom_line(data = data %>% filter(region == canton) %>% filter(orig_data)  %>% mutate(n1 = n1/10^13), 
              aes(x=date, y= n1,colour = name_orig), linetype = 'dashed', colour = "black") +
    labs(x = 'Date' , y=expression("Gene copies ("%*%"10"^13*")")) +
    ggtitle(paste0("SARS-CoV2-RNA copies in Wastewater in ", ref[[canton]])) +
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

re_plotter <- function(source, canton) {
  date_range <- range((plotData %>% filter(data_type=="Wastewater", region == canton) %>% select(date))[["date"]])
  
  plotData %>% filter(region == canton) %>%
    filter(data_type %in% source) %>%
    ggplot() +
    geom_line(aes(x = date, y = median_R_mean, colour = data_type), 
              alpha = 0.7) +
    geom_ribbon(aes(x = date, ymin = median_R_lowHPD,
                    ymax = median_R_highHPD, fill = data_type),
                alpha = 0.2, show.legend = F) +
    geom_hline(yintercept = 1) +
    scale_colour_manual(values = viridis(5)[c(1, 4, 5, 3, 2)], #'lightseagreen'
                        labels = c('Wastewater', 'Confirmed (Catchment)', 'Confirmed cases', 
                                   'Deaths', 'Hospitalized patients'),
                        breaks = c('Wastewater', 'Confirmed (Catchment)','Confirmed cases', 
                                   'Deaths', 'Hospitalized patients')) +
    scale_fill_manual(values = viridis(5)[c(1, 4, 5, 3, 2)], #'lightseagreen'
                       labels = c('Wastewater', 'Confirmed (Catchment)', 'Confirmed cases', 
                                  'Deaths', 'Hospitalized patients'),
                       breaks = c('Wastewater', 'Confirmed (Catchment)', 'Confirmed cases', 
                                  'Deaths', 'Hospitalized patients')) +
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




