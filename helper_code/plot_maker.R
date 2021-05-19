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
  dplyr::select(-estimate_type, -countryIso3, -country, -source) %>%
  mutate(data_type = recode_factor(data_type, 'Confirmed cases' = 'Confirmed (Canton)'))



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



# reference:
ref <- c("ZH"="Zurich" ,  "VD"="Lausanne",
         "SG"="Altenrhein", "GR"="Chur",
         "FR"="Laupen", "TI"="Lugano")

ref_size <- c("ZH"="450'000" ,  "VD"="240'000",
              "SG"="64'000", "GR"="55'000",
              "FR"="62'000", "TI"="124'000")

# Raw plots ####

case_plotter <- function(data = case_data, canton) {
  date_range <- range((ww_data %>% filter(region == canton) %>% select(date))[["date"]])
  
  data %>% filter(region == canton) %>% 
    ggplot(aes(x=date, y = cases) ) +
    geom_bar(stat="identity", colour = viridis(5)[4], fill = viridis(5)[4], alpha = 0.7) +
    scale_x_date(limits = c(date_range[1], Sys.Date()), 
                 date_breaks = "months", date_labels = "%b") +
    labs(x = 'Date' , y=expression("Cases per 100'000 residents")) +
    ggtitle(bquote(.(ref[[canton]])*"'s Catchment Area ("*.(ref_size[[canton]])*" residents): Cases, SARS-CoV Gene copies in Wastewater, Estimated R"['e'])) +
    theme_minimal() +
    theme(strip.text = element_text(size=18),
          axis.text= element_text(size=15),
          axis.title =  element_text(size=18),
          legend.text= element_text(size=15),
          legend.title= element_text(size=18),
          plot.title = element_text(size = 17),
          panel.spacing.y = unit(2, "lines"),
          legend.position = 'bottom',
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
}

raw_plotter <- function(data, canton) {
  n <- ww_data %>% filter(region==canton) %>% 
    group_by(quantification_flag) %>% tally() %>% nrow()
  
  date_range <- range((ww_data %>% filter(region == canton) %>% select(date))[["date"]])
  
  data %>% filter(region == canton) %>% mutate(n1 = n1/10^13) %>%
    ggplot( ) +
    geom_point(aes(x=date, y = n1, colour = quantification_flag)) +
    scale_x_date(limits = c(date_range[1], Sys.Date()), 
                 date_breaks = "months", date_labels = "%b") +
    scale_colour_manual(values = c(viridis(4)[1], 'darkgrey', 'firebrick', viridis(5)[5]), #'lightseagreen'
                        labels = c('> LOQ', 'Imputed', '> LOD', '< LOD'),
                        breaks = c('> LOQ', 'Imputed', '> LOD', '< LOD'),
                        name = 'Quantification flag**', 
                        guide = guide_legend(override.aes = list(size = 3) )) + # to increase size of point in legend
    geom_line(data = data %>% filter(region == canton) %>% filter(orig_data)  %>% mutate(n1 = n1/10^13), 
              aes(x=date, y= n1,colour = name_orig), linetype = 'dashed', colour = "black") +
    labs(x = 'Date' , y=expression("Gene copies ("%*%"10"^13*")")) +
    #ggtitle(paste0("SARS-CoV2-RNA copies in Wastewater in ", ref[[canton]])) +
    theme_minimal() +
    theme(strip.text = element_text(size=18),
          axis.text= element_text(size=14),
          axis.title =  element_text(size=17.5),
          legend.text= element_text(size=15),
          legend.title= element_text(size=18),
          plot.title = element_text(size = 18),
          panel.spacing.y = unit(2, "lines"),
          legend.position = 'bottom',
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
}

# Re plots ####

re_plotter <- function(source, canton) {
  date_range <- range((ww_data %>% filter(region == canton) %>% select(date))[["date"]])
  # For CHUR: leave out confirmed catchment case Re for now (as lots of missing data) ---------
  if (canton == "GR") {
    source <- source[! source %in% 'Confirmed (Catchment)']
  }
  new_data <- plotData %>% filter(region %in% canton) %>%
    filter(data_type %in% source) 
  
  data_ends <- new_data %>% group_by(data_type) %>% filter(row_number()==n())
  disc <- "*This is the most recent possible Re estimate due to delays between infection being observed."
  
  new_data %>%
    ggplot() +
    geom_line(aes(x = date, y = median_R_mean, colour = data_type), 
              alpha = 0.7, lwd = 0.8) +
    geom_ribbon(aes(x = date, ymin = median_R_lowHPD,
                    ymax = median_R_highHPD, fill = data_type),
                alpha = 0.2, show.legend = F) +
    geom_hline(yintercept = 1) +
    geom_point(aes(x = date, y = median_R_mean, colour = data_type), 
               data = data_ends, shape = 8) +
    scale_colour_manual(values = viridis(5)[c(1, 4, 5, 3, 2)], #'lightseagreen'
                        labels = c('Wastewater', 'Confirmed (Catchment)', 'Confirmed (Canton)', 
                                   'Deaths', 'Hospitalized patients'),
                        breaks = c('Wastewater', 'Confirmed (Catchment)','Confirmed (Canton)', 
                                   'Deaths', 'Hospitalized patients')) +
    scale_fill_manual(values = viridis(5)[c(1, 4, 5, 3, 2)], #'lightseagreen'
                       labels = c('Wastewater', 'Confirmed (Catchment)', 'Confirmed (Canton)', 
                                  'Deaths', 'Hospitalized patients'),
                       breaks = c('Wastewater', 'Confirmed (Catchment)', 'Confirmed (Canton)', 
                                  'Deaths', 'Hospitalized patients')) +
    scale_x_date(limits = c(date_range[1], Sys.Date()), 
                 date_breaks = "months", date_labels = "%b") +
    coord_cartesian(ylim = c(0, 2)) +
    labs( x = 'Date', y = bquote("Estimated R"['e']~" (95% CI)"),
          colour = 'Source', fill = 'Source') +
    guides(color = guide_legend(override.aes = list(size=5, shape = 0))) + 
    #ggtitle(expression("Estimated R"["e"]*" using Different Data Sources")) + 
    theme_minimal() +
    theme(strip.text = element_text(size=18),
          axis.text= element_text(size=15),
          axis.title =  element_text(size=18),
          legend.text= element_text(size=15),
          legend.title= element_text(size=18),
          plot.title = element_text(size = 18),
          panel.spacing.y = unit(2, "lines"),
          legend.position = 'bottom') +
    annotate(geom = 'text', 
             label = disc, 
             x = summary(date_range)[['3rd Qu.']], y = 0.1, hjust = 0.5, vjust = 1, size = 4)
}
# special plot for Chur - 2 cantons ------
re_plotter2 <- function(source, canton) {
  date_range <- range((ww_data %>% filter(region %in% canton) %>% select(date))[["date"]])
  canton <- c("BE", "FR")
  # Rww and Rcc for catchment unaffected. Other sources change.
  
  source_canton <- source[source %in% c('Confirmed (Canton)')]
  source_without_canton <- source[! source %in% c('Confirmed (Canton)')]
  
  bern_confirmed <- plotData %>% filter(region == "BE") %>%
    filter(data_type == "Confirmed (Canton)") %>%
    mutate(data_type = recode_factor(data_type, 'Confirmed (Canton)' = 'Confirmed (Bern)'))
  
  fribourg_confirmed <- plotData %>% filter(region == "FR") %>%
    filter(data_type == "Confirmed (Canton)") %>%
    mutate(data_type = recode_factor(data_type, 'Confirmed (Canton)' = 'Confirmed (Fribourg)'))
  
  new_data <- plotData %>% filter(region %in% canton) %>%
    filter(data_type %in% source_without_canton)
  
  if (length(source_canton)>0) {
    new_data <- new_data %>% bind_rows(bern_confirmed) %>% bind_rows(fribourg_confirmed)
  }
  
  # even though two cantons, rww only exists internally for FR! (so BE makes no diff)
  # Must now include plots for BE
  disc <- "*This is the most recent possible Re estimate due to delays between infection being observed."
  data_ends <- new_data %>% group_by(data_type) %>% filter(row_number()==n())
  
   new_data %>%
    ggplot() + 
    geom_line(aes(x = date, y = median_R_mean, colour = data_type), 
              alpha = 0.7, lwd = 0.8) +
    geom_ribbon(aes(x = date, ymin = median_R_lowHPD,
                    ymax = median_R_highHPD, fill = data_type),
                alpha = 0.2, show.legend = F) +
    geom_hline(yintercept = 1) +
    geom_point(aes(x = date, y = median_R_mean, colour = data_type), 
               data = data_ends, shape = 8) +
    scale_colour_manual(values = c(viridis(5)[c(1, 4)], '#CFE11CFF', '#2E6E8EFF'), #'lightseagreen'
                        labels = c('Wastewater', 'Confirmed (Catchment)', 'Confirmed (Fribourg)', 
                                   'Confirmed (Bern)'),
                        breaks = c('Wastewater', 'Confirmed (Catchment)','Confirmed (Fribourg)', 
                                   'Confirmed (Bern)')) +
    scale_fill_manual(values = c(viridis(5)[c(1, 4)], '#CFE11CFF', '#2E6E8EFF'), #'lightseagreen'
                      labels = c('Wastewater', 'Confirmed (Catchment)', 'Confirmed (Fribourg)', 
                                 'Confirmed (Bern)'),
                      breaks = c('Wastewater', 'Confirmed (Catchment)','Confirmed (Fribourg)', 
                                 'Confirmed (Bern)')) +
    scale_x_date(limits = c(date_range[1], Sys.Date()), 
                 date_breaks = "months", date_labels = "%b") +
    coord_cartesian(ylim = c(0, 2)) +
    labs( x = 'Date', y = bquote("Estimated R"['e']~" (95% CI)"),
          colour = 'Source', fill = 'Source') +
    guides(color = guide_legend(override.aes = list(size=5, shape = 0))) + 
    #ggtitle(expression("Estimated R"["e"]*" using Different Data Sources")) + 
    theme_minimal() +
    theme(strip.text = element_text(size=18),
          axis.text= element_text(size=15),
          axis.title =  element_text(size=18),
          legend.text= element_text(size=15),
          legend.title= element_text(size=18),
          plot.title = element_text(size = 18),
          panel.spacing.y = unit(2, "lines"),
          legend.position = 'bottom') +
     annotate(geom = 'text', 
              label = disc, 
              x = summary(date_range)[['3rd Qu.']], y = 0.1, hjust = 0.5, vjust = 1, size = 4)
}

# Plotting for all plants --------------

rww_plotter <- function(source = "Wastewater", canton) {
  date_range <- range((ww_data %>% filter(region %in% canton) %>% select(date))[["date"]])
  # for now, as Zurich is only one from Oct - Jan end. 
  # After: make into sliding scale
  date_range[1] <- as.Date('2021-02-01')
  
  plotData %>% filter(region %in% canton) %>%
    filter(data_type %in% source) %>%
    ggplot() +
    geom_line(aes(x = date, y = median_R_mean, colour = region), 
              alpha = 0.7) +
    geom_ribbon(aes(x = date, ymin = median_R_lowHPD,
                    ymax = median_R_highHPD, fill = region),
                alpha = 0.2, show.legend = F) +
    geom_hline(yintercept = 1) +
    scale_colour_manual(values = viridis(6), #'lightseagreen'
                        labels = c('Zurich', 'Lausanne', 'Altenrhein', 'Chur', 
                                   'Laupen', 'Lugano'),
                        breaks = c('ZH', 'VD', 'SG','GR', 
                                   'FR', 'TI')) +
    scale_fill_manual(values = viridis(6), #'lightseagreen'
                      labels = c('Zurich', 'Lausanne', 'Altenrhein', 'Chur', 
                                 'Laupen', 'Lugano'),
                      breaks = c('ZH', 'VD', 'SG','GR', 
                                 'FR', 'TI')) +
    scale_x_date(limits = c(date_range[1], Sys.Date()), 
                 date_breaks = "months", date_labels = "%b") +
    coord_cartesian(ylim = c(0, 2)) +
    labs( x = 'Date', y = bquote("Estimated R"['e']~" (95% CI)"),
          colour = 'Canton', fill = 'Canton') +
    guides(color = guide_legend(override.aes = list(size=5))) + 
    ggtitle(expression("Estimated Wastewater R"["e"]*" for different cantons")) + 
    theme_minimal() +
    theme(strip.text = element_text(size=18),
          axis.text= element_text(size=15),
          axis.title =  element_text(size=18),
          legend.text= element_text(size=15),
          legend.title= element_text(size=18),
          plot.title = element_text(size = 18),
          panel.spacing.y = unit(2, "lines"),
          legend.position = 'bottom')
}
