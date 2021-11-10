# plots ####
library(tidyverse)
library(lubridate)
#library(patchwork)
library(viridis)

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



# Reading in Rww and catchment Rcc estimates ####

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
ref <- c("ZH"="Zurich" ,  "GE"="Geneva",
         "SG"="Altenrhein", "GR"="Chur",
         "FR"="Laupen", "TI"="Lugano")

ref_size <- c("ZH"="471'000" ,  "GE"="454'000",
              "SG"="64'000", "GR"="55'000",
              "FR"="62'000", "TI"="124'000")

global_date_range <- range(ww_data$date)
  

# Raw plots ####

case_plotter <- function(data = case_data, canton, date_range, i18n = NA) {
  #date_range <- range((ww_data %>% filter(region == canton) %>% select(date))[["date"]])
  p1 <- i18n$t("Catchment area")
  p2 <- i18n$t("residents): Cases, SARS-CoV Gene copies in Wastewater,")
  p3 <- i18n$t("Estimated R")
  if (nchar(p3)>10) {
    # English and German
    main_title <-bquote(.(ref[[canton]])~.(p1)~"("~.(ref_size[[canton]])~.(p2)~.(p3)['e'])
  }
  else {
    # French and Italian
    p3 <- strsplit(p3, " ")[[1]][2]
    main_title <-bquote(.(ref[[canton]])~.(p1)~"("~.(ref_size[[canton]])~.(p2)~"R"['e']~.(p3))
  }

  data %>% filter(region == canton) %>% filter(date >= date_range[1] & date <= date_range[2]) %>%
    ggplot(aes(x=date, y = cases, fill = region) ) + # filling simply for a legend...
    geom_bar(stat="identity", colour = viridis(5)[4], alpha = 0.7) +
    scale_x_date(limits = c(date_range[1], date_range[2]),
                 date_breaks = "months", date_labels = "%b") +
    scale_y_continuous(labels = function(label) sprintf('%6.1f', label)) +
    labs(x = i18n$t("Date") , y=i18n$t("Cases per 100'000 residents")) +
    scale_fill_manual(name = '',values=rep(viridis(5)[4], 6), breaks = c('ZH', 'GE', 'SG','GR',
                                                          'FR', 'TI'),
                      labels=rep(i18n$t("Confirmed cases (in catchment area)"), 6)) + # legend
    ggtitle(main_title) +
    theme_minimal() +
    theme(strip.text = element_text(size=17),
          axis.text= element_text(size=14),
          axis.title =  element_text(size=16),
          legend.text= element_text(size=14),
          legend.title= element_text(size=17),
          plot.title = element_text(size = 16),
          panel.spacing.y = unit(2, "lines"),
          legend.position = 'bottom')
}

raw_plotter <- function(data, canton, date_range, i18n = NA) {
  n <- ww_data %>% filter(region==canton) %>%
    group_by(quantification_flag) %>% tally() %>% nrow()

  #date_range <- range((ww_data %>% filter(region == canton) %>% select(date))[["date"]])
  p1 <- i18n$t("Gene copies")
  ylabel <- bquote(.(p1)*" ("%*%"10"^12*")")
  data %>% filter(region == canton) %>% mutate(n1 = n1/10^12) %>%
    filter(date >= date_range[1] & date <= date_range[2]) %>%
    ggplot( ) +
    geom_point(aes(x=date, y = n1, colour = quantification_flag)) +
    scale_x_date(limits = c(date_range[1], date_range[2]),
                 date_breaks = "months", date_labels = "%b") +
    scale_y_continuous(labels = function(label) sprintf('%4.1f', label)) +
    scale_colour_manual(values = c(viridis(4)[1], 'darkgrey', 'firebrick', "#f4bc1c"), #'lightseagreen'
                        labels = c('> LOQ', i18n$t('Imputed'), '> LOD', '< LOD'),
                        breaks = c('> LOQ', 'Imputed', '> LOD', '< LOD'),
                        name = i18n$t('Quantification flag**'),
                        guide = guide_legend(override.aes = list(size = 3) )) + # to increase size of point in legend
    geom_line(data = data %>% filter(region == canton) %>% filter(orig_data)  %>% 
                filter(date >= date_range[1] & date <= date_range[2]) %>% mutate(n1 = n1/10^12),
              aes(x=date, y= n1,colour = name_orig), linetype = 'dashed', colour = "black") +
    labs(x = i18n$t("Date") , y=ylabel) +
    #ggtitle(paste0("SARS-CoV2-RNA copies in Wastewater in ", ref[[canton]])) +
    theme_minimal() +
    theme(strip.text = element_text(size=17),
          axis.text= element_text(size=14),
          axis.title =  element_text(size=15),
          legend.text= element_text(size=14),
          legend.title= element_text(size=17),
          plot.title = element_text(size = 18),
          panel.spacing.y = unit(2, "lines"),
          legend.position = 'bottom')
}

# Re plots ####

re_plotter <- function(source, canton, date_range, i18n = NA) {
  #date_range <- range((ww_data %>% filter(region == canton) %>% select(date))[["date"]])
  # For CHUR: leave out confirmed catchment case Re for now (as lots of missing data) ---------
  if (canton == "GR") {
    source <- source[! source %in% 'Confirmed (Catchment)']
  }
  new_data <- plotData %>% filter(region %in% canton) %>%
    filter(data_type %in% source) %>% filter(date >= date_range[1])

  data_ends <- new_data %>% group_by(data_type) %>% filter(row_number()==n())
  disc <- i18n$t("*This is the most recent possible Re estimate due to delays between infection and being observed.")
  
  p1 <- i18n$t("Estimated R")
  if (nchar(p1)>10) {
    # English and German
    ylabel <- bquote(.(p1)['e']~" (95% CI)")
  }
  else {
    # French and Italian
    p1 <- strsplit(p1, " ")[[1]][2]
    ylabel <- bquote("R"['e']~.(p1)~" (95% CI)")
  }
  
  new_data <- new_data %>% filter(date >= date_range[1] & date <= date_range[2])
  
  if (global_date_range[1]==date_range[1] & date_range[2]==Sys.Date()) { 
    ylimits <- c(0, 2)
  } else { 
     ylimits <- c(max(min(new_data$median_R_lowHPD), 0), min(max(new_data$median_R_highHPD), 2))}
  
  new_data  %>%
    ggplot() +
    geom_line(aes(x = date, y = median_R_mean, colour = data_type),
              alpha = 0.7, lwd = 0.8) +
    geom_ribbon(aes(x = date, ymin = median_R_lowHPD,
                    ymax = median_R_highHPD, fill = data_type),
                alpha = 0.2, show.legend = F) +
    geom_hline(yintercept = 1) +
    geom_point(aes(x = date, y = median_R_mean, colour = data_type),
               data = data_ends, shape = 8) +
    scale_colour_manual(values = c(viridis(5)[c(1, 4)], "#f4bc1c", viridis(5)[c(3, 2)]), #'lightseagreen'
                        labels = i18n$t(c('Wastewater', 'Confirmed cases (Catchment)', 'Confirmed cases (Canton)',
                                   'Deaths', 'Hospitalized patients')),
                        breaks = c('Wastewater', 'Confirmed (Catchment)','Confirmed (Canton)',
                                   'Deaths', 'Hospitalized patients')) +
    scale_fill_manual(values = c(viridis(5)[c(1, 4)], "#f4bc1c", viridis(5)[c(3, 2)]), #'lightseagreen'
                       labels = i18n$t(c('Wastewater', 'Confirmed cases (Catchment)', 'Confirmed cases (Canton)',
                                  'Deaths', 'Hospitalized patients')),
                       breaks = c('Wastewater', 'Confirmed (Catchment)', 'Confirmed (Canton)',
                                  'Deaths', 'Hospitalized patients')) +
    scale_x_date(limits = c(date_range[1], date_range[2]),
                 date_breaks = "months", date_labels = "%b") +
    scale_y_continuous(labels = function(label) sprintf('%6.1f', label)) +
    coord_cartesian(ylim = ylimits) + # change this? Autoadjust? but how?
    labs( x = i18n$t("Date"), y = ylabel,
          colour = i18n$t('Source'), fill = i18n$t('Source')) +
    guides(color = guide_legend(override.aes = list(size=5, shape = 0))) +
    theme_minimal() +
    theme(strip.text = element_text(size=17),
          axis.text= element_text(size=14),
          axis.title =  element_text(size=16),
          legend.text= element_text(size=14),
          legend.title= element_text(size=17),
          plot.title = element_text(size = 18),
          panel.spacing.y = unit(2, "lines"),
          legend.position = 'bottom') +
    annotate(geom = 'text',
             label = ifelse(is.infinite(min(data_ends$date)), " ",
                            ifelse(date_range[2] >= min(data_ends$date) , disc, " ")),
             x = summary(date_range)[['3rd Qu.']]-25, y = 0.1, hjust = 0.5, vjust = 1, size = 3.9)
}
# special plot for Chur - 2 cantons ------
re_plotter2 <- function(source, canton, date_range, i18n = NA) {
  #date_range <- range((ww_data %>% filter(region %in% canton) %>% select(date))[["date"]])
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
    filter(data_type %in% source_without_canton) %>% filter(date >= date_range[1])

  if (length(source_canton)>0) {
    new_data <- new_data %>% bind_rows(bern_confirmed) %>% bind_rows(fribourg_confirmed)
  }

  # even though two cantons, rww only exists internally for FR! (so BE makes no diff)
  # Must now include plots for BE
  disc <- "*This is the most recent possible Re estimate due to delays between infection and being observed."
  data_ends <- new_data %>% group_by(data_type) %>% filter(row_number()==n())
  
  new_data <- new_data %>% filter(date >= date_range[1] & date <= date_range[2])
  
  if (global_date_range[1]==date_range[1] & date_range[2]==Sys.Date()) { 
    ylimits <- c(0, 2)
  } else { 
    ylimits <- c(max(min(new_data$median_R_lowHPD), 0), min(max(new_data$median_R_highHPD), 2))}
  
  
  p1 <- i18n$t("Estimated R")
  if (nchar(p1)>10) {
    # English and German
    ylabel <- bquote(.(p1)['e']~" (95% CI)")
  }
  else {
    # French and Italian
    p1 <- strsplit(p1, " ")[[1]][2]
    ylabel <- bquote("R"['e']~.(p1)~" (95% CI)")
  }
  
  
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
                        labels = c('Wastewater', 'Confirmed cases (Catchment)', paste0(i18n$t('Confirmed cases'), ' (Fribourg)'),
                                   paste0(i18n$t('Confirmed cases'), ' (Bern)')),
                        breaks = c('Wastewater', 'Confirmed (Catchment)','Confirmed (Fribourg)',
                                   'Confirmed (Bern)')) +
    scale_fill_manual(values = c(viridis(5)[c(1, 4)], '#CFE11CFF', '#2E6E8EFF'), #'lightseagreen'
                      labels = c('Wastewater', 'Confirmed cases (Catchment)', paste0(i18n$t('Confirmed cases'), ' (Fribourg)'),
                                 paste0(i18n$t('Confirmed cases'), ' (Bern)')),
                      breaks = c('Wastewater', 'Confirmed (Catchment)','Confirmed (Fribourg)',
                                 'Confirmed (Bern)')) +
    scale_x_date(limits = c(date_range[1], date_range[2]),
                 date_breaks = "months", date_labels = "%b") +
    scale_y_continuous(labels = function(label) sprintf('%6.1f', label)) +
    coord_cartesian(ylim = ylimits) +
    labs( x = i18n$t("Date"), y = ylabel,
          colour = 'Source', fill = 'Source') +
    guides(color = guide_legend(override.aes = list(size=5, shape = 0))) +
    #ggtitle(expression("Estimated R"["e"]*" using Different Data Sources")) +
    theme_minimal() +
     theme(strip.text = element_text(size=17),
           axis.text= element_text(size=14),
           axis.title =  element_text(size=16),
           legend.text= element_text(size=14),
           legend.title= element_text(size=17),
           plot.title = element_text(size = 18),
           panel.spacing.y = unit(2, "lines"),
           legend.position = 'bottom') +
     annotate(geom = 'text',
              label = ifelse(is.infinite(min(data_ends$date)), " ",
                             ifelse(date_range[2] >= min(data_ends$date) , disc, " ")),
              x = summary(date_range)[['3rd Qu.']]-25, y = 0.1, hjust = 0.5, vjust = 1, size = 4)
}

# Plotting for all plants --------------

canton_plotter <- function(source, canton, date_range, i18n = NA) {
  #date_range <- range((ww_data %>% filter(region %in% canton) %>% select(date))[["date"]])
  # for now, as Zurich is the only one from Oct - Jan end.
  # After: make into sliding scale
  #date_range[1] <- as.Date('2021-02-01')
  p1 <- i18n$t("Estimated R")
  if (nchar(p1)>10) {
    # English and German
    ylabel <- bquote(.(p1)['e']~" (95% CI)")
  }
  else {
    # French and Italian
    p1 <- strsplit(p1, " ")[[1]][2]
    ylabel <- bquote("R"['e']~.(p1)~" (95% CI)")
  }
  
  plotData %>% filter(region %in% canton) %>%
    filter(data_type %in% source) %>%
    ggplot() +
    geom_line(aes(x = date, y = median_R_mean, colour = region),
              alpha = 0.7, lwd = 0.8) +
    geom_ribbon(aes(x = date, ymin = median_R_lowHPD,
                    ymax = median_R_highHPD, fill = region),
                alpha = 0.2, show.legend = F) +
    geom_hline(yintercept = 1) +
    scale_colour_manual(values = c(viridis(6)[1:5], "#f4bc1c"), #'lightseagreen'
                        labels = c('Zurich', 'Geneva', 'Altenrhein', 'Chur',
                                   'Laupen', 'Lugano'),
                        breaks = c('ZH', 'GE', 'SG','GR',
                                   'FR', 'TI')) +
    scale_fill_manual(values = c(viridis(6)[1:5], "#f4bc1c"), #'lightseagreen'
                      labels = c('Zurich', 'Geneva', 'Altenrhein', 'Chur',
                                 'Laupen', 'Lugano'),
                      breaks = c('ZH', 'GE', 'SG','GR',
                                 'FR', 'TI')) +
    scale_x_date(limits = c(date_range[1], date_range[2]),
                 date_breaks = "months", date_labels = "%b") +
    coord_cartesian(ylim = c(0, 2)) +
    labs( x = i18n$t('Date'), y = ylabel,
          colour = i18n$t('Canton'), fill = i18n$t('Canton')) +
    guides(color = guide_legend(override.aes = list(size=5))) +
    theme_minimal() +
    theme(strip.text = element_text(size=17),
          axis.text= element_text(size=14),
          axis.title =  element_text(size=16),
          legend.text= element_text(size=14),
          legend.title= element_text(size=17),
          plot.title = element_text(size = 18),
          panel.spacing.y = unit(2, "lines"),
          legend.position = 'bottom')
}


