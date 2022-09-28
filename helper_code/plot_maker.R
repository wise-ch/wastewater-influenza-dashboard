# plots ####
library(tidyverse)
library(lubridate)
library(viridis)
library(zoo)

# Reading in data: each helper script has function(s) to read in data set(s) ####
# Columns in Re data sets must be: region, pathogen_type, data_type, date, median_R_mean, median_R_highHPD, median_R_lowHPD
# Columns in observation data sets must be: region, pathogen_type, data_type, date, observation, (optional: observation_smooth, orig_data, protocol_status)

source("helper_code/reading_in/reading_in_flu_ili.R")
source("helper_code/reading_in/reading_in_flu_ww.R")
source("helper_code/reading_in/reading_in_covid_ww.R")
source("helper_code/reading_in/reading_in_covid_cases.R")

flu_ili_data <- load_flu_ili_all()
flu_ww_data <- load_flu_ww_all()
covid_ww_data <- load_covid_ww_all()
covid_catchment_cases_re <- load_covid_catchment_cases_re()
covid_cantonal_cases_re <- load_covid_cantonal_cases_re()

# Binding the Re estimates for all diseases from all data types ####
plotDataRe <- covid_cantonal_cases_re %>%
  bind_rows(covid_catchment_cases_re) %>%
  bind_rows(covid_ww_data$re) %>%
  bind_rows(flu_ili_data$re) %>%
  bind_rows(flu_ww_data$re)

# reference:
ref <- c("ZH"="Zurich" ,  "GE"="Geneva",
         "SG"="Altenrhein", "GR"="Chur",
         "FR"="Laupen", "TI"="Lugano")

ref_size <- c("ZH"="471'000" ,  "GE"="454'000",
              "SG"="64'000", "GR"="55'000",
              "FR"="62'000", "TI"="124'000")
  

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

transition_period <- as.Date(c('2021-11-10', '2021-11-30'))

raw_plotter <- function(data, canton, pathogen, date_range, i18n = NA) {

  data <- data %>%  mutate(measurement = measurement/10^12) %>% filter(region == canton, pathogen_type == pathogen)  %>%
    filter(date >= date_range[1] & date <= date_range[2])
  
  p1 <- i18n$t("Gene copies")
  ylabel <- bquote(.(p1)*" ("%*%"10"^12*")")
  
  p <- data  %>%
    ggplot( ) +  
    geom_point(data = data %>% filter(protocol_status =='latest', orig_data), 
               aes(x=date, y = measurement, colour = quantification_flag), alpha = 0.5) +
    geom_line(data = data %>% filter(protocol_status == 'latest') %>% filter(!is.na(measurement)),
              aes(x=date, y= measurement,colour = 'black'), linetype = 'dotted', colour = "black") +
    scale_x_date(limits = c(date_range[1], date_range[2]),
                 date_breaks = "months", date_labels = "%b") +
    scale_y_continuous(labels = function(label) sprintf('%4.1f', label)) +
    scale_colour_manual(values = c(viridis(4)[1], 'darkgrey', 'firebrick', "#f4bc1c"), #'lightseagreen'
                        labels = c('> LOQ', i18n$t('Imputed'), '> LOD', '< LOD'),
                        breaks = c('> LOQ', 'Imputed', '> LOD', '< LOD'),
                        name = i18n$t('Quantification flag**'),
                        guide = guide_legend(override.aes = list(size = 3) )) + # to increase size of point in legend
    labs(x = i18n$t("Date") , y=ylabel) +
    theme_minimal() +
    theme(strip.text = element_text(size=17),
          axis.text= element_text(size=14),
          axis.title =  element_text(size=15),
          legend.text= element_text(size=14),
          legend.title= element_text(size=17),
          plot.title = element_text(size = 18),
          panel.spacing.y = unit(2, "lines"),
          legend.position = 'bottom')
  
    # Add layers to plot for data generated under older protocols
    if (pathogen == "COVID") {
      p <- p + 
        annotate('rect',xmin = as.Date('2021-11-10'), xmax = as.Date('2021-11-30'), ymin = 0, ymax = Inf ,
                        fill = 'grey', alpha = 0.4) +
        # old protocol
        geom_point(data = data %>% filter(protocol =='v3.1'), aes(x=date, y = measurement, colour = quantification_flag)) +
        geom_line(data = data %>% filter(protocol=='v3.1') %>% filter(!is.na(measurement)),
                  aes(x=date, y= measurement,colour = 'black'), linetype = 'dashed', colour = "black") +
        geom_vline(xintercept = as.Date(c('2021-11-10', '2021-11-30')), linetype = 'dotdash', alpha = 0.6)
    }
  p
}


# Re plots ####

re_plotter <- function(source, canton, pathogen, date_range, i18n = NA) {
  
  new_data <- plotDataRe %>% 
    filter(region %in% canton) %>%
    filter(pathogen_type %in% pathogen) %>%
    filter(data_type %in% source)
  
  data_ends <- new_data %>% filter(data_type != 'Wastewater') %>% group_by(data_type) %>% filter(row_number()==n())
  disc <- i18n$t("*This is the most recent possible Re estimate due to delays between infection and being observed.")
  
  p1 <- i18n$t("Estimated R")
  if (nchar(p1)>10) {
    # English and German
    ylabel <- bquote(.(p1)['e']~" (95% CI)")
  } else {
    # French and Italian
    p1 <- strsplit(p1, " ")[[1]][2]
    ylabel <- bquote("R"['e']~.(p1)~" (95% CI)")
  }
  
  new_data <- new_data %>% filter(date >= date_range[1] & date <= date_range[2])
  
  if (range((ww_data_all %>% filter(region == canton, pathogen_type == pathogen) %>%
             select(date))[["date"]])[1]==date_range[1] & date_range[2]==Sys.Date()) { 
    ylimits <- c(0, 2) # default
  } else { 
    ylimits <- c(max(min(new_data$median_R_lowHPD, na.rm = T), 0), min(max(new_data$median_R_highHPD, na.rm = T), 2))}
  
  p <- new_data  %>%
    ggplot() +
    geom_line(aes(x = date, y = median_R_mean, colour = data_type),
              alpha = 0.7, lwd = 0.8) +
    geom_ribbon(aes(x = date, ymin = median_R_lowHPD,
                    ymax = median_R_highHPD, fill = data_type),
                alpha = 0.2, show.legend = F) +
    geom_hline(yintercept = 1) +
    geom_point(aes(x = date, y = median_R_mean, colour = data_type),
               data = data_ends, shape = 8) +
    scale_colour_manual(values = c(viridis(5)[c(1, 4)], "#f4bc1c", viridis(5)[c(3, 2)]), #repeating purple
                        labels = i18n$t(c('Wastewater','Confirmed cases (Catchment)',
                        'Confirmed cases (Canton)',
                        'Deaths', 'Hospitalized patients')),
                        breaks = c('Wastewater','Confirmed (Catchment)', 
                                   'Confirmed (Canton)',
                                   'Deaths', 'Hospitalized patients')) +
    scale_fill_manual(values = c(viridis(5)[c(1, 4)], "#f4bc1c", viridis(5)[c(3, 2)]), #repeating purple
                      labels = i18n$t(c('Wastewater','Confirmed cases (Catchment)',
                                        'Confirmed cases (Canton)',
                                        'Deaths', 'Hospitalized patients')),
                      breaks = c('Wastewater','Confirmed (Catchment)', 
                                 'Confirmed (Canton)',
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
          legend.position = 'bottom') #+ # TODO: add influenza case-based Re
    # annotate(geom = 'text',
    #          label = ifelse(is.infinite(min(data_ends$date)), " ",
    #                         ifelse(date_range[2] >= min(data_ends$date) , disc, " ")),
    #          x = summary(date_range)[['3rd Qu.']]-25, y = 0.1, hjust = 0.5, vjust = 1, size = 3.9)
    
  # Add layers to plot for data generated under older protocols
  if (pathogen == "COVID" & 'Wastewater' %in% source) {
    
    pmg <- plotDataRe %>% filter(region %in% canton) %>%
      filter(data_type == 'Wastewater (PMG2)') %>% filter(date >= date_range[1])
    
    if (range((ww_data_all %>% filter(region == canton) %>%
               select(date))[["date"]])[1]==date_range[1] & date_range[2]==Sys.Date()) { 
      ylimits <- c(0, 2) # default
    } else { 
      ylimits <- c(min(range(pmg$median_R_mean)[1], range(new_data$median_R_mean)[1]), 
                   max(range(pmg$median_R_mean)[1], 2))}
    
    p <- p + 
      annotate('rect',xmin = as.Date('2021-10-31'), xmax = as.Date('2021-11-20'), ymin = 0, ymax = Inf ,
               fill = 'grey', alpha = 0.4) +
      # old protocol
      geom_line(data = pmg , 
                aes(x = date, y = median_R_mean), 
                linetype = 'dashed', colour = viridis(1), alpha = 0.6, lwd = 0.8) +
      geom_ribbon(data = pmg %>% filter(region==canton),
                  aes(x = date, ymin = median_R_lowHPD,
                      ymax = median_R_highHPD) , 
                  colour = NA, alpha = 0.1, show.legend = F, fill = viridis(1)) +
      geom_point(aes(x = date, y = median_R_mean, colour = data_type),
                 data = pmg %>% filter(row_number()==n()), shape = 8, colour = viridis(1)) +
      geom_point(aes(x = date, y = median_R_mean, colour = data_type),
                 data = new_data %>% filter(data_type == 'Wastewater') %>% filter(row_number()==n()), 
                 shape = 4, colour = viridis(1)) +
      coord_cartesian(ylim = ylimits) 
      geom_vline(xintercept = as.Date(c('2021-10-31', '2021-11-20')), linetype = 'dotdash', alpha = 0.6)
  }
  
  p
}


# special plot for Chur - 2 cantons ------
re_plotter2 <- function(source, canton, date_range, i18n = NA) {
  #date_range <- range((ww_data %>% filter(region %in% canton) %>% select(date))[["date"]])
  canton <- c("BE", "FR")
  # Rww and Rcc for catchment unaffected. Other sources change.

  source_canton <- source[source %in% c('Confirmed (Canton)')]
  source_without_canton <- source[! source %in% c('Confirmed (Canton)')]

  bern_confirmed <- plotDataRe %>% filter(region == "BE") %>%
    filter(data_type == "Confirmed (Canton)") %>%
    mutate(data_type = recode_factor(data_type, 'Confirmed (Canton)' = 'Confirmed (Bern)'))

  fribourg_confirmed <- plotDataRe %>% filter(region == "FR") %>%
    filter(data_type == "Confirmed (Canton)") %>%
    mutate(data_type = recode_factor(data_type, 'Confirmed (Canton)' = 'Confirmed (Fribourg)'))

  new_data <- plotDataRe %>% filter(region %in% canton) %>%
    filter(data_type %in% source_without_canton) %>% filter(date >= date_range[1])

  if (length(source_canton)>0) {
    new_data <- new_data %>% bind_rows(bern_confirmed) %>% bind_rows(fribourg_confirmed)
  }

  # even though two cantons, rww only exists internally for FR! (so BE makes no diff)
  # Must now include plots for BE
  disc <- "*This is the most recent possible Re estimate due to delays between infection and being observed."
  data_ends <- new_data %>% filter(data_type != 'Wastewater') %>% group_by(data_type) %>% filter(row_number()==n())
  
  new_data <- new_data %>% filter(date >= date_range[1] & date <= date_range[2])
  
  if (range((ww_data_all %>% filter(region == 'FR') %>%
             select(date))[["date"]])[1]==date_range[1] & date_range[2]==Sys.Date()) { 
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
  
  
  pp <- new_data %>%
    ggplot() +
    geom_line(aes(x = date, y = median_R_mean, colour = data_type),
              alpha = 0.7, lwd = 0.8) +
    geom_ribbon(aes(x = date, ymin = median_R_lowHPD,
                    ymax = median_R_highHPD, fill = data_type),
                alpha = 0.2, show.legend = F) +
    geom_hline(yintercept = 1) +
    geom_point(aes(x = date, y = median_R_mean, colour = data_type),
               data = data_ends, shape = 8) +
     geom_vline(xintercept = as.Date(c('2021-10-31', '2021-11-20')), linetype = 'dotdash', alpha = 0.6)  +
     annotate('rect',xmin = as.Date('2021-10-31'), xmax = as.Date('2021-11-20'), ymin = 0, ymax = Inf ,
              fill = 'grey', alpha = 0.4) +
    scale_colour_manual(values = c(viridis(5)[c(1, 4)], '#CFE11CFF', '#2E6E8EFF'), #'lightseagreen'
                        labels = c('Wastewater (v3.1)', 'Confirmed cases (Catchment)', paste0(i18n$t('Confirmed cases'), ' (Fribourg)'),
                                   paste0(i18n$t('Confirmed cases'), ' (Bern)')),
                        breaks = c('Wastewater', 'Confirmed (Catchment)','Confirmed (Fribourg)',
                                   'Confirmed (Bern)')) +
    scale_fill_manual(values = c(viridis(5)[c(1, 4)], '#CFE11CFF', '#2E6E8EFF'), #'lightseagreen'
                      labels = c('Wastewater (v3.1)', 'Confirmed cases (Catchment)', paste0(i18n$t('Confirmed cases'), ' (Fribourg)'),
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
           legend.position = 'bottom')  +
     annotate(geom = 'text',
              label = ifelse(is.infinite(min(data_ends$date)), " ",
                             ifelse(date_range[2] >= min(data_ends$date) , disc, " ")),
              x = summary(date_range)[['3rd Qu.']]-25, y = 0.1, hjust = 0.5, vjust = 1, size = 4)
   
  # would have to change once we are over the threshold
  if ('Wastewater' %in% source) {
    pmg <- plotDataRe %>% filter(region %in% canton) %>%
      filter(data_type == 'Wastewater (PMG2)') %>% filter(date >= date_range[1])
    
    
    if (range((ww_data_all %>% filter(region == canton) %>%
               select(date))[["date"]])[1]==date_range[1] & date_range[2]==Sys.Date()) { 
      ylimits <- c(0, 2) # default
    } else { 
      ylimits <- c(min(range(pmg$median_R_mean)[1], range(new_data$median_R_mean)[1]), 
                   max(range(pmg$median_R_mean)[1], 2))}
    
    
    pp <- pp +
      geom_line(data = pmg , 
                aes(x = date, y = median_R_mean), 
                linetype = 'dashed', colour = viridis(1), alpha = 0.6, lwd = 0.8) +
      geom_ribbon(data = pmg %>% filter(region==canton) ,
                  aes(x = date, ymin = median_R_lowHPD,
                      ymax = median_R_highHPD) , 
                  colour = NA, alpha = 0.1, show.legend = F, fill = viridis(1)) +
      # geom_line(data = pmg %>% filter(date>=(transition_period[2]-10)) , 
      #           aes(x = date, y = median_R_mean), 
      #           colour = viridis(1), alpha = 0.7, lwd = 0.8) +
      # geom_ribbon(data = pmg %>% filter(region==canton)%>% filter(date>=(transition_period[2]-10)),
      #             aes(x = date, ymin = median_R_lowHPD,
      #                 ymax = median_R_highHPD) , 
      #             colour = NA, alpha = 0.2, show.legend = F, fill = viridis(1)) +
      geom_point(aes(x = date, y = median_R_mean, colour = data_type),
                 data = pmg %>% filter(row_number()==n()), shape = 8, colour = viridis(1)) +
      geom_point(aes(x = date, y = median_R_mean, colour = data_type),
                 data = new_data %>% filter(data_type == 'Wastewater') %>% filter(row_number()==n()), 
                 shape = 4, colour = viridis(1)) +
      
      #scale_colour_manual(values = viridis(1))  +
      coord_cartesian(ylim = ylimits) 
  }
  

  pp
   
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
  } else {
    # French and Italian
    p1 <- strsplit(p1, " ")[[1]][2]
    ylabel <- bquote("R"['e']~.(p1)~" (95% CI)")
  }
  
  
  
  if (source == 'Wastewater') {
    CH_data <- plotDataRe %>% filter(region %in% canton) %>%
      filter(data_type %in% source) %>%
      bind_rows(plotDataRe %>% filter(region %in% canton) %>%
                  filter(data_type == 'Wastewater (PMG2)') %>%
                  filter(date > (transition_period[2] - 10))) %>% 
      filter(date >= date_range[1] & date <= date_range[2])
  } else {
    CH_data <- plotDataRe %>% filter(region %in% canton) %>%
      filter(data_type %in% source) %>% filter(date >= date_range[1] & date <= date_range[2])
  }
  
  
  pp <- CH_data %>%
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
          colour = i18n$t('Catchment area'), fill = i18n$t('Canton')) +
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
  
  if (source == 'Wastewater') {
    pp <- pp +
      geom_vline(xintercept = as.Date('2021-11-20'), linetype = 'dashed', colour = 'grey') 
      
  }
  
  pp
}


