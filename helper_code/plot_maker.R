# plots ####
library(tidyverse)
library(lubridate)
library(viridis)
library(zoo)
library(RColorBrewer)

# Reading in data: each helper script has function(s) to read in data set(s) ####
# Columns in Re data sets must be: region, pathogen_type, data_type, date, median_R_mean, median_R_highHPD, median_R_lowHPD, (optional: protocol)
# Columns in observation and WW data sets must be: region, pathogen_type, data_type, date, observation, (optional: observation_smooth, orig_data, protocol_status, protocol, quantification_flag)

source("helper_code/reading_in/reading_in_flu_ili.R")
source("helper_code/reading_in/reading_in_flu_ww.R")
source("helper_code/reading_in/reading_in_covid_ww.R")
source("helper_code/reading_in/reading_in_covid_cases.R")

flu_ili <- load_flu_ili_all()
flu_ww <- load_flu_ww_all()
covid_ww <- load_covid_ww_all()
covid_catchment_cases_re <- load_covid_catchment_cases_re()
covid_cantonal_cases_re <- load_covid_cantonal_cases_re()

# Binding the Re estimates for all diseases from all data types ####
plotDataRe <- covid_cantonal_cases_re %>%
  bind_rows(covid_catchment_cases_re) %>%
  bind_rows(covid_ww$re) %>%
  bind_rows(flu_ili$re) %>%
  bind_rows(flu_ww$re)

# Binding the observation data for all diseases from all data types ####
plotDataObs <- covid_ww$data$cantonal_cases %>%
  bind_rows(flu_ili$data)

# Binding the wastewater concentration data for all diseases from all data types ####
plotDataWW <- bind_rows(covid_ww$data$ww) %>%
  bind_rows(flu_ww$data)

# Data type colors
data_type_colors <- viridis(length(unique(plotDataRe$data_type)))
names(data_type_colors) <- unique(plotDataRe$data_type)

# Canton colors
canton_colors <- RColorBrewer::brewer.pal(name = "Dark2", n = 7)
canton_labels <- c('Zurich', 'Geneva', 'Basel', 'Altenrhein', 'Chur', 'Laupen', 'Lugano')
canton_breaks <- c('ZH', 'GE', 'BS', 'SG', 'GR', 'FR', 'TI')

# WW quantification protocol alpha levels
protocol_alphas <- c(0.6, 0.8)
names(protocol_alphas) <- c("v3.1", "PMG2")

# Catchment region/canton abbreviations to WWTP name
canton_to_catchment <- c(
  "ZH"="Zurich" ,
  "GE"="Geneva",
  "BS" = "Basel",
  "SG"="Altenrhein",
  "GR"="Chur",
  "FR"="Laupen",
  "TI"="Lugano"
)

# Catchment region/canton abbreviations to cantons to show in Re plot
catchment_to_cantons <- list(
  "ZH" = "ZH",
  "GE" = "GE",
  "BS" = "BS",
  "SG" = "SG",
  "GR" = "GR",
  "FR" = c("FR", "BE"),  # WWTP covers communities in both cantons
  "TI" = "TI"
)

# Approximate number of people in each catchment area
catchment_sizes <- c(
  "ZH"="471'000",
  "GE"="454'000",
  "BS" = "260'000",
  "SG"="64'000",
  "GR"="55'000",
  "FR"="62'000",
  "TI"="124'000"
)

#' Generate main plot title for appropriate i18n
get_main_title <- function(i18n, canton) {
  p1 <- i18n$t("Catchment area")
  p2 <- i18n$t("residents): Cases, Gene copies in Wastewater,")
  p3 <- i18n$t("Estimated R")
  if (nchar(p3)>10) {
    # English and German
    main_title <-bquote(.(canton_to_catchment[[canton]])~.(p1)~"("~.(catchment_sizes[[canton]])~.(p2)~.(p3)['e'])
  }
  else {
    # French and Italian
    p3 <- strsplit(p3, " ")[[1]][2]
    main_title <-bquote(.(canton_to_catchment[[canton]])~.(p1)~"("~.(catchment_sizes[[canton]])~.(p2)~"R"['e']~.(p3))
  }
  return(main_title)
}

#' Generate WW data plot y-axis label for appropriate i18n
get_ww_label <- function(i18n) {
  p1 <- i18n$t("Gene copies")
  ww_ylabel <- bquote(.(p1)*" ("%*%"10"^12*")")
  return(ww_ylabel)
}

#' Generate Re plot y-axis label for appropriate i18n
get_re_label <- function(i18n) {
  p1 <- i18n$t("Estimated R")
  if (nchar(p1)>10) {
    # English and German
    re_ylabel <- bquote(.(p1)['e']~" (95% CI)")
  } else {
    # French and Italian
    p1 <- strsplit(p1, " ")[[1]][2]
    re_ylabel <- bquote("R"['e']~.(p1)~" (95% CI)")
  }
  return(re_ylabel)
}

# Define shared theme for plots
shared_theme <- theme(strip.text = element_text(size=17),
      axis.text= element_text(size=14),
      axis.title =  element_text(size=16),
      legend.text= element_text(size=14),
      legend.title= element_text(size=17),
      plot.title = element_text(size = 16),
      panel.spacing.y = unit(2, "lines"),
      legend.position = 'bottom')

#' Plot observation data
case_plotter <- function(data = plotDataObs, canton, pathogen, date_range, i18n = NA) {

  data_filtered <- data %>%
    filter(pathogen_type == pathogen) %>%
    filter(region == canton) %>%
    filter(date >= date_range[1] & date <= date_range[2])
  
  p <- ggplot(data = data_filtered, aes(x = date, y = observation)) +
    geom_bar(aes(colour = data_type, fill = data_type),
             stat = "identity", alpha = 0.7) +
    scale_x_date(limits = c(date_range[1], date_range[2]),
                 date_breaks = "months", date_labels = "%b") +
    scale_y_continuous(labels = function(label) sprintf('%6.1f', label)) +
    scale_color_manual(values = data_type_colors, 
                       aesthetics = c("color", "fill"), 
                       breaks = unique(data_filtered$data_type)) +
    labs(x = i18n$t("Date"), y = i18n$t("Observations"),
         colour = i18n$t('Source'), fill = i18n$t('Source')) +
    ggtitle(get_main_title(i18n, canton)) +
    theme_minimal() +
    shared_theme
}

#' Plot raw wastewater measurements
raw_plotter <- function(data = plotDataWW, canton, pathogen, date_range, i18n = NA) {

  data_filtered <- data %>%
    mutate(observation = observation/10^12) %>%
    filter(region == canton) %>%
    filter(pathogen_type == pathogen)  %>%
    filter(date >= date_range[1] & date <= date_range[2])
  
  p <- ggplot() +  
    geom_point(data = data_filtered %>% filter(orig_data), 
               aes(x = date, y = observation, alpha = protocol), color = data_type_colors['Wastewater']) +
    geom_line(data = data_filtered %>% filter(!is.na(observation)),
              aes(x = date, y= observation, colour = 'black', alpha = protocol), linetype = 'dashed', colour = "black") +
    scale_x_date(limits = c(date_range[1], date_range[2]),
                 date_breaks = "months", date_labels = "%b") +
    scale_y_continuous(labels = function(label) sprintf('%4.1f', label)) +
    scale_alpha_manual(values = protocol_alphas, na.value = 0.7) +
    labs(x = i18n$t("Date"), y = get_ww_label(i18n)) +
    guides(alpha = "none") +
    theme_minimal() +
    shared_theme
  
  p
}

#' Plot Re estimates
re_plotter <- function(data = plotDataRe, source, canton, pathogen, date_range, i18n = NA) {
  
  data_filtered <- data %>% 
    filter(region %in% catchment_to_cantons[[canton]]) %>%
    filter(pathogen_type == pathogen) %>%
    filter(data_type %in% source) %>%
    filter(date >= date_range[1] & date <= date_range[2])
  
  ylimits <- c(0, 2)  # TODO: re-implement reactive y limits
  
  p <- ggplot(data = data_filtered) +
    geom_line(aes(x = date, y = median_R_mean, colour = data_type, linetype = region, alpha = protocol),
              lwd = 0.8) +
    geom_ribbon(aes(x = date, ymin = median_R_lowHPD,
                    ymax = median_R_highHPD, fill = data_type, linetype = region, alpha = protocol),
                show.legend = F) +
    geom_hline(yintercept = 1) +
    scale_color_manual(values = data_type_colors, aesthetics = c("color", "fill"), breaks = source) +
    scale_alpha_manual(values = protocol_alphas, na.value = 0.7) +
    scale_x_date(limits = c(date_range[1], date_range[2]),
                 date_breaks = "months", date_labels = "%b") +
    scale_y_continuous(labels = function(label) sprintf('%6.1f', label)) +
    coord_cartesian(ylim = ylimits) + # change this? Autoadjust? but how?
    labs(x = i18n$t("Date"), y = get_re_label(i18n),
        colour = i18n$t('Source'), fill = i18n$t('Source'), linetype = "Canton", alpha = "Protocol") +
    theme_minimal() +
    shared_theme
  
  # Remove linetype legend unless catchment encompasses multiple cantons (like Laupen)
  if (length(catchment_to_cantons[[canton]]) == 1) {
    p <- p + guides(linetype = "none")
  }
  # Remove alpha legend unless protocol changed
  if (length(unique(data_filtered$protocol)) == 1) {
   p <- p + guides(alpha = "none") 
  }
  
  p
}

# Plotting for all plants --------------

canton_plotter <- function(source, canton, pathogen, date_range, i18n = NA) {
  
  data_filtered <- plotDataRe %>% 
    filter(region %in% canton) %>%
    filter(pathogen_type == pathogen) %>%
    filter(data_type %in% source) %>%
    filter(date >= date_range[1] & date <= date_range[2])
  
  ylimits <- c(0, 2)  # TODO: re-implement reactive y limits
  
  p <- ggplot(data = data_filtered) +
    geom_line(aes(x = date, y = median_R_mean, colour = region,
              alpha = protocol), lwd = 0.8) +
    geom_ribbon(aes(x = date, ymin = median_R_lowHPD,
                    ymax = median_R_highHPD, fill = region,
                alpha = protocol)) +
    geom_hline(yintercept = 1) +
    scale_color_manual(values = canton_colors,
                       labels = canton_labels,
                       breaks = canton_breaks,
                       aesthetics = c("color", "fill")) +
    scale_alpha_manual(values = protocol_alphas - 0.5, na.value = 0.2) +  # reduce alpha for busier plots
    scale_x_date(limits = c(date_range[1], date_range[2]),
                 date_breaks = "months", date_labels = "%b") +
    coord_cartesian(ylim = ylimits) +
    labs( x = i18n$t('Date'), y = get_re_label(i18n),
          fill = i18n$t('Catchment area'), alpha = "Protocol") +
    theme_minimal() +
    shared_theme +
    guides(color = "none")
  
  # Remove alpha legend unless protocol changed
  if (length(unique(data_filtered$protocol)) == 1) {
    p <- p + guides(alpha = "none") 
  }
  
  p
}
