# This script is to make plots for the shiny app

library(ggplot2)
library(dplyr)
library(readr)
library(RColorBrewer)
library(zoo)

# Read in latest data
confirmed_cases <- read_csv("data/confirmed_cases.csv", col_types = cols(date = "D"))
ww_loads <- read_csv("data/ww_loads.csv", col_types = cols(sample_date = "D"))  # path relative to app directory
ww_re_estimates <- read_csv("data/ww_re_estimates.csv", col_types = cols(date = "D"))
case_re_estimates <- read_csv("data/confirmed_case_re_estimates.csv", col_types = c(date = "D"))

# Read in cached data, merge to latest data
for (file in list.files(path = "data/cached_data", full.names = T)) {
  print(paste("Reading in", file))
  if (endsWith(file, "_confirmed_cases.csv")) {
    confirmed_cases <- rbind(confirmed_cases, read_csv(file, col_types = cols(date = "D")))
  } else if (endsWith(file, "_ww_loads.csv")) {
    ww_loads <- rbind(ww_loads, read_csv(file, col_types = cols(sample_date = "D")))
  } else if (endsWith(file, "_confirmed_case_re_estimates.csv")) {
    case_re_estimates <- rbind(case_re_estimates, read_csv(file, col_types = cols(date = "D")))
  } else if (endsWith(file, "_ww_re_estimates.csv")) {
    ww_re_estimates <- rbind(ww_re_estimates, read_csv(file, col_types = cols(date = "D")))
  } else {
    warning(paste("Unknown cached data file", file, "ignored."))
  }
}

# Clean case data
confirmed_cases <- confirmed_cases %>%
  mutate(wwtp = recode(
    wwtp,
    "BASEL" = "ARA Basel",
    "ZUERICH(WERDHOELZLI)" = "ARA Werdhölzli Zurich",
    "VERNIER/AIRE" = "STEP Aire Geneva",
    "BIOGGIO(LUGANO)" = "IDA CDA Lugano",
    "CHUR" = "ARA Chur",
    "LAUPEN(SENSETAL)" = "ARA Sensetal Laupen",
    "THAL/ALTENRHEIN" = "ARA Altenrhein"
  )) %>%
  mutate(influenza_type = recode(
    influenza_type,
    "A" = "Influenza A virus",
    "B" = "Influenza B virus"
  )) %>%  # fill gaps due to interpolation
  mutate(
    data_type = "Confirmed cases",
    wwtp = zoo::na.locf(wwtp),
    influenza_type = zoo::na.locf(influenza_type),
    measuring_period = zoo::na.locf(measuring_period)) %>%
  mutate(dummy_year = case_when(
    format(date, "%m") %in% c("08", "09", "10", "11", "12") ~ "1999",
    T ~ "2000"
  )) %>%  # start each season from Sept for plotting
  mutate(date_to_plot = as.Date(paste0(dummy_year, format(date, "-%m-%d")))) %>%
  mutate(data_type = factor(data_type, levels = c("Wastewater", "Confirmed cases")))

# Clean load data
ww_loads <- ww_loads %>%
  mutate(wwtp = recode(
    wwtp,
    "STEP Aire" = "STEP Aire Geneva",
    "ARA Werdhölzli" = "ARA Werdhölzli Zurich",
    "ARA Sensetal" = "ARA Sensetal Laupen",
    "CDA Lugano" = "IDA CDA Lugano"
  )) %>%  # fill gaps due to interpolation
  mutate(
    data_type = "Wastewater",
    wwtp = zoo::na.locf(wwtp),
    influenza_type = zoo::na.locf(influenza_type),
    measuring_period = zoo::na.locf(measuring_period)) %>%
  mutate(influenza_type = recode(
    influenza_type,
    "IAV" = "Influenza A virus",
    "IBV" = "Influenza B virus"
  )) %>%
  mutate(dummy_year = case_when(
    format(sample_date, "%m") %in% c("09", "10", "11", "12") ~ "1999",
    T ~ "2000"
  )) %>%  # start each season from Sept for plotting
  mutate(date_to_plot = as.Date(paste0(dummy_year, format(sample_date, "-%m-%d")))) %>%
  mutate(data_type = factor(data_type, levels = c("Wastewater", "Confirmed cases")))

# Join Re data
re_to_plot <- bind_rows(
  case_re_estimates %>%
    mutate(wwtp = recode(
      observation_type,
      "BASEL" = "ARA Basel",
      "ZUERICH(WERDHOELZLI)" = "ARA Werdhölzli Zurich",
      "VERNIER/AIRE" = "STEP Aire Geneva",
      "BIOGGIO(LUGANO)" = "IDA CDA Lugano",
      "CHUR" = "ARA Chur",
      "LAUPEN(SENSETAL)" = "ARA Sensetal Laupen",
      "THAL/ALTENRHEIN" = "ARA Altenrhein"
    )) %>%
    mutate(influenza_type = recode(
      influenza_type,
      "A" = "Influenza A virus",
      "B" = "Influenza B virus"
    )) %>%
    mutate(CI_down_Re_estimate = pmin(CI_down_Re_estimate, Re_lowHPD, na.rm = T),
           CI_up_Re_estimate = pmax(CI_up_Re_estimate, Re_highHPD, na.rm = T)) %>%  # this is to fix a bug(?) around the confidence intervals for the first weeks/months in Lugano, Laupen, and Chur, they're NA for combined uncertainty even though non-NA for Re_low/highHPD
    mutate(data_type = "Confirmed cases"),
  ww_re_estimates %>%
    mutate(wwtp = recode(
      observation_type,
      "STEP Aire" = "STEP Aire Geneva",
      "ARA Werdhölzli" = "ARA Werdhölzli Zurich",
      "ARA Sensetal" = "ARA Sensetal Laupen",
      "CDA Lugano" = "IDA CDA Lugano"
    )) %>%
    mutate(influenza_type = recode(
      influenza_type,
      "IAV" = "Influenza A virus",
      "IBV" = "Influenza B virus"
    )) %>%
    mutate(data_type = "Wastewater")
) %>%
  mutate(dummy_year = case_when(
    format(date, "%m") %in% c("09", "10", "11", "12") ~ "1999",
    T ~ "2000"
  )) %>%  # start each season from Sept for plotting
  mutate(date_to_plot = as.Date(paste0(dummy_year, format(date, "-%m-%d")))) %>%
  mutate(data_type = factor(data_type, levels = c("Wastewater", "Confirmed cases")))

# Data type colors
all_seasons <- unique(c(confirmed_cases$measuring_period, ww_loads$measuring_period))
all_sources <- c("Confirmed cases", "Wastewater")
all_levels <- levels(interaction(all_sources, all_seasons, sep=" ", lex.order = T))
all_levels_verbose <- levels(interaction(c("Laboratory-confirmed cases", "Virus load in wastewater"), all_seasons, sep=" ", lex.order = T))

confirmed_cases <- confirmed_cases %>% mutate(color_code = factor(paste(data_type,measuring_period,sep=" "), levels = all_levels))
ww_loads <- ww_loads %>% mutate(color_code = factor(paste(data_type,measuring_period,sep=" "), levels = all_levels))
re_to_plot <- re_to_plot %>% mutate(color_code = factor(paste(data_type,measuring_period,sep=" "), levels = all_levels))

color_codes <- c(
  rgb(0.9,seq(0.6,0.1,length.out=length(all_seasons)),0.1), # confirmed cases, all seasons
  rgb(0.1,seq(0.6,0.1,length.out=length(all_seasons)),0.9) # wastewater, all seasons
  )

#data_type_colors <- brewer.pal(n = 8, name = "Set1")[1:2]
#names(data_type_colors) <- c("Confirmed cases", "Wastewater")
data_type_color_scale <- scale_color_manual(
  values = color_codes,
  aesthetics = c("color", "fill"),
  labels = all_levels_verbose,
  drop = F
)

# WWTP names to display
wwtp_levels <- c("ARA Werdhölzli Zurich", "STEP Aire Geneva", "ARA Basel", "IDA CDA Lugano", "ARA Chur", "ARA Sensetal Laupen", "ARA Altenrhein", "All provided data")
wwtp_labels <- c("Zurich", "Geneva", "Basel", "Lugano", "Chur", "Laupen", "Altenrhein", "All\ncatchments")
re_to_plot <- re_to_plot %>% mutate(wwtp_factor = factor(wwtp, levels = wwtp_levels, labels = wwtp_labels))

# y-axis limits for Re plots
re_ylimits <- c(0, 2)

# Define shared theme for plots
shared_theme <- theme_minimal() +
  theme(
    strip.text = element_text(size = 17, hjust = 0),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 17),
    plot.title = element_text(size = 16),
    panel.spacing.y = unit(2, "lines"),
    panel.spacing.x = unit(2, "lines"),
    legend.position = "none"
  )

shared_date_scale <- scale_x_date(
  date_breaks = "months", date_labels = "%b",
  limits = c(as.Date("1999-07-01"), as.Date("2000-06-30")),
  expand = c(0, 0))

ma_legend_inset <- cowplot::plot_grid(
  cowplot::get_legend(data.frame(x = rnorm(10), y = rnorm(10)) %>%
                        ggplot(aes(x = x, y = y)) +
                        geom_point(shape = 21, size = 2, aes(color = "Measurements")) +
                        scale_color_manual(values = c("black","black")) +
                        theme_bw() +
                        theme(
                          legend.title = element_blank(),
                          legend.box.margin = margin(0, 0, 0, 0),
                          legend.margin = margin(0, 0, 0, 0),
                          legend.position = c(0, 0.5),
                          plot.margin = margin(0, 0, 0, 0),
                          legend.background = element_blank(),
                          legend.key = element_blank()
                        ))
  ,
  cowplot::get_legend(data.frame(x = rnorm(10), y = rnorm(10)) %>%
                        ggplot(aes(x = x, y = y)) +
                        geom_line(aes(color = "7-day moving average")) +
                        scale_color_manual(values = c("black","black")) +
                        theme_bw() +
                        theme(
                          legend.title = element_blank(),
                          legend.box.margin = margin(0, 0, 0, 0),
                          legend.margin = margin(0, 0, 0, 0),
                          legend.position = c(0, 0.5),
                          plot.margin = margin(0, 0, 0, 0),
                          legend.background = element_blank(),
                          legend.key = element_blank()
                        )), 
  NULL,
  align = "v", ncol=1, rel_heights = c(1,1,5))

#' Plot wastewater loads
plot_ww_loads <- function(data = ww_loads, wwtp_to_plot, measuring_periods) {

  data_filtered <- data %>%
    filter(wwtp == wwtp_to_plot) %>%
    filter(measuring_period %in% measuring_periods) %>% 
    group_by(influenza_type) %>% 
    mutate(latest_date = max(sample_date, na.rm = T),
           influenza_type = paste0(influenza_type," (last update: ",latest_date,")"))
  
  p <- ggplot() +
    geom_line(
      data = data_filtered,
      aes(x = date_to_plot, y = zoo::rollmean(observation, 7, fill = NA), color = color_code),
      size = 1, alpha = 1, linetype = "solid"
    ) +
    geom_point(
      data = data_filtered %>% filter(is_observation),
      aes(x = date_to_plot, y = observation, color = color_code),
      size = 2, shape = 21
    ) +
    facet_grid(. ~ influenza_type) +
    scale_shape_discrete(name = "Influenza season") +
    shared_date_scale +
    data_type_color_scale +
    scale_y_continuous(labels = function(label) sprintf("%4.1f", label)) +
    labs(x = element_blank(), y = "Genome copies per day\n(normalized by minimum value)") +
    shared_theme

  cowplot::ggdraw(p) +
    cowplot::draw_plot(ma_legend_inset, .15, .35, .5, .5) +
    cowplot::draw_plot(ma_legend_inset, .62, .35, .5, .5)
}

plot_cases <- function(data = confirmed_cases, wwtp_to_plot, measuring_periods) {
  data_filtered <- data %>%
    filter(is_observation) %>%
    filter(wwtp == wwtp_to_plot) %>%
    filter(measuring_period %in% measuring_periods) %>% 
    group_by(influenza_type) %>% 
    mutate(latest_date = max(date, na.rm = T),
           influenza_type = paste0(influenza_type," (last update: ",latest_date,")"))

  p <- ggplot() +
    geom_line(
      data = data_filtered,
      aes(x = date_to_plot, y = zoo::rollmedian(total_cases, 1, fill = NA), color = color_code),
      size = 1, alpha = 0.7
    ) +
    geom_point(
      data = data_filtered,
      aes(x = date_to_plot, y = total_cases, color = color_code),
      size = 2
    ) +
    facet_grid(. ~ influenza_type) +
    shared_date_scale +
    scale_shape_discrete(name = "Influenza season") +
    data_type_color_scale +
    scale_y_continuous(labels = function(label) sprintf("%4.1f", label)) +
    labs(x = element_blank(), y = "Weekly confirmed cases\nin the catchment") +
    shared_theme

  p
}

#' Plot Re estimates
plot_re <- function(data = re_to_plot, data_types, wwtp_to_plot, measuring_periods) {
  data_filtered <- data %>%
    filter(wwtp == wwtp_to_plot) %>%
    filter(data_type %in% data_types) %>%
    filter(measuring_period %in% measuring_periods)

  ylimits <- c(0, 2) # TODO: re-implement reactive y limits

  p <- ggplot(data = data_filtered) +
    geom_line(
      aes(x = date_to_plot, y = Re_estimate, color = color_code),
      lwd = 0.8) +
    geom_ribbon(
      aes(x = date_to_plot, ymin = CI_down_Re_estimate, ymax = CI_up_Re_estimate, fill = color_code),
      alpha = 0.5) +
    facet_grid(. ~ influenza_type) +
    geom_hline(yintercept = 1) +
    data_type_color_scale +
    shared_date_scale +
    scale_y_continuous(labels = function(label) sprintf("%6.1f", label)) +
    coord_cartesian(ylim = re_ylimits) +
    shared_theme +
    labs(
      x = element_blank(), y = "Reproductive number",
      colour = "Data source", fill = "Data source") +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(ncol = 2, byrow = F))

  p
}

# Plot Re for all catchments together
plot_all_re <- function(data = re_to_plot, data_types, measuring_period_to_plot) {
  data_filtered <- data %>%
    filter(data_type %in% data_types) %>%
    filter(measuring_period == measuring_period_to_plot)

  p <- ggplot(data = data_filtered) +
    geom_line(
      aes(x = date_to_plot, y = Re_estimate, color = color_code),
      lwd = 0.8) +
    geom_ribbon(
      aes(x = date_to_plot, ymin = CI_down_Re_estimate,
          ymax = CI_up_Re_estimate,
          fill = color_code),
      alpha = 0.5) +
    facet_grid(wwtp_factor ~ influenza_type, drop = F) +
    geom_hline(yintercept = 1) +
    data_type_color_scale +
    shared_date_scale +
    scale_y_continuous(labels = function(label) sprintf("%6.1f", label)) +
    coord_cartesian(ylim = re_ylimits) +
    labs(
      x = element_blank(), y = "Reproductive number",
      colour = "Data type", fill = "Data type"
    ) +
    shared_theme +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(ncol = 2, byrow = F))

  p
}
