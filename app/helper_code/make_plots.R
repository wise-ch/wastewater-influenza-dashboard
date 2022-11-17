# This script is to make plots for the shiny app

library(readr)
library(RColorBrewer)

# Read in data
ww_loads <- read_csv("data/ww_loads.csv", col_types = cols(sample_date = "D"), )
ww_re_estimates <- read_csv("data/ww_re_estimates.csv", col_types = cols(date = "D"))
case_re_estimates <- read_csv("data/confirmed_case_re_estimates.csv", col_types = c(date = "D"))

# Clean load data
ww_loads <- ww_loads %>%
  mutate(wwtp = recode(
    wwtp,
    "STEP Aire" = "STEP Aire Geneva",
    "ARA Werdhölzli" = "ARA Werhölzli Zurich"
  ))

# Join Re data
re_to_plot <- bind_rows(
  case_re_estimates %>% 
    mutate(wwtp = recode(
      observation_type,
      "BASEL" = "ARA Basel",
      "ZUERICH(WERDHOELZLI)" = "ARA Werhölzli Zurich",
      "VERNIER/AIRE" = "STEP Aire Geneva",
      "BIOGGIO(LUGANO)" = "IDA CDA Lugano",
      "CHUR" = "ARA Chur",
      "LAUPEN(SENSETAL)" = "ARA Sensetal Laupen",
      "THAL/ALTENRHEIN" = "ARA Altenrhein"
    )) %>%
    mutate(influenza_type = recode(
      influenza_type,
      "A" = "IAV",
      "B" = "IBV"
    )) %>%
    mutate(data_type = "Confirmed cases"),
  ww_re_estimates %>%
    mutate(wwtp = recode(
      observation_type,
      "STEP Aire" = "STEP Aire Geneva",
      "ARA Werdhölzli" = "ARA Werhölzli Zurich"
    )) %>%
    mutate(data_type = "Wastewater")
)

# Data type colors
data_type_colors <- RColorBrewer::brewer.pal(name = "Set2", n = 3)[1:2]
names(data_type_colors) <- c("Confirmed cases", "Wastewater")

# WWTP colors
wwtp_colors <- RColorBrewer::brewer.pal(name = "Set2", n = 7)
wwtp_labels <- c("ARA Werhölzli Zurich", "STEP Aire Geneva", "ARA Basel", "IDA CDA Lugano", "ARA Chur", "ARA Sensetal Laupen", "ARA Altenrhein")
wwtp_breaks <- wwtp_labels

# Define shared theme for plots
shared_theme <- theme_minimal() +
  theme(
    strip.text = element_text(size = 17),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 17),
    plot.title = element_text(size = 16),
    panel.spacing.y = unit(2, "lines"),
    legend.position = "bottom"
  )

date_range <- range(ww_loads$sample_date)

#' Plot wastewater loads
plot_ww_loads <- function(data = ww_loads, wwtp_to_plot, date_range) {
  data_filtered <- data %>%
    filter(is_observation) %>%
    mutate(observation = genome_copies_per_day / 10^12) %>%
    filter(wwtp == wwtp_to_plot) %>%
    filter(sample_date >= date_range[1] & sample_date <= date_range[2])

  p <- ggplot() +
    geom_point(
      data = data_filtered %>% filter(is_observation),
      aes(x = sample_date, y = observation), color = data_type_colors["Wastewater"]
    ) +
    geom_line(
      data = data_filtered %>% filter(is_observation),
      aes(x = sample_date, y = observation), linetype = "dashed", colour = "black"
    ) +
    facet_grid(. ~ influenza_type) +
    scale_x_date(
      limits = c(date_range[1], date_range[2]),
      date_breaks = "months", date_labels = "%b"
    ) +
    scale_y_continuous(labels = function(label) sprintf("%4.1f", label)) +
    labs(x = "Date", y = "Genome copies per day (x10^12)") +
    shared_theme

  p
}

#' Plot Re estimates
plot_re <- function(data = re_to_plot, data_types, wwtp_to_plot, date_range) {
  data_filtered <- data %>%
    filter(wwtp == wwtp_to_plot) %>%
    filter(data_type %in% data_types) %>%
    filter(date >= date_range[1] & date <= date_range[2])

  ylimits <- c(0, 2) # TODO: re-implement reactive y limits

  p <- ggplot(data = data_filtered) +
    geom_line(aes(x = date, y = Re_estimate, colour = data_type), lwd = 0.8) +
    geom_ribbon(aes(x = date, ymin = CI_down_Re_estimate, ymax = CI_up_Re_estimate, fill = data_type)) +
    facet_grid(. ~ influenza_type) +
    geom_hline(yintercept = 1) +
    scale_color_manual(values = data_type_colors, aesthetics = c("color", "fill"), breaks = data_types) +
    scale_x_date(
      limits = c(date_range[1], date_range[2]),
      date_breaks = "months", date_labels = "%b"
    ) +
    scale_y_continuous(labels = function(label) sprintf("%6.1f", label)) +
    coord_cartesian(ylim = ylimits) + # change this? Autoadjust? but how?
    labs(
      x = "Date", y = "Reproductive number",
      colour = "Data type", fill = "Data type"
    ) +
    shared_theme

  p
}

# # Plotting for all plants --------------
# 
# canton_plotter <- function(source, canton, pathogen, date_range, i18n = NA) {
#   data_filtered <- plotDataRe %>%
#     filter(region %in% canton) %>%
#     filter(pathogen_type == pathogen) %>%
#     filter(data_type %in% source) %>%
#     filter(date >= date_range[1] & date <= date_range[2])
# 
#   ylimits <- c(0, 2) # TODO: re-implement reactive y limits
# 
#   p <- ggplot(data = data_filtered) +
#     geom_line(aes(
#       x = date, y = median_R_mean, colour = region,
#       alpha = protocol
#     ), lwd = 0.8) +
#     geom_ribbon(aes(
#       x = date, ymin = median_R_lowHPD,
#       ymax = median_R_highHPD, fill = region,
#       alpha = protocol
#     )) +
#     geom_hline(yintercept = 1) +
#     scale_color_manual(
#       values = canton_colors,
#       labels = canton_labels,
#       breaks = canton_breaks,
#       aesthetics = c("color", "fill")
#     ) +
#     scale_alpha_manual(values = protocol_alphas - 0.5, na.value = 0.2) + # reduce alpha for busier plots
#     scale_x_date(
#       limits = c(date_range[1], date_range[2]),
#       date_breaks = "months", date_labels = "%b"
#     ) +
#     coord_cartesian(ylim = ylimits) +
#     labs(
#       x = i18n$t("Date"), y = get_re_label(i18n),
#       fill = i18n$t("Catchment area"), alpha = "Protocol"
#     ) +
#     theme_minimal() +
#     shared_theme +
#     guides(color = "none")
# 
#   # Remove alpha legend unless protocol changed
#   if (length(unique(data_filtered$protocol)) == 1) {
#     p <- p + guides(alpha = "none")
#   }
# 
#   p
# }
