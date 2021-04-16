#### Plotting the data ####
# Zurich only first
library(tidyverse)
library(lubridate)
#library(patchwork) - not on server
library(viridis)
library(EpiEstim)

#source("helper_code/reading_in.R")
source("helper_code/processing.R") # takes time

# List of all raw ww plots.
all_re_plots <- list()

#### ZURICH ####
# singular plot
# singular_rww <- ggplot() +
#   geom_ribbon(data = Re_ww, aes(x = date, ymin = median_R_lowHPD,
#                                 ymax = median_R_highHPD, fill = data_type),
#               alpha = 0.2, show.legend = F) +
#   geom_line(data = Re_ww,
#             aes(x = date, y = median_R_mean, colour = data_type), 
#             alpha = 0.7, show.legend = F) +
#   scale_x_date(limits = c(as_date(min(Re_ww[["date"]])), 
#                           as_date(max(Re_ww[["date"]])))
#   ) +
#   labs( x = 'Date', y = 'Estimated Re', 
#         colour = 'Observation Type', fill = 'Observation Type') +
#   ggtitle("Estimated Wastewater Re in Zurich") +
#   theme_light()


all_re_plots[["zh"]] <- ggplot(plotData) +
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
  labs( x = 'Date', y = expression("Estimated R"["e"]),
        colour = 'Source', fill = 'Source') +
  guides(color = guide_legend(override.aes = list(size=5))) +  
  theme(
     panel.spacing.y = unit(2, "lines"),
     legend.position = 'bottom'
   ) + ggtitle(expression("Estimated R"["e"]*" in Zurich using different data sources"))
