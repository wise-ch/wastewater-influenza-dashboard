#### Plotting the data ####
# Zurich only first
library(tidyverse)
library(lubridate)
#library(patchwork) - not on server
library(viridis)
library(EpiEstim)

source("helper_code/reading_in.R")


# List of all raw ww plots.
all_raw_plots <- list()
all_re_plots <- list()

#### ZURICH ####
#### Wastewater raw  - Zurich ####
all_raw_plots[["zh"]] <- ggplot(raw_data_ZH, aes(x=date, y = n1)) +
  geom_point(colour = 'blue') +
  geom_line(colour = 'black', linetype = 'dashed') +
  geom_smooth(method = 'loess', colour = 'black',
              method.args = list(span = 0.05, degree = 1) ) +
  labs(x = 'Date' , y='Gene copies per day') +
  ggtitle("SARS-CoV2-RNA copies in Zurich Wastewater") +
  theme_light()


