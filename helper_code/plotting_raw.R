#### Plotting the data ####
# Zurich only first
library(tidyverse)
library(lubridate)
#library(patchwork) - not on server
library(viridis)
library(EpiEstim)

source("helper_code/reading_in.R")

# colours

# The palette with grey: colour blind friendly
cbp1 <- c("#009E73", "#D55E00", "#0072B2", "#CC79A7", 
          "#E69F00", "#56B4E9", "#F0E442", "#999999") # green, dark orange, dark blue, pink

# List of all raw ww plots.
all_raw_plots <- list()

#### ZURICH ####
#### Wastewater raw  - Zurich ####
all_raw_plots[["zh"]] <- ggplot(raw_data_ZH, aes(x=date, y = n1)) +
  geom_point(colour = "#440154FF") +
  geom_line(colour = "#440154FF", linetype = 'dashed') +
  labs(x = 'Date' , y='Gene copies per day') +
  ggtitle("SARS-CoV2-RNA copies in Zurich Wastewater")


