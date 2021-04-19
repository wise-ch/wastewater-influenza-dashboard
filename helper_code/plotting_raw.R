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


date_range <- range(raw_data[["ZH"]][["date"]], raw_data[["VD"]][["date"]])


#### ZURICH ####
#### Wastewater raw  - Zurich ####
par(mar=c(5,6,4,2)+0.1,mgp=c(5,1,0))
all_raw_plots[["ZH"]] <- ggplot(raw_data_ZH, aes(x=date, y = n1)) +
  geom_point(colour = "#440154FF") +
  scale_x_date(limits = c(date_range[1], date_range[2])) +
  geom_line(colour = "#440154FF", linetype = 'dashed') +
  labs(x = 'Date' , y='Gene copies per day') +
  ggtitle("SARS-CoV2-RNA copies in Zurich Wastewater")

# Lausanne ####

par(mar=c(5,6,4,2)+0.1,mgp=c(5,1,0))
all_raw_plots[["VD"]] <- ggplot(raw_data_VD, aes(x=date, y = n1)) +
  geom_point(colour = "#440154FF") +
  scale_x_date(limits = c(date_range[1], date_range[2])) +
  geom_line(colour = "#440154FF", linetype = 'dashed') +
  labs(x = 'Date' , y='Gene copies per day') +
  ggtitle("SARS-CoV2-RNA copies in Lausanne Wastewater")



