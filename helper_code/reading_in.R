#### Reading in the data ####
# Zurich only first
library(tidyverse)
library(lubridate)
#library(patchwork) - not on server
library(viridis)
library(EpiEstim)
library(zoo)

# setting the basic plotting theme

theme_set(theme_minimal() +
            theme(
              strip.text = element_text(size=16),
              axis.text= element_text(size=14),
              axis.title =  element_text(size=16),
              legend.text= element_text(size=14),
              legend.title= element_text(size=16)
            ))

#### ZURICH ####
ZH_flow_url = "http://parsivel-eawag.ch/sarscov2/__data__/ARA%20Werdhoelzli_flow_cases.csv"
ZH_genes_url = "http://parsivel-eawag.ch/sarscov2/__data__/ARA%20Werdhoelzli_genes.csv"

raw_flow_data_ZH <- read_delim(ZH_flow_url, delim = ';',
                               col_names = c('date', 'cases', 'cases_smooth', 
                                             'flow', 'n1_smooth', 'n2_smooth'),
                               col_types = cols(date = col_date(format = '')),
                               skip = 1) 

raw_gene_data_ZH <- read_delim(ZH_genes_url, delim = ';',
                               col_names = c('date', 'n1', 'n2'),
                               col_types = cols(date = col_date(format = '')),
                               skip = 1) 

# We select data from January 20th, because the sampling was different before
# Missing data is imputed with linear interpolation
# This is necessary because we don't expect 0 in between
# and otherwise our smoothing step makes the average too low
raw_data_ZH <- raw_flow_data_ZH %>%
  left_join(raw_gene_data_ZH, c('date')) %>%
  filter(!is.na(n1),
         date >= as_date("2021-01-20")) %>%
  mutate(orig_data = TRUE) %>%
  complete(date = seq.Date(min(date), max(date), by = 'days')) %>%
  mutate(across(where(is.numeric), ~ zoo::na.approx(.x, na.rm = F) )) %>%
  mutate(region = 'ZH')

# drop n2 and n2_smooth as it is no longer recorded from February 2021
raw_data_ZH <- raw_data_ZH %>%
  select(-n2, -n2_smooth)

# Plot `raw' WW data - Zurich #####
# Required to be displayed

# List of all raw ww plots.
all_raw_plots <- list()

all_raw_plots[["zh"]] <- ggplot(raw_data_ZH, aes(x=date, y = n1)) +
  geom_point(colour = 'blue') +
  geom_line(colour = 'black', linetype = 'dashed') +
  geom_smooth(method = 'loess', colour = 'black',
              method.args = list(span = 0.05, degree = 1) ) +
  labs(x = 'Date' , y='N1 load') +
  ggtitle("Wastewater Gene Copies in Zurich") 





