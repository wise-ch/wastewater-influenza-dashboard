#### Reading in and pre-processing the data ####
library(tidyverse)
library(lubridate)
library(zoo)

ww_data <- data.frame()

ww_read_in <- function(data_url, ww, region) {

  raw_data <- read_delim(data_url, delim = ";",
                         col_names = c('date', 'n1', 'n1_smooth', 'cases', 
                                       'cases_smooth', 'quantification_flag', 'flow'),
                         col_types = cols(date = col_date(format = '')),
                         skip = 1)
  
  new_ww_data <- raw_data %>% filter(!is.na(n1),
                                     date >= as_date("2020-10-01")) %>%
    mutate(orig_data = TRUE) %>%
    complete(date = seq.Date(min(date), max(date), by = 'days')) %>%
    mutate(across(where(is.numeric), ~ zoo::na.approx(.x, na.rm = F) )) %>%
    mutate(region = region) %>% 
    mutate(norm_n1 = n1/min(n1)) %>%# normalise! 
    mutate(name_orig = ifelse(!is.na(orig_data), 'N1', 'Imputed'))
  
  ww_data <- bind_rows(ww, new_ww_data)
}


#### New raw data format: Read in --------

ZH_url = "https://sensors-eawag.ch/sars/__data__/processed_normed_data_zurich.csv"
ww_data <- ww_read_in(ZH_url, ww_data, "ZH") # what's different? The rna values seem different

VD_url = "https://sensors-eawag.ch/sars/__data__/processed_normed_data_lausanne.csv"
ww_data <- ww_read_in(VD_url, ww_data, "VD") # what's different? The rna values seem different

SG_url = "https://sensors-eawag.ch/sars/__data__/processed_normed_data_altenrhein.csv"
ww_data <- ww_read_in(SG_url, ww_data, "SG")

GR_url = "https://sensors-eawag.ch/sars/__data__/processed_normed_data_chur.csv"
ww_data <- ww_read_in(GR_url, ww_data, "GR")

FR_url = "https://sensors-eawag.ch/sars/__data__/processed_normed_data_chur.csv"
ww_data <- ww_read_in(FR_url, ww_data, "FR")

TI_url = "https://sensors-eawag.ch/sars/__data__/processed_normed_data_lugano.csv"
ww_data <- ww_read_in(TI_url, ww_data, "TI")


