#### Reading in and pre-processing the data ####
library(tidyverse)
library(lubridate)
library(zoo)

# Reading in wastewater data ####

ww_data <- data.frame()

ww_read_in <- function(data_url, ww, region) {
  # raw_flow_data <- read_delim(flow, delim = ';',
  #                                col_names = c('date', 'cases', 'cases_smooth', 
  #                                              'flow', 'n1_smooth', 'n2_smooth'),
  #                                col_types = cols(date = col_date(format = '')),
  #                                skip = 1) 
  # 
  # raw_gene_data <- read_delim(genes, delim = ';',
  #                                col_names = c('date', 'n1', 'n2'),
  #                                col_types = cols(date = col_date(format = '')),
  #                                skip = 1) 
  raw_data <- read_delim(data_url, delim = ";",
                         col_names = c('date', 'n1', 'n1_smooth', 'cases', 
                                       'cases_smooth', 'quantification_flag', 'flow'),
                         col_types = cols(date = col_date(format = '')),
                         skip = 1)
  
  new_ww_data <- raw_data %>% filter(!is.na(n1),
                                     date >= as_date("2020-09-01")) %>%
    mutate(orig_data = TRUE) %>%
    complete(date = seq.Date(min(date), max(date), by = 'days')) %>%
    mutate(across(where(is.numeric), ~ zoo::na.approx(.x, na.rm = F) )) %>%
    mutate(region = region) %>% 
    mutate(norm_n1 = n1/min(n1)) %>%# normalise! 
    mutate(name_orig = ifelse(!is.na(orig_data), 'N1', 'Imputed'))
  
  # new_ww_data <- raw_flow_data %>%
  #   left_join(raw_gene_data, c('date')) %>%
  #   filter(!is.na(n1),
  #          date >= as_date("2020-09-01")) %>%
  #   mutate(orig_data = TRUE) %>%
  #   complete(date = seq.Date(min(date), max(date), by = 'days')) %>%
  #   mutate(across(where(is.numeric), ~ zoo::na.approx(.x, na.rm = F) )) %>%
  #   mutate(region = region) %>% 
  #   select(-n2, -n2_smooth) %>%# drop n2, n2_smooth as no longer recorded from 02.21
  #   mutate(norm_n1 = n1/min(n1)) %>%# normalise! 
  #   mutate(name_orig = ifelse(!is.na(orig_data), 'N1', 'Imputed')) # identifies whether imputed or not
  # 
  ww_data <- bind_rows(ww, new_ww_data)
}

#ZH_flow_url = "http://parsivel-eawag.ch/sarscov2/__data__/ARA%20Werdhoelzli_flow_cases.csv"
#ZH_genes_url = "http://parsivel-eawag.ch/sarscov2/__data__/ARA%20Werdhoelzli_genes.csv"

#ww_data <- ww_read_in(ZH_flow_url, ZH_genes_url, ww_data, "ZH")

#VD_flow_url = "https://sensors-eawag.ch/sarscov2/__data__/STEP%20Vidy_flow_cases.csv"
#VD_genes_url = "http://sensors-eawag.ch/sarscov2/__data__/STEP%20Vidy_genes.csv"

#ww_data <- ww_read_in(VD_flow_url, VD_genes_url, ww_data, "VD")


#### New raw data format --------

ZH_url = "https://sensors-eawag.ch/sars/__data__/processed_normed_data_zurich.csv"
ww_data <- ww_read_in(ZH_url, ww_data, "ZH") # what's different? The rna values seem different
#View(read_delim(ZH_url, delim = ";"))

VD_url = "https://sensors-eawag.ch/sars/__data__/processed_normed_data_lausanne.csv"
ww_data <- ww_read_in(VD_url, ww_data, "VD") # what's different? The rna values seem different

