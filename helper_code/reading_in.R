#### Reading in and pre-processing the data ####
library(tidyverse)
library(lubridate)
library(zoo)

ww_data <- data.frame()
case_data <- data.frame()

ww_read_in <- function(data_url, region) {

  raw_data <- read_delim(data_url, delim = ";",
                         col_names = c('date', 'n1', 'n1_smooth', 'cases', 
                                       'cases_smooth', 'quantification_flag', 'flow'),
                         col_types = cols(date = col_date(format = '')),
                         skip = 1)
  
  new_case_data <- raw_data %>% select(date, cases, cases_smooth) %>% 
                    complete(date = seq.Date(min(date), max(date), by = 'days')) %>% # 
                     mutate(region = region) #%>% # no interpolation 
                      #mutate(cases = replace_na(cases , 0))
                    
  
  new_ww_data <- raw_data %>% filter(!is.na(n1)) %>%
    mutate(orig_data = TRUE) %>%
    complete(date = seq.Date(min(date), max(date), by = 'days')) %>% # so we lose out on the newer cases...
    mutate(across(where(is.numeric), ~ zoo::na.approx(.x, na.rm = F) )) %>%
    mutate(region = region) %>% 
    mutate(norm_n1 = n1/min(n1)) %>%# normalise! 
    mutate(name_orig = ifelse(!is.na(orig_data), 'N1', 'Imputed')) %>% 
    select(-cases, -cases_smooth) %>%
    mutate(quantification_flag = replace_na(quantification_flag, "Imputed")) %>%
    mutate(quantification_flag = recode(quantification_flag, Q = '> LOQ', D = '> LOD', N = '< LOD'))
  
  if (region == "TI" | region == "GR") {
    new_ww_data <- new_ww_data %>%
      filter(date >= as.Date('2021-03-08'))
  }
  
  list(ww = new_ww_data, case = new_case_data)
}


#### New raw data format: Read in --------

ZH_url = "https://sensors-eawag.ch/sars/__data__/processed_normed_data_zurich.csv"
all_data <- ww_read_in(ZH_url, "ZH") # what's different? The rna values seem different
ww_data <- bind_rows(ww_data, all_data[["ww"]])
case_data <- bind_rows(case_data, all_data[["case"]])

VD_url = "https://sensors-eawag.ch/sars/__data__/processed_normed_data_lausanne.csv"
all_data <- ww_read_in(VD_url, "VD")
ww_data <- bind_rows(ww_data, all_data[["ww"]])
case_data <- bind_rows(case_data, all_data[["case"]])

SG_url = "https://sensors-eawag.ch/sars/__data__/processed_normed_data_altenrhein.csv"
all_data <- ww_read_in(SG_url, "SG")
ww_data <- bind_rows(ww_data, all_data[["ww"]])
case_data <- bind_rows(case_data, all_data[["case"]])

GR_url = "https://sensors-eawag.ch/sars/__data__/processed_normed_data_chur.csv"
all_data <- ww_read_in(GR_url, "GR")
ww_data <- bind_rows(ww_data, all_data[["ww"]])
case_data <- bind_rows(case_data, all_data[["case"]])

FR_url = "https://sensors-eawag.ch/sars/__data__/processed_normed_data_laupen.csv"
all_data <- ww_read_in(FR_url,  "FR")
ww_data <- bind_rows(ww_data, all_data[["ww"]])
case_data <- bind_rows(case_data, all_data[["case"]])

TI_url = "https://sensors-eawag.ch/sars/__data__/processed_normed_data_lugano.csv"
all_data <- ww_read_in(TI_url,"TI")
ww_data <- bind_rows(ww_data, all_data[["ww"]])
case_data <- bind_rows(case_data, all_data[["case"]])

