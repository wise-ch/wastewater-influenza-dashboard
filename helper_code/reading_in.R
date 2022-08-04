#### Reading in and pre-processing the data ####
library(tidyverse)
library(lubridate)
library(zoo)

ww_data <- data.frame()
ww_data_new <- data.frame()
case_data <- data.frame()

ww_read_in <- function(data_url, region) {

  raw_data <- read_delim(data_url, delim = ";",
                         col_names = c('date', 'n1', 'n1_smooth', 'cases', 
                                       'cases_smooth', 'quantification_flag', 'flow'),
                         col_types = cols(date = col_date(format = '')),
                         skip = 1) %>% mutate(flow = as.numeric(flow))
  
  new_case_data <- raw_data %>% select(date, cases, cases_smooth) %>% 
                    complete(date = seq.Date(min(date), max(date), by = 'days')) %>% # 
                     mutate(region = region) #%>%  # no interpolation
                    #mutate(cases = ifelse(!is.na(cases), cases, 0))
  
  new_ww_data <- raw_data %>% filter(!is.na(n1)) %>%
    mutate(orig_data = TRUE) %>%
    complete(date = seq.Date(min(date), max(date), by = 'days')) %>% # so we lose out on the newer cases...
    mutate(across(where(is.numeric), ~ zoo::na.approx(.x, na.rm = F) )) %>%
    mutate(region = region) %>% 
    mutate(norm_n1 = n1/10^10) %>%# normalise! 
    mutate(name_orig = ifelse(!is.na(orig_data), 'N1', 'Imputed')) %>% 
    select(-cases, -cases_smooth) %>%
    mutate(quantification_flag = replace_na(quantification_flag, "Imputed")) %>%
    mutate(quantification_flag = recode(quantification_flag, Q = '> LOQ', D = '> LOD', N = '< LOD'))
  
  if (region == "TI" | region == "GR") {
    new_ww_data <- new_ww_data %>%
      filter(date >= as.Date('2021-03-08'))
  }
  
  # For Altenrhein, there were a few ww measurements in Nov to be ignored.
  if (region == "SG") {
    new_ww_data <- new_ww_data %>%
      filter(date >= as.Date('2021-02-01'))
  }
  
  if (region == "GE") {
    new_ww_data <- new_ww_data %>%
      filter(date >= as.Date('2021-08-01'))
  }
  
  list(ww = new_ww_data, case = new_case_data)
}


#### New raw data format: Read in --------

regions <- c('ZH', 'GE','SG', 'GR', 'FR','TI')

ref <- c("ZH"="Zurich" ,  "GE"="Geneva",
         "SG"="Altenrhein", "GR"="Chur",
         "FR"="Laupen", "TI"="Lugano")

# promega - Nov 10 onwards (day 1)
# v3 - Discontinued from Nov 30 onwards (last day)
# want a long df


# for (i in regions) {
#   url = paste0("https://sensors-eawag.ch/sars/__data__/processed_normed_data_",tolower(ref[[i]]),"_v1.csv")
#   if (i == "GE") {
#     url = paste0("https://sensors-eawag.ch/sars/__data__/processed_normed_data_geneve_v1.csv")
#   }
#   all_data <- ww_read_in(url, i)
#   ww_data <- bind_rows(ww_data, all_data[["ww"]])
#   case_data <- bind_rows(case_data, all_data[["case"]])
# }


for (i in regions) {
  url = paste0("https://sensors-eawag.ch/sars/__data__/processed_normed_data_",tolower(ref[[i]]),"_v1.csv")
  url2 = paste0("https://sensors-eawag.ch/sars/__data__/processed_normed_data_",tolower(ref[[i]]),"_v2.csv")

  if (i == "GE") {
    url = paste0("https://sensors-eawag.ch/sars/__data__/processed_normed_data_geneve_v1.csv")
    url2 = paste0("https://sensors-eawag.ch/sars/__data__/processed_normed_data_geneve_v2.csv")

  }
  all_data <- ww_read_in(url, i)
  all_data2 <- ww_read_in(url2, i)
  ww_data <- bind_rows(ww_data, all_data[["ww"]]) # the old protocol
  ww_data_new <- bind_rows(ww_data_new, all_data2[["ww"]]) # new protocol
  # case data in both v1 and v2
  case_data <- bind_rows(case_data, all_data[["case"]]) #%>% bind_rows(all_data2[["case"]]) %>% unique()
}

ww_data_all <- ww_data %>% mutate(protocol = 'v3.1') %>%
  bind_rows(ww_data_new %>% mutate(protocol = 'PMG2'))

# stitching the old and new 
ww_data_new <- ww_data %>% filter(date<min(ww_data_new$date)) %>% bind_rows(ww_data_new) %>% 
  group_by(region) %>% mutate(norm_n1 = n1/min(n1)) %>% ungroup()
# so still using the same normalisation values
