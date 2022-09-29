#' Read in COVID Re and data from wastewater
load_covid_ww_all <- function() {
  results <- list("re" = load_covid_ww_re(), "data" = load_covid_ww_data())
  return(results)
}

#' Read in COVID Re based on wastewater
#' @return Data frame with columns: region, pathogen_type, data_type, date, median_R_mean, median_R_highHPD, median_R_lowHPD
load_covid_ww_re <- function(
    path_to_data = "rww_data/Rww_cantonal.csv", 
    path_to_pmg_data = "rww_data/Rww_cantonal_pmg.csv"
) {
  Re_ww_needed <- read_csv(path_to_data)
  Re_ww_needed_pmg <- read_csv(path_to_pmg_data)  # estimates after switching to promega kit
  
  covid_ww_re <- Re_ww_needed %>% 
    mutate(data_type = factor(data_type), protocol = "v3.1") %>%
    mutate(data_type = recode_factor(data_type, "infection_norm_n1" = "Wastewater")) %>%
    bind_rows(Re_ww_needed_pmg %>% mutate(data_type = 'Wastewater', protocol = "PMG2")) %>%
    mutate(pathogen_type = "COVID")
  
  return(covid_ww_re)  # formerly called Re_cc_needed
}

#' Read in data on COVID from wastewater and from confirmed cases in the canton
#' @return Data frames with columns: region, pathogen_type, data_type, date, observation, (optional: observation_smooth, orig_data, protocol_status)
load_covid_ww_data <- function() {
  ww_data <- data.frame()
  ww_data_new <- data.frame()
  case_data <- data.frame()

  regions <- c('ZH', 'GE','SG', 'GR', 'FR','TI')
  ref <- c("ZH"="Zurich" ,  "GE"="Geneva",
           "SG"="Altenrhein", "GR"="Chur",
           "FR"="Laupen", "TI"="Lugano")
  
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
    case_data <- bind_rows(case_data, all_data[["case"]])
  }
  
  ww_data_sars_cov_2 <- ww_data %>% mutate(protocol = 'v3.1') %>%
    bind_rows(ww_data_new %>% mutate(protocol = 'PMG2')) %>%
    pivot_longer(cols = "n1", names_to = "measurement_type", values_to = "measurement")
  
  # Post-processing to adapt format from legacy code
  covid_ww_data <- ww_data_sars_cov_2 %>%
    mutate(pathogen_type = "COVID") %>%
    mutate(protocol_status = case_when(protocol == "PMG2" ~ "latest", T ~ "older")) %>%
    rename(data_type = measurement_type, observation = measurement) %>%
    select(region, pathogen_type, data_type, date, observation, orig_data, protocol_status, protocol, quantification_flag)
  
  covid_cantonal_data <- case_data %>%
    mutate(pathogen_type = "COVID", data_type = "Confirmed (Canton)") %>%
    rename(observation = cases, observation_smooth = cases_smooth)
  
  return(list("ww" = covid_ww_data, "cantonal_cases" = covid_cantonal_data))
}

#' Helper function to download and transform wastewater load data for COVID
ww_read_in <- function(data_url, region) {
  
  raw_data <- read_delim(data_url, delim = ";",
                         col_names = c('date', 'n1', 'n1_smooth', 'cases', 
                                       'cases_smooth', 'quantification_flag', 'flow'),
                         col_types = cols(date = col_date(format = '')),
                         skip = 1) %>% mutate(flow = as.numeric(flow))
  
  new_case_data <- raw_data %>% select(date, cases, cases_smooth) %>% 
    complete(date = seq.Date(min(date), max(date), by = 'days')) %>% # 
    mutate(region = region)
  
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
