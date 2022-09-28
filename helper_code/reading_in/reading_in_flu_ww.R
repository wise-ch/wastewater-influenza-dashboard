#' Read in influenza Re and data from IAV and IBV from wastewater
load_flu_ww_all <- function() {
  results <- list("re" = load_flu_ww_re(), "data" = load_flu_ww_data())
  return(results)
}

#' Read in IAV and IBV Re based on wastewater
#' @return Data frame with columns: region, pathogen_type, data_type, date, median_R_mean, median_R_highHPD, median_R_lowHPD
load_flu_ww_re <- function(path_to_data = "rww_data_flu/ww_re_estimates.csv") {
  flu_ww_re <- read_csv(path_to_data) %>%
    mutate(data_type = "Wastewater") %>%
    mutate(region = case_when(
      observation_type == "ARA Basel" ~ "BS",
      observation_type == "ARA Werdhölzli" ~ "ZH",
      observation_type == "STEP Aire" ~ "GE"
    )) %>%
    mutate(pathogen_type = influenza_type) %>%
    rename(median_R_mean = Re_estimate, median_R_highHPD = CI_up_Re_estimate, 
           median_R_lowHPD = CI_down_Re_estimate) %>%
    select(region, data_type, date, median_R_mean, median_R_highHPD, median_R_lowHPD,
           pathogen_type)
  
  return(flu_ww_re)
}

#' Read in data on IAV and IBV from wastewater
#' @return Data frame with columns: region, pathogen_type, data_type, date, observation, (optional: observation_smooth, orig_data, protocol_status)
load_flu_ww_data <- function(path_to_data = "rww_data_flu/ww_loads.csv") {
  flu_ww_data_raw <- read_csv(path_to_data)
  flu_ww_data <- flu_ww_data_raw %>%
    rename(date = sample_date, orig_data = is_observation) %>%
    mutate(quantification_flag = "N/A") %>%
    mutate(region = case_when(
      observation_type == "ARA Basel" ~ "BS",
      observation_type == "ARA Werdhölzli" ~ "ZH",
      observation_type == "STEP Aire" ~ "GE"
    )) %>%
    pivot_longer(cols = c("IAV_(gc/day)", "IBV_(gc/day)"), names_to = "data_type", values_to = "observation") %>%
    select(region, data_type, date, observation, orig_data) %>%
    mutate(pathogen_type = case_when(
      data_type == "IAV_(gc/day)" ~ "IAV",
      data_type == "IBV_(gc/day)" ~ "IBV"
    )) %>%
    mutate(protocol_status = "latest")
  
  return(flu_ww_data)
}
