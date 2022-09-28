#' Read in influenza Re and data from doctor consultations for influenza-like-ilnness reported in the national Sentinella system
load_flu_ili_all <- function() {
  results <- list("re" = load_flu_ili_re(), "data" = load_flu_ili_data())
  return(results)
}

#' Read in infuenza Re based on doctor consultations for influenza-like-ilnness reported in the national Sentinella system
#' @return Data frame with columns: region, pathogen_type, data_type, date, median_R_mean, median_R_highHPD, median_R_lowHPD
load_flu_ili_re <- function(path_to_data = "rww_data_flu/case_re_estimates.csv") {
  flu_ili_re_tmp <- read_csv(path_to_data) %>%
    mutate(data_type = "Influenza-like illness consultations (National)") %>%
    rename(median_R_mean = Re_estimate, median_R_highHPD = CI_up_Re_estimate, 
           median_R_lowHPD = CI_down_Re_estimate) %>%
    select(data_type, date, median_R_mean, median_R_highHPD, median_R_lowHPD)

  # Have ILI-based Re apply to IAV and IBV and all cantons
  flu_ili_re_tmp2 <- rbind(
    flu_ili_re_tmp %>% mutate(pathogen_type = "IAV"),
    flu_ili_re_tmp %>% mutate(pathogen_type = "IBV")
  )
  
  flu_ili_re <- rbind(
    flu_ili_re_tmp2 %>% mutate(region = "ZH"),
    flu_ili_re_tmp2 %>% mutate(region = "BS"),
    flu_ili_re_tmp2 %>% mutate(region = "GE")
  )
  
  return(flu_ili_re)
}

#' Read in data on IAV and IBV from wastewater
#' @return Data frame with columns: region, pathogen_type, data_type, date, observation, (optional: observation_smooth, orig_data, protocol_status)
load_flu_ili_data <- function(path_to_data = "rww_data_flu/Inzidenz_Grippeverdacht.csv") {
  flu_ili_data_raw <- read_csv(path_to_data)
  flu_ili_data <- flu_ili_data_raw %>%
    mutate(year_week = paste(Jahr, `Sentinella Woche`, "0", sep = "-")) %>%
    mutate(date = as.Date(x = year_week, format = "%Y-%U-%w") - 1) %>%  # date made to match report statement that Sentinella week 20 is 14.05.2022 - 20.05.2022
    rename(observation = `Konsultationen pro 100 000 Einwohner`) %>%
    mutate(data_type = "Influenza-like illness consultations (National)") %>%
    mutate(region = NA) %>%
    mutate(pathogen_type = "ILI") %>%
    select(region, pathogen_type, data_type, date, observation)
  
  return(flu_ili_data)
}
