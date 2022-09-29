#' Read in COVID Re based on confirmed cases in the catchment
#' @return Data frame with columns: region, pathogen_type, data_type, date, median_R_mean, median_R_highHPD, median_R_lowHPD
load_covid_catchment_cases_re <- function(path_to_data = "rww_data/Rcc_catchment.csv") {
  covid_catchment_re <- read_csv(path_to_data) %>%
    mutate(pathogen_type = "COVID") %>%
    mutate(data_type = factor(data_type)) %>%
    mutate(data_type = recode_factor(data_type, "infection_cases" = "Confirmed (Catchment)"))

  return(covid_catchment_re) # formerly called Re_cc_needed
}


#' Read in COVID Re based on confirmed cases in the canton
#' @return Data frame with columns: region, pathogen_type, data_type, date, median_R_mean, median_R_highHPD, median_R_lowHPD
load_covid_cantonal_cases_re <- function(url_to_data = "https://raw.githubusercontent.com/covid-19-Re/dailyRe-Data/master/CHE-estimates.csv") {
  covid_cantonal_re <- read_csv(url_to_data,
    col_names = c(
      "country", "region", "source", "data_type",
      "estimate_type", "date", "median_R_mean",
      "median_R_highHPD", "median_R_lowHPD",
      "countryIso3"
    ),
    col_types = cols(date = col_date(format = "")),
    skip = 1
  )

  covid_cantonal_re <- covid_cantonal_re %>%
    filter(
      estimate_type == "Cori_slidingWindow",
      data_type != "Confirmed cases / tests",
      date >= as_date("2020-10-01")
    ) %>%
    dplyr::select(-estimate_type, -countryIso3, -country, -source) %>%
    mutate(data_type = recode_factor(data_type, "Confirmed cases" = "Confirmed (Canton)")) %>%
    mutate(pathogen_type = "COVID")

  return(covid_cantonal_re)
}

# Read in data on COVID from confirmed cases in the canton
# For legacy reasons, this data is packaged with the wastewater load data and is loaded in "reading_in_covid_ww.R"
