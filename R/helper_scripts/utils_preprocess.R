#' Get newest data file in repository
#' Assumes a naming scheme that sorts lexicographically by recency
#' @param dir The directory where the data is stored.
#' @return Path of newest data file
get_newest_file <- function(dir, filename_pattern) {
  files <- list.files(path = dir, pattern = filename_pattern, full.names = T)
  newest_data_file <- sort(files)[length(files)]
  return(newest_data_file)
}

#' Assigns dates to measuring periods
#' The measuring periods/seasons follow the influenza monitoring scheme by FOPH
#' @param date A date or date vector
#' @return A character or character vector with the measuring period(s)
get_measuring_period <- function(date) {
  return(
    case_when(
      date >= as.Date("2021-07-01") & date < as.Date("2022-07-01") ~ "2021/22",
      date >= as.Date("2022-07-01") & date < as.Date("2023-07-01") ~ "2022/23",
      date >= as.Date("2023-07-01") & date < as.Date("2024-07-01") ~ "2023/24",
      T ~ "Outside of measuring period"
    )
  )
}
