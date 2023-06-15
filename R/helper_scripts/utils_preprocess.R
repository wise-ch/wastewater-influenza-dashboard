#' Get newest data file in repository
#' Assumes a naming scheme that sorts lexicographically by recency
#' @param dir The directory where the data is stored.
#' @return Path of newest data file
get_newest_file <- function(dir, filename_pattern) {
  files <- list.files(path = dir, pattern = filename_pattern, full.names = T)
  newest_data_file <- sort(files)[length(files)]
  return(newest_data_file)
}

