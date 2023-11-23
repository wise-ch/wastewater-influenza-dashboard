library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

#' Get newest data file from FOPH.
#' @param path_to_data The directory where FOPH data is stored.
#' @return Newest data as a data frame.


get_newest_data <- function(path_to_data = "data/raw_data/sentinella_data"){
  files_cnslt<- list.files(path = path_to_data, pattern = ".*ili_ari_sentinellapop_ab2020.xlsx$", full.names = T)
  files_tests <- list.files(path = path_to_data, pattern = ".*CNRIresults_ab2020.xlsx$", full.names = T)
  newest_data_file_cnslt <- sort(files_cnslt)[length(files_cnslt)]
  newest_data_file_tests <- sort(files_tests)[length(files_tests)]
  print(paste("Newest consultations file found is: ", newest_data_file_cnslt, "\nNewest tests file found is:", newest_data_file_tests))
  
  newest_data_cnslt_pop <- data.table(readxl::read_excel(newest_data_file_cnslt, 1))# for first tab
  newest_data_cnslt_ili <- data.table(readxl::read_excel(newest_data_file_cnslt, 2))
  newest_data_cnslt_ari <- data.table(readxl::read_excel(newest_data_file_cnslt, 3))
  
  
  newest_data_tests = data.table()
  for (tab in 4:6){
    newest_data_tests <- rbind(newest_data_tests, data.table(readxl::read_excel(newest_data_file_tests, tab)))# for first tab
  }

  return(list(pop_consultations = newest_data_cnslt_pop, ili_consultations = newest_data_cnslt_ili, ari_consultations = newest_data_cnslt_ari, tests = newest_data_tests))
}

print("Loading newest sentinella data from FOPH")
sentinella_data_raw <- get_newest_data()

sentinella_pop = sentinella_data_raw$pop_consultations
sentinella_ili = sentinella_data_raw$ili_consultations
sentinella_ari = sentinella_data_raw$ari_consultations
sentinella_test_data = sentinella_data_raw$tests

region_numbers = fread('data/raw_data/sentinella_data/region_numbers.csv')

## summarise the ili consultations 

sentinella_ili[, year  := floor(sntnl_jahrwoche/100)]
sentinella_ili[, week := sntnl_jahrwoche - (year * 100)]

sentinella_ili[ , total_ili := sum(n_ili),   by=c('region', 'year', 'week')]
sentinella_ili[ , total_pop := sum(sentpop), by=c('region', 'year', 'week')]
sentinella_ili_summary = unique(sentinella_ili[region != 'unbekannt', c('region', 'year', 'week', 'total_pop', 'total_ili')])

## summarise the ili consultations 

sentinella_ari[, year  := floor(sntnl_jahrwoche/100)]
sentinella_ari[, week := sntnl_jahrwoche - (year * 100)]

sentinella_ari[ , total_ari := sum(n_ari),   by=c('region', 'year', 'week')]
sentinella_ari[ , total_pop := sum(sentpop), by=c('region', 'year', 'week')]
sentinella_ari_summary = unique(sentinella_ari[region != 'unbekannt', c('region', 'year', 'week', 'total_pop', 'total_ari')])

#combine consultations and link to region numbers 
sentinella_summary = merge(sentinella_ili_summary, sentinella_ari_summary, by=c('region', 'year', 'week', 'total_pop'))
sentinella_summary = merge(sentinella_summary, region_numbers, by=c('region'), all.x = T)

sentinella_test_data[, region_nr := Region]

sentinella_test_data[, year  := floor(sntnl_jahrwoche/100)]
sentinella_test_data[, week := sntnl_jahrwoche - (year * 100)]


sentinella_summary_tests = merge(sentinella_summary, sentinella_test_data, by=c('year', 'week', 'region_nr'))

get_date_from_weekyear = function(y, w){return(as.Date(paste0(y, str_pad(w, 2, pad = "0"), 1), "%Y%U%u"))}

sentinella_summary_tests[, date := as.Date(paste0(year, str_pad(week, 2, pad = "0"), 1), "%Y%U%u")]


sentinella_summary_tests[, prop_rsv := n_rsv / n_complete]
sentinella_summary_tests[, total_rsv := prop_rsv * total_ili]

sentinella_summary_tests[, prop_iva := n_influ_a / n_complete]
sentinella_summary_tests[, total_iva := prop_iva * total_ili]

sentinella_summary_tests[, prop_ivb := n_influ_b / n_complete]
sentinella_summary_tests[, total_ivb := prop_ivb * total_ili]

sentinella_summary_tests[, prop_cov := n_sars_cov2 / n_complete]
sentinella_summary_tests[, total_cov := n_sars_cov2 * total_ili]


assign_measuring_period = function(year, week){ 
  if(week>30){
    return(paste0(year, '/', year - 2000 + 1))
  }
  else{
    return(paste0(year - 1, '/', year - 2000))
  }
}



sentinella_summary_tests[, measuring_period:=assign_measuring_period(year, week), by=c('year', 'week')]



clean_data_sentinella = 
  sentinella_summary_tests[, c('date', 'region_nr', 'measuring_period', 'total_rsv', 'total_iva', 'total_ivb', 'total_cov')] |>
  
  rename(
    RSV = total_rsv, 
    IVA = total_iva, 
    IVB = total_ivb, 
    `SARS-COV-2` = total_cov) |>
  
  melt(id.vars = c("date", "region_nr",  "measuring_period"), 
       measure.vars = c('RSV', 'IVA', 'IVB', 'SARS-COV-2'), 
       value.name = "total_cnslt", 
       variable.name = "pathogen")

write.csv(file='data/clean_data_sentinella.csv', x = clean_data_sentinella)
write.csv(file='data/fuller_sentinealla_data.csv', x = sentinella_summary_tests)

sentinella_summary_tests %>% 
       ggplot()+
       geom_line(aes(x=date, y=total_ari), color='red') + 
       geom_line(aes(x=date, y=n_samples), color='blue') +
       facet_grid(region~measuring_period, scales='free')


sentinella_summary_tests %>% 
  ggplot() + 
  geom_histogram(aes(x=total_ari/n_samples))


sentinella_summary_tests %>% 
  ggplot() + 
  geom_histogram(aes(x=total_ari-total_ili))
