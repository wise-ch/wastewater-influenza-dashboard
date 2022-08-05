# variant Re estimation
# would also need to be run using cronjob
# can make a master file to run this and rww_estimation
# first get data
# cov spectrum
# data read --------------
library(jsonlite)
library(ggplot2)
library(dplyr)
library(EpiEstim)
library(ggrepel)
# wastewater -----------
# Query the API
#date_from <- as.Date("2020-11-01")
query <- paste0(
  "https://cov-spectrum.ethz.ch/api/v2/resource/wastewater?",
  "country=Switzerland"
  #"&dateFrom=", date_from
)
response <- fromJSON(query)

# Check for errors
errors <- response$errors
if (length(errors) > 0) {
  stop("Errors")
}

# Check for deprecation
deprecationDate <- response$info$deprecationDate
if (!is.null(deprecationDate)) {
  warning(paste0("This version of the API will be deprecated on ", deprecationDate,
                 ". Message: ", response$info$deprecationInfo))
}

# The data is good to be used!
#data <- response$data$data$timeseriesSummary
# just the first response$data now has the location and variant? 
# change in the way they are reporting
data <- response$data
# convert this data now
# probably using dplyr?

new_df <- tibble()

for (i in 1:nrow(data)) {
  new_df <- new_df %>%
    bind_rows(tibble(data$data$timeseriesSummary[i][[1]], 
                     location = data$location[i], variantName = data$variantName[i]))
}

#jsonlite::flatten(data)

# new_df <- data.frame(location = data$location, variant = data$variantName) %>%
#   bind_cols(data$data$timeseriesSummary)
# # not working?
# # where are the identifiers for location and variant?
# new_df <- bind_rows(data)

# data clean --------------
# instead of B.1.1.529, we have BA.1 and BA.2

# alpha = B.1.1.7
# delta = B.1.617.2
# omicron = BA.1 AND BA.2 
# do separately

variants <- c('B.1.1.7', 'B.1.617.2', 'BA.1', 'BA.2', 'BA.2.75', 'BA.4', 'BA.5')
#  BA.2.75 very low atm (end of June)
variants <- c('B.1.1.7', 'B.1.617.2', 'BA.1', 'BA.2', 'BA.4', 'BA.5') 

# wwtp size to un-normalise cases?
ref_size <- c("ZH"=471000 ,  "GE"=454000,
              "SG"=64000, "GR"=55000,
              "FR"=62000, "TI"=124000)

ref <- c("ZH"="Zurich" ,  "GE"="Geneva",
         "SG"="Altenrhein", "GR"="Chur",
         "FR"="Laupen", "TI"="Lugano")

dates_regions <- as.Date(c('2020-10-01', '2021-08-01', '2021-02-01',
                           '2021-03-08', '2021-02-02', '2021-03-08'))
names(dates_regions) <- names(ref)

variant_ww <- new_df %>% dplyr::rename(region = 'location', variant = 'variantName') %>%
  mutate(date = as.Date(date)) %>%
  mutate(region = substr(region, nchar(region)-2, nchar(region)-1)) %>%
  mutate(region = dplyr::recode(region, 'BE'= 'FR')) %>%
  filter(variant %in% variants) %>%
  filter(region %in% names(ref))

# calculate Re ---------
source('helper_code/reading_in.R')
source('helper_code/Re_helper_functions_variant.R')

variant_ww <- variant_ww %>% left_join(ww_data_new, by = c('date', 'region'))

variants_smoothed <- variant_ww %>% 
  group_by(region, variant) %>% # would only interpolate per region... and variant
  # beginning date for ww values at least
  filter(date >= dates_regions[region[1]]) %>%
  mutate(norm_n1_variant = norm_n1*proportion) %>% # would have some NA's here
  complete(date = seq.Date(min(date), max(date), by = 'days')) %>% # so we lose out on the newer cases..
  mutate(norm_n1_variant = zoo::na.approx(norm_n1_variant, na.rm = F) ,
         proportion = zoo::na.approx(proportion, na.rm = F)) %>%
  select(-n1_smooth, -flow, - orig_data) %>% 
  mutate(data_type='NGS smoothed') %>%
  ungroup()

variants_smoothed_subset <- variants_smoothed %>% 
  group_by(region, variant) %>%
  slice(ifelse(length(find_variant_dates(proportion, 0.02))==0, length(region), 
               find_variant_dates(proportion, 0.02)[1]):
          ifelse(length(find_variant_dates(proportion, 0.02))==0, length(region), 
                 find_variant_dates(proportion, 0.02)[2])) %>%
  filter(length(region)>1) %>%
  ungroup()

load_re_helpers()

regions <- c('ZH','SG', 'GR','TI', 'FR','GE')
#vars <- c('B.1.1.7', 'B.1.617.2', 'BA.1', 'BA.2')

config_df = expand.grid("region" = regions,
                        'variant' = variants,
                        'incidence_var' = c('norm_n1_variant'),
                        'GammaParams' = list(c('incubation', 'benefield')) )
# exclude Geneva and B.1.1.7 as ww data does not exist for this period
config_df <- config_df[-which(config_df$region=='GE'&config_df$variant=='B.1.1.7') ,]
# for now: just the two variants - can be easily extended

re_ww <- estimate_Re(variants_smoothed_subset, config_df = config_df, n_boot = 50)   %>% 
  mutate(source = "Wastewater (NGS)")

write.csv(re_ww, 'rww_data/variant_re.csv', row.names = F)



# read case proportions ---------
variants <- c('B.1.1.7', 'B.1.617.2', 'BA.1', 'BA.2', 'BA.2.75', 'BA.4', 'BA.5')
# no BA.2.75 atm and BA.4
variants <- c('B.1.1.7', 'B.1.617.2', 'BA.1', 'BA.2', 'BA.5') 

variant_case_proportions <- tibble()

delta_prop <- read_csv('rww_data/delta_ch.csv') %>% 
   complete(date = seq.Date(min(date), max(date), by = 'days')) %>% # so we lose out on the newer cases..
   mutate(proportion = zoo::na.approx(estimatedCases, na.rm = F)) 
 
variant_case_proportions <- delta_prop %>% select(date, proportion) %>% mutate(variant='B.1.617.2')


for (i in 1:length(variants)) {
  variant_url <- paste0('https://lapis.cov-spectrum.org/gisaid/v1/sample/aggregated?country=Switzerland&pangoLineage=',
                        variants[i],'&fields=date&dataFormat=csv&accessKey=TkdRpl8XC2bNS4ZEq0PVEWggZmk')
  variant_cases <- read_delim(variant_url)
  total_url <- 'https://lapis.cov-spectrum.org/gisaid/v1/sample/aggregated?country=Switzerland&fields=date&dataFormat=csv&accessKey=TkdRpl8XC2bNS4ZEq0PVEWggZmk'
  total_cases <- read_delim(total_url)
  
  variant_prop <- variant_cases %>% left_join(total_cases, by = 'date') %>% 
    mutate(proportion = count.x/count.y) %>% arrange(date) %>% select(date, proportion) %>%
    mutate(variant = variants[i])
  # hardcode to remove the delta inconsistency
  # not displaying correct proportions for delta for some reason
  if (variants[i]!='B.1.617.2') {
    variant_case_proportions <- variant_case_proportions %>% bind_rows(variant_prop)
  # at the moment, BA.2.75 not present  
  }
}

clinical_variant_data <- case_data %>% 
  left_join(variant_case_proportions, by = 'date') %>%
  group_by(region, variant) %>%
  complete(date = seq.Date(min(date), max(date), by = 'days')) %>% # so we lose out on the newer cases..
  mutate(proportion = zoo::na.approx(proportion, na.rm = F)) %>%
  mutate(estimatedCases = proportion*cases) %>%  
  # somehow here, delta is disappearing
  slice(ifelse(length(find_variant_dates(proportion))==0, length(region), 
               find_variant_dates(proportion)[1]):
          ifelse(length(find_variant_dates(proportion))==0, length(region), 
                 find_variant_dates(proportion)[2])) %>%
  filter(length(region)>1) %>%
  ungroup() 

# why doesn't delta exist here? Super confused...


#vars <- c('B.1.1.7', 'BA.1', 'BA.2')
config_df = expand.grid("region" = regions,
                        'variant' = variants,
                        'incidence_var' = c('estimatedCases'),
                        'GammaParams' = list(c('incubation', 'confirmed')) )
config_df <- config_df[-which(config_df$region=='GE'&config_df$variant=='B.1.1.7') ,]
config_df <- config_df[-which(config_df$region=='SG'&config_df$variant=='BA.5') ,]

clinical_variant_data <- clinical_variant_data %>% filter(region %in% regions) %>% 
  filter(variant %in% variants)

re_cc <- tibble()

for (i in 1:nrow(config_df)) {
  # where is the problem?
  re_cc <- re_cc %>% 
    bind_rows(estimate_Re(clinical_variant_data, 
                          config_df = config_df[i, ]) %>% 
                mutate(source = "Catchment cases"))
  
}
#re_cc <- estimate_Re(clinical_variant_data, config_df = config_df, n_boot = 50)   %>% 
#  mutate(source = "Catchment cases")

# anything it doesn't work for?

write.csv(re_cc, 'rww_data/variant_rcc.csv', row.names = F)


