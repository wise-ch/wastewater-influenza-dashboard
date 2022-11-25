# This script is to cacluate the percentage of the Swiss population covered by the WWTPs studied

library(dplyr)
library(tidyr)
library(xlsx)

# Load data
coverage_data_raw <- read.csv("data/raw_data/DATA_Ara_ZurAireBasel.csv")  # percent of population in different locations inside WWTP catchment area
population_data_raw <- read.csv("data/raw_data/swiss_population_data_by_plz.csv")

# Re-format data
population_data <- population_data_raw %>%
    mutate(PLZ_permanent_residents = gsub(" ", "", Total)) %>%
    mutate(PLZ_permanent_residents = as.numeric(PLZ_permanent_residents)) %>%
    select(-Total)
coverage_data <- coverage_data_raw %>%
    group_by(ARA_NAMEN, PLZ) %>%
    summarise(
        Per_PLZ_avg = mean(Per_PLZ),
        Per_PLZ_min = min(Per_PLZ),
        Per_PLZ_max = max(Per_PLZ),
        n_communities_in_PLZ = n(),
        .groups = "drop"
    )
total_pop <- sum(population_data$PLZ_permanent_residents)

# Ensure all PLZ in WWTP coverage data in population data
non_matching_plz <- coverage_data$PLZ[!(coverage_data$PLZ %in% population_data$Postleitzahl)]
if (length(non_matching_plz) > 0) {
    warning("Some PLZ not found in population data source!")
}

# Join population data to WWTP coverage data
# TODO: something wrong here: total is well above 100%
coverage_data_w_pop <- coverage_data %>%
    left_join(population_data, by = c("PLZ" = "Postleitzahl")) %>%
    mutate(
        est_residents_covered = PLZ_permanent_residents * Per_PLZ_avg / 100,
        est_min_residents_covered = PLZ_permanent_residents * Per_PLZ_min / 100,
        est_max_residents_covered = PLZ_permanent_residents * Per_PLZ_max / 100
    )

coverage_total <- coverage_data_w_pop %>%
    group_by(ARA_NAMEN) %>%
    summarize(
        est_residents_covered = sum(est_residents_covered),
        est_min_residents_covered = sum(est_min_residents_covered),
        est_max_residents_covered = sum(est_max_residents_covered)
    ) %>%
    mutate(
        est_percent_pop_covered = round(est_residents_covered / total_pop, 2),
        est_min_percent_pop_covered = round(est_min_residents_covered / total_pop, 2),
        est_max_percent_pop_covered = round(est_max_residents_covered / total_pop, 2)
    )

alt_estimates <- data.frame(
    ARA_NAMEN = c("VERNIER/AIRE", "ZUERICH(WERDHOELZLI)", "BASEL"),
    alt_est_residents_covered = c(445000, 670000, 270000),
    alt_est_percent_pop_covered = c(445000, 670000, 270000) / total_pop,
    alt_est_source = c(
        "https://ww2.sig-ge.ch/en/a-propos-de-sig/nous-connaitre/sites_expositions/step-aire",  # "+445000 residents are connected to the plant"
        "https://de.wikipedia.org/wiki/Kl%C3%A4rwerk_Werdh%C3%B6lzli",  # "Sie ist mit einem Einwohnerwert von 670.000 Menschen die grösste Kläranlage der Schweiz"
        "https://www.prorheno.ch/anlagen/ara-basel"  # "Im gesamten Einzugsbebiet sind rund 270'000 Einwohner angeschlossen"
    )
)
coverage_summary <- coverage_total %>%
    full_join(alt_estimates, by = "ARA_NAMEN")

write.csv(coverage_summary, "data/data_used_in_manuscript/wwtp_population_coverage_summary.csv", row.names = F)

sum(coverage_summary$alt_est_percent_pop_covered)  # 16% Swiss population covered by Zurich, Basel, Geneva WWTP
sum(coverage_summary$est_percent_pop_covered)  # 12% Swiss population covered by Zurich, Basel, Geneva WWTP
