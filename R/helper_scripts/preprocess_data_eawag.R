##### EAWAG data: 6 WWTPs #####

library(ggplot2)
library(tidyr)
library(dplyr)

source("R/local_config.R")
source("R/helper_scripts/utils_preprocess.R")

## Old data from RESP4 pannel
print("Loading newest RESP4 data from EAWAG")
file_eawag <- get_newest_file(dir = "data/raw_data/eawag_data", filename_pattern = "^LatestFluData.*csv$")
data_eawag4 <- read.csv(file_eawag, fileEncoding = "UTF-8") %>%
  mutate(sample_date = as.Date(sample_date),
         wwtp = ifelse(grepl(wwtp, pattern = "ARA Werdh"), "ARA Werdhölzli", wwtp))

eawag_cloud_filepath <- file.path(eawag_cloud_folder,"LatestFluData.csv")
newest_data_cloud <- read.csv(eawag_cloud_filepath, fileEncoding = "UTF-8") %>%
  mutate(sample_date = as.Date(sample_date),
         wwtp = ifelse(grepl(wwtp, pattern = "ARA Werdh"), "ARA Werdhölzli", wwtp))

if (!identical(data_eawag4, newest_data_cloud)) {
  message("Copying new RESP4 data from cloud folder to local folder")
  date_cloudfile_modified <- format(as.Date(file.info(eawag_cloud_filepath)$mtime)+1, "%y-%m-%d")
  file.copy(eawag_cloud_filepath, file.path("data/raw_data/eawag_data",paste0("LatestFluData_",date_cloudfile_modified,".csv")))
  data_eawag4 <- newest_data_cloud
} else {
  message("No new RESP4 data in cloud folder found.")
}

## New data from RESP6 pannel
print("Loading newest RESP6 data from EAWAG")
file_eawag6 <- get_newest_file(dir = "data/raw_data/eawag_data", filename_pattern = "^FilteredData-RESP6.*csv$")
data_eawag6 <- read.csv(file_eawag6, fileEncoding = "UTF-8") %>%
  mutate(sample_date = as.Date(SampleDate),
         wwtp = ifelse(grepl(wwtp_name, pattern = "ARA Werdh"), "ARA Werdhölzli", wwtp_name))

eawag_cloud_filepath6 <- file.path(eawag_cloud_folder,"FilteredData-RESP6.csv")
newest_data_cloud6 <- read.csv(eawag_cloud_filepath6, fileEncoding = "UTF-8") %>%
  mutate(sample_date = as.Date(SampleDate),
         wwtp = ifelse(grepl(wwtp_name, pattern = "ARA Werdh"), "ARA Werdhölzli", wwtp_name))

if (!identical(data_eawag6, newest_data_cloud6)) {
  message("Copying new RESP6 data from cloud folder to local folder")
  date_cloudfile_modified6 <- format(as.Date(file.info(eawag_cloud_filepath6)$mtime)+1, "%y-%m-%d")
  file.copy(eawag_cloud_filepath6, file.path("data/raw_data/eawag_data",paste0("FilteredData-RESP6_",date_cloudfile_modified,".csv")))
  data_eawag6 <- newest_data_cloud6
} else {
  message("No new RESP6 data in cloud folder found.")
}

data_eawag6 <- data_eawag6 |> 
  distinct() |> 
  rename(
    replicate = Replicate,
    dilution = Dilution,
    target = Target,
    spiked = Spiked,
    file_name = Filename,
    run_date = RunDate,
    sample_type = SampleType,
    TotalDroplets = TotDroplets
    )

data_eawag6 <- data_eawag6 %>%
  mutate(gc.per.mLWW. = gc_per_Lww/1000, target = stringr::str_replace(target, "-", ".")) %>%
  select(sample_date, wwtp, target, ChamberID, replicate, gc.per.mLWW., file_name, sample_type, flow) %>%
  filter(!(wwtp=="CDA Lugano" & sample_date=="2023-08-30" & flow ==26669.00)) |> # temporary workaround: filter out wrong value
  filter(!(wwtp=="CDA Lugano" & sample_date=="2023-09-01" & flow ==19761.00)) |> # temporary workaround: filter out wrong value
  pivot_wider(id_cols = -c(target, gc.per.mLWW.),
              names_from = target, values_from = gc.per.mLWW., names_glue = "{target}_.gc.mLWW.")

## Combine data
data_eawag <- bind_rows(list(
  resp4 = data_eawag4 |> select(
    assay = target,
    sample_date, wwtp, ChamberID, replicate,
    TotalDroplets,
    file_name, sample_type,
    IAV.M_.gc.mLWW.,
    IBV.M_.gc.mLWW.,
    SARS.N1_.gc.mLWW.,
    SARS.N2_.gc.mLWW.
  ),
  respv6 = data_eawag6 |> 
    mutate(assay = "respv6") |> select(
    assay,
    sample_date, wwtp, ChamberID, replicate,
    file_name, sample_type,
    IAV.M_.gc.mLWW.,
    IBV.M_.gc.mLWW.,
    SARS.N1_.gc.mLWW.,
    SARS.N2_.gc.mLWW.
  )
))

# Flow data
print(paste("Loading flow data from online EAWAG sensors"))
flow_data_gr <- read.table(
  "https://sensors-eawag.ch/sars/__data__/processed_normed_data_chur_v2.csv",
  sep = ";",
  header = T,
  check.names = F
)
colnames(flow_data_gr) <- c("date", colnames(flow_data_gr)[2:length(flow_data_gr)])
flow_data_sg <- read.table(
  "https://sensors-eawag.ch/sars/__data__/processed_normed_data_altenrhein_v2.csv",
  sep = ";",
  header = T,
  check.names = F
)
colnames(flow_data_sg) <- c("date", colnames(flow_data_sg)[2:length(flow_data_sg)])
flow_data_fr <- read.table(
  "https://sensors-eawag.ch/sars/__data__/processed_normed_data_laupen_v2.csv",
  sep = ";",
  header = T,
  check.names = F
)
colnames(flow_data_fr) <- c("date", colnames(flow_data_fr)[2:length(flow_data_fr)])
flow_data_ti <- read.table(
  "https://sensors-eawag.ch/sars/__data__/processed_normed_data_lugano_v2.csv",
  sep = ";",
  header = T,
  check.names = F
)
colnames(flow_data_ti) <- c("date", colnames(flow_data_ti)[2:length(flow_data_ti)])
flow_data_ge <- read.table(
  "https://sensors-eawag.ch/sars/__data__/processed_normed_data_geneve_v2.csv",
  sep = ";",
  header = T,
  check.names = F
)
colnames(flow_data_ge) <- c("date", colnames(flow_data_ge)[2:length(flow_data_ge)])
flow_data_zh <- read.table(
  "https://sensors-eawag.ch/sars/__data__/processed_normed_data_zurich_v2.csv",
  sep = ";",
  header = T,
  check.names = F
)
colnames(flow_data_zh) <- c("date", colnames(flow_data_zh)[2:length(flow_data_zh)])

if (!("ARA Werdhölzli" %in% data_eawag$wwtp)) {
  stop("Potential encoding problem with EAWAG data file.")
}

# Remove faulty samples
bad_samples <- read.csv("data/raw_data/eawag_data/bad samples.csv")
data_eawag <- data_eawag %>% filter(!(file_name %in% bad_samples$file_name))

# Wrangle data
control_data_eawag <- data_eawag %>% filter(sample_type %in% c("pos", "ntc"))

control_data_long_eawag <- control_data_eawag %>%
  pivot_longer(cols = c(
    IAV.M_.gc.mLWW.,
    IBV.M_.gc.mLWW.,
    SARS.N1_.gc.mLWW.,
    SARS.N2_.gc.mLWW.
  ))

clean_data_eawag <- data_eawag %>%
  filter(TotalDroplets >= 15000 | is.na(TotalDroplets), sample_type == "ww") %>%
  rename(IAV_gc_per_mL_WW = IAV.M_.gc.mLWW.) %>%
  rename(IBV_gc_per_mL_WW = IBV.M_.gc.mLWW.) %>%
  rename(SARS2_N1_gc_per_mL_WW = SARS.N1_.gc.mLWW.) %>%
  rename(SARS2_N2_gc_per_mL_WW = SARS.N2_.gc.mLWW.)

# Annotate different measuring periods (Re estimated for each separately)
clean_data_eawag <- clean_data_eawag %>%
  mutate(measuring_period = get_measuring_period(sample_date))

if (any(clean_data_eawag$measuring_period == "Outside of measuring period")) {
  warning("Some data is outside of a known measuring period, have you started monitoring a new season? Add the date range to code if so.")
}

flow_data_gr_clean <- flow_data_gr %>%
  mutate(wwtp = "ARA Chur") %>%
  mutate(date = as.Date(date)) %>%
  dplyr::select(wwtp, date, `flow [m^3/d]`)
flow_data_sg_clean <- flow_data_sg %>%
  mutate(wwtp = "ARA Altenrhein") %>%
  mutate(date = as.Date(date)) %>%
  dplyr::select(wwtp, date, `flow [m^3/d]`)
flow_data_fr_clean <- flow_data_fr %>%
  mutate(wwtp = "ARA Sensetal") %>%
  mutate(date = as.Date(date)) %>%
  dplyr::select(wwtp, date, `flow [m^3/d]`)
flow_data_ti_clean <- flow_data_ti %>%
  mutate(wwtp = "CDA Lugano") %>%
  mutate(date = as.Date(date)) %>%
  dplyr::select(wwtp, date, `flow [m^3/d]`)
flow_data_ge_clean <- flow_data_ge %>%
  mutate(wwtp = "STEP Aire") %>%
  mutate(date = as.Date(date)) %>%
  dplyr::select(wwtp, date, `flow [m^3/d]`)
flow_data_zh_clean <- flow_data_zh %>%
  mutate(wwtp = "ARA Werdhölzli") %>%
  mutate(date = as.Date(date)) %>%
  dplyr::select(wwtp, date, `flow [m^3/d]`)

ml_per_l <- 1000
l_per_m3 <- 1000

clean_data_gr <- clean_data_eawag %>%
  filter(wwtp == "ARA Chur") %>%
  left_join(flow_data_gr_clean, by = c("sample_date" = "date", "wwtp")) %>%
  mutate("IBV_gc_per_day" = IBV_gc_per_mL_WW * `flow [m^3/d]` * ml_per_l * l_per_m3) %>%
  mutate("IAV_gc_per_day" = IAV_gc_per_mL_WW * `flow [m^3/d]` * ml_per_l * l_per_m3)
clean_data_sg <- clean_data_eawag %>%
  filter(wwtp == "ARA Altenrhein") %>%
  left_join(flow_data_sg_clean, by = c("sample_date" = "date", "wwtp")) %>%
  mutate("IBV_gc_per_day" = IBV_gc_per_mL_WW * `flow [m^3/d]` * ml_per_l * l_per_m3) %>%
  mutate("IAV_gc_per_day" = IAV_gc_per_mL_WW * `flow [m^3/d]` * ml_per_l * l_per_m3)
clean_data_fr <- clean_data_eawag %>%
  filter(wwtp == "ARA Sensetal") %>%
  left_join(flow_data_fr_clean, by = c("sample_date" = "date", "wwtp")) %>%
  mutate("IBV_gc_per_day" = IBV_gc_per_mL_WW * `flow [m^3/d]` * ml_per_l * l_per_m3) %>%
  mutate("IAV_gc_per_day" = IAV_gc_per_mL_WW * `flow [m^3/d]` * ml_per_l * l_per_m3)
clean_data_ti <- clean_data_eawag %>%
  filter(wwtp == "CDA Lugano") %>%
  left_join(flow_data_ti_clean, by = c("sample_date" = "date", "wwtp")) %>%
  mutate("IBV_gc_per_day" = IBV_gc_per_mL_WW * `flow [m^3/d]` * ml_per_l * l_per_m3) %>%
  mutate("IAV_gc_per_day" = IAV_gc_per_mL_WW * `flow [m^3/d]` * ml_per_l * l_per_m3)
clean_data_ge <- clean_data_eawag %>%
  filter(wwtp == "STEP Aire") %>%
  left_join(flow_data_ge_clean, by = c("sample_date" = "date", "wwtp")) %>%
  mutate("IBV_gc_per_day" = IBV_gc_per_mL_WW * `flow [m^3/d]` * ml_per_l * l_per_m3) %>%
  mutate("IAV_gc_per_day" = IAV_gc_per_mL_WW * `flow [m^3/d]` * ml_per_l * l_per_m3)
clean_data_zh <- clean_data_eawag %>%
  filter(wwtp == "ARA Werdhölzli") %>%
  left_join(flow_data_zh_clean, by = c("sample_date" = "date", "wwtp")) %>%
  mutate("IBV_gc_per_day" = IBV_gc_per_mL_WW * `flow [m^3/d]` * ml_per_l * l_per_m3) %>%
  mutate("IAV_gc_per_day" = IAV_gc_per_mL_WW * `flow [m^3/d]` * ml_per_l * l_per_m3)

clean_data_long_eawag <- rbind(
  clean_data_gr,
  clean_data_sg,
  clean_data_fr,
  clean_data_ti,
  clean_data_ge,
  clean_data_zh
) %>%
  dplyr::select(
    sample_date,
    wwtp,
    measuring_period,
    assay,
    IAV_gc_per_mL_WW,
    IBV_gc_per_mL_WW,
    SARS2_N1_gc_per_mL_WW,
    SARS2_N2_gc_per_mL_WW,
    IAV_gc_per_day,
    IBV_gc_per_day) %>%
  pivot_longer(cols = c(
    IAV_gc_per_mL_WW,
    IBV_gc_per_mL_WW,
    SARS2_N1_gc_per_mL_WW,
    SARS2_N2_gc_per_mL_WW,
    IAV_gc_per_day,
    IBV_gc_per_day),
    names_to = "measurement_type")

clean_data_long_means_eawag <- clean_data_long_eawag %>%
  group_by(assay, sample_date, measuring_period, wwtp, measurement_type) %>%
  summarize(
    mean = mean(value),
    min = min(value),
    max = max(value),
    n_measurements = n(),
    .groups = "drop")

# Plot controls (manually inspect)
# try({
#   ggplot(data = control_data_long_eawag, aes(x = assay, y = value)) +
#   geom_boxplot(aes(color = assay)) +
#   facet_grid(name ~ sample_type, scales = "free_y") +
#   labs(x = "Assay", y = "Measurement (genome copies per mL wastewater)")
# 
# ggsave("figures/all_controls_eawag.png", width = 7, height = 7, units = "in")
# })

# Plot all data
ggplot(data = clean_data_long_means_eawag,
       aes(x = as.Date(sample_date), y = mean, color = assay)) +
  geom_point() +
  geom_errorbar(aes(ymin = min, ymax = max), width = 5) +
  facet_grid(measurement_type ~ wwtp + measuring_period, scales = "free") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  theme(legend.position = "bottom") +
  labs(x = element_blank(), y = "Mean and min/max measurement across replicates")

ggsave("figures/all_data_eawag.png", width = 9, height = 9, units = "in")

# Select data for influenza analysis
clean_data_long_eawag_for_analysis <- clean_data_long_eawag %>%
  filter(!(assay %in% c("sn1flu", "sn1(2x)flu"))) %>%  # remove data from earlier assays with different primer and/or probe concentrations
  filter(!(measurement_type %in% c("IBV_gc_per_mL_WW", "IBV_gc_per_day") & assay == "iabv")) %>%  # remove IABV assay data for IBV due to poor separation between positive and negative droplets in controls
  filter(!(sample_date <= as.Date("2021-12-07") & measurement_type %in% c("IBV_gc_per_mL_WW", "IBV_gc_per_day") & wwtp == "ARA Werdhölzli")) %>%  # single non-zero measurement followed by many 0 measurements for IBV in Zurich cause estimateR bug where minimum incidence not met, this is a crude solution to remove early measurements
  filter(!(measurement_type %in% c("SARS2_N1_gc_per_mL_WW", "SARS2_N2_gc_per_mL_WW")))  # don't analyze SARS-CoV-2

# write.csv(clean_data_long_eawag_for_analysis, "data/data_used_in_manuscript/unnaggregated_data_ge_zh.csv", row.names = F)

clean_data_long_means_eawag_for_analysis <- clean_data_long_eawag_for_analysis %>%
  group_by(sample_date, wwtp, measuring_period, measurement_type) %>%
  summarize(
    mean = mean(value),
    min = min(value),
    max = max(value),
    n_measurements = n(),
    assays = paste(unique(assay), collapse = "+"),
    .groups = "drop")

# Add data from repo
print("Loading newest RESP6 data from EAWAG github repo")
system("cd /Users/alison/Documents/dev/RespiratoryVirusesWastewater; git pull")
data_eawag6_git <- readr::read_csv("/Users/alison/Documents/dev/RespiratoryVirusesWastewater/LatestRESP6Data-filtered.csv")

data_eawag6_git <- data_eawag6_git |> 
  mutate(Protein = ifelse(Protein %in% c("N1","N2"), Protein, "")) |> # all other targets only have one Protein anyways
  mutate(measuring_period = get_measuring_period(sample_date), measurement_type = paste0(Target, Protein, "_gc_per_day")) |> 
  transmute(sample_date, wwtp, measuring_period, measurement_type, mean = Load, assays = "respv6") |> 
  mutate(wwtp = ifelse(grepl(wwtp, pattern = "ARA Werdh"), "ARA Werdhölzli", wwtp))

data_eawag6_git <- data_eawag6_git |> filter(!(wwtp == "ARA Buholz" & sample_date == "2023-10-03")) # remove duplicate, inconsistent data

if (data_eawag6_git |> count(across(-mean)) |> pull(n) |> max() > 1) {
  stop("There are duplicate measurements for some samples in the github data.")
}

catchment_pops <- tribble(
  ~wwtp, ~pop,
  "ARA Altenrhein", 64000,
  "ARA Chur", 55000,
  "ARA Sensetal", 62000,
  "ARA Werdhölzli", 471000,
  "CDA Lugano", 124000,
  "STEP Aire", 454000
)

data_eawag6_git <- data_eawag6_git |> 
  left_join(catchment_pops, by = "wwtp") |> 
  mutate(mean = mean * pop)  |> select(-pop)

data_eawag6_git |> count(wwtp, isna = is.na(mean)) |> filter(isna)

clean_data_long_means_eawag_for_analysis <- bind_rows(
  polybox = clean_data_long_means_eawag_for_analysis,
  github = data_eawag6_git,
  .id = "source"
  )

# check pop coverage
data_eawag6_git |> group_by(sample_date, wwtp, measuring_period, measurement_type) |> count() |> arrange(desc(n))

# check duplicates between polybox and github
clean_data_long_means_eawag_for_analysis |> 
  filter(stringr::str_detect(measurement_type, "gc_per_day")) |> 
  pivot_wider(id_cols = c(sample_date, wwtp, measuring_period, measurement_type, assays), names_from = source, values_from = mean) |> 
  mutate(across(c(polybox,github), as.numeric)) |> 
  mutate(pop = round(polybox / github, 2)) |> 
  filter(!is.na(pop)) |> 
  filter(pop!=1)

# Select values from duplicate information
clean_data_long_means_eawag_for_analysis <- clean_data_long_means_eawag_for_analysis |> 
  arrange(source, desc(assays)) |> # this picks github before polybox, and respv6 before respv4
  group_by(sample_date, wwtp, measuring_period, measurement_type) |> 
  summarize(mean = first(na.omit(mean)))

## Select only currently supported wwtps
clean_data_long_means_eawag_for_analysis <- clean_data_long_means_eawag_for_analysis |> 
  filter(wwtp %in% catchment_pops$wwtp)
  
# Plot data used in analysis
(ggplot(data = clean_data_long_means_eawag_for_analysis,
       aes(x = as.Date(sample_date), y = mean)) +
  geom_point(size = 0.2) + geom_line() +
  #geom_errorbar(aes(ymin = min, ymax = max), width = 5) +
  facet_grid(measurement_type ~ wwtp, scales = "free") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  theme(legend.position = "bottom") +
  labs(x = element_blank(), y = "Mean and min/max measurement across analyzed replicates")) |> plotly::ggplotly()

# ggplot(data = clean_data_long_means_eawag_for_analysis,
#        aes(x = as.Date(sample_date), y = mean)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = min, ymax = max), width = 5) +
#   facet_grid(measurement_type ~ wwtp + measuring_period, scales = "free") +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
#   theme(legend.position = "bottom") +
#   labs(x = element_blank(), y = "Mean and min/max measurement across analyzed replicates")

ggsave("figures/analyzed_data_eawag.png", width = 9, height = 9, units = "in")

write.csv(clean_data_long_means_eawag_for_analysis, "data/clean_data_eawag.csv", row.names = F)
