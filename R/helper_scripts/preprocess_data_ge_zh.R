##### EAWAG data: Geneva and Zurich #####

library(ggplot2)
library(dplyr)
library(tidyr)

data_eawag <- read.csv("data/raw_data/LatestFluData_22-11-09.csv")

flow_data_ge<- read.table(
  "https://sensors-eawag.ch/sars/__data__/processed_normed_data_geneve_v2.csv",
  sep = ";",
  header = T,
  check.names = F
)
flow_data_zh <- read.table(
  "https://sensors-eawag.ch/sars/__data__/processed_normed_data_zurich_v2.csv",
  sep = ";",
  header = T,
  check.names = F
)

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
  filter(TotalDroplets >= 15000, sample_type == "ww") %>%
  mutate(sample_date = as.Date(sample_date)) %>%
  rename(IAV_gc_per_mL_WW = IAV.M_.gc.mLWW.) %>%
  rename(IBV_gc_per_mL_WW = IBV.M_.gc.mLWW.) %>%
  rename(SARS2_N1_gc_per_mL_WW = SARS.N1_.gc.mLWW.) %>%
  rename(SARS2_N2_gc_per_mL_WW = SARS.N2_.gc.mLWW.)

flow_data_ge_clean <- flow_data_ge %>%
  rename("date" = "") %>%
  mutate(wwtp = "STEP Aire") %>%
  mutate(date = as.Date(date)) %>%
  select(wwtp, date, `flow [m^3/d]`)
flow_data_zh_clean <- flow_data_zh %>%
  rename("date" = "") %>%
  mutate(wwtp = "ARA Werdhölzli") %>%
  mutate(date = as.Date(date)) %>%
  select(wwtp, date, `flow [m^3/d]`)

ml_per_l <- 1000
l_per_m3 <- 1000

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

clean_data_long_eawag <- rbind(clean_data_ge, clean_data_zh) %>%
  select(
    sample_date, 
    wwtp, 
    target,
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
  group_by(sample_date, wwtp, target, measurement_type) %>%
  summarize(
    mean = mean(value), 
    min = min(value), 
    max = max(value), 
    n_measurements = n(), 
    .groups = "drop")

# Plot controls (manually inspect)
ggplot(data = control_data_long_eawag, aes(x = target, y = value)) +
  geom_boxplot(aes(color = target)) +
  facet_grid(name ~ sample_type, scales = "free_y") +
  labs(x = "Target", y = "Measurement (genome copies per mL wastewater)")

ggsave("figures/all_controls_eawag.png", width = 7, height = 7, units = "in")

# Plot all data
ggplot(data = clean_data_long_means_eawag %>% filter(sample_date <= as.Date("2022-08-01")), 
       aes(x = as.Date(sample_date), y = mean, color = target)) +
  geom_point() +
  geom_errorbar(aes(ymin = min, ymax = max), width = 5) +
  facet_grid(measurement_type ~ wwtp, scales = "free_y") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  theme(legend.position = "bottom") +
  labs(x = element_blank(), y = "Mean and min/max measurement across replicates")

ggsave("figures/all_data_eawag.png", width = 9, height = 9, units = "in")

# Select data for influenza analysis
clean_data_long_means_eawag_for_analysis <- clean_data_long_eawag %>%
  filter(!(sample_date <= as.Date("2022-02-01") & target == "sn1flu")) %>%  # remove in favor of respv4 data; TODO: re-run all data with respv4 assay
  filter(!(measurement_type %in% c("IBV_gc_per_mL_WW", "IBV_gc_per_day") & target == "iabv")) %>%  # remove IABV assay data for IBV due to poor separation between positive and negative droplets in controls
  filter(as.Date(sample_date) <= as.Date("2022-08-01")) %>%
  filter(!(measurement_type %in% c("SARS2_N1_gc_per_mL_WW", "SARS2_N2_gc_per_mL_WW"))) %>%
  group_by(sample_date, wwtp, measurement_type) %>%
  summarize(
    mean = mean(value), 
    min = min(value), 
    max = max(value), 
    n_measurements = n(), 
    .groups = "drop")

# Plot data used in analysis
ggplot(data = clean_data_long_means_eawag_for_analysis, 
       aes(x = as.Date(sample_date), y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = min, ymax = max), width = 5) +
  facet_grid(measurement_type ~ wwtp, scales = "free_y") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  theme(legend.position = "bottom") +
  labs(x = element_blank(), y = "Mean and min/max measurement across analyzed replicates")

ggsave("figures/analyzed_data_eawag.png", width = 9, height = 9, units = "in")

write.csv(clean_data_long_means_eawag_for_analysis, "data/clean_data_ge_zh.csv", row.names = F)
