# This script is to estimate the shedding load distribution from data collected from a few studies

library(ggplot2)
library(dplyr)
library(MASS)
library(fitdistrplus)
library(xlsx)

# Load, clean, and filter data
data <- read.xlsx("data/shedding_profile_data.xlsx", sheetIndex = 2)

data_cleaned <- data %>% group_by(specimen.type) %>%
    mutate(sample_type = case_when(
        specimen.type == "nose-throat swab" ~ "respiratory",
        specimen.type == "sputum" ~ "respiratory",
        specimen.type == "faeces" ~ "stool")) %>%
    mutate(title_2 = paste(author_year, title, sep = ": ")) %>%
    mutate(third_sd = 3 * sd(measurement)) %>%
    mutate(outlier = case_when(measurement > third_sd ~ T, T ~ F)) %>%
    mutate(measurement.day = as.numeric(measurement.day))

data_filtered <- data_cleaned %>% 
    filter(
        units != "Cycle threshold",
        !is.na(measurement.day),
        specimen.type == "faeces",
        units == "copies/g")  %>% 
        mutate(measurement.day = measurement.day + 2)  # temporal.measurement.reference.point is always symptom onset, shift by 2 to account for mean delay from infection to symptom onset in Carrat 2008

# Write out data used
write.csv(data_filtered, "data/data_for_fecal_shedding_dist.csv", row.names = F)

data_summarized <- data_filtered %>% group_by(measurement.day) %>% summarize(measurement = mean(measurement))

p <- ggplot(
    data = data_filtered,
    aes(x = measurement.day, y = measurement)
) + geom_point() +
    geom_line(data = data_summarized, aes(color = "Mean measurement")) +
    scale_y_continuous(limits = c(0, NA))
show(p)

# Method 2: as used by EAWAG, fit by moments

# Get a function that returns linearly interpolated data points according to empirical data
d.empirical <- approxfun(
    x = data_summarized$measurement.day, 
    y = data_summarized$measurement, 
    yleft = 0,  # assumes shedding is 0 before innoculation
    yright = 0  # assumes end of shedding captured by the data (at 9 days)
)

# Test the fuction
p2 <- p + geom_line(
    data = data.frame(
        measurement.day = seq(to = 35, by = 0.5),
        measurement = d.empirical(seq(to = 35, by = 0.5))
    ),
    aes(color = "Empirical distribution")
)
show(p2)

# Compute the integral of that function
Z <- integrate(f = d.empirical, lower = 0, upper = 35)$value  # defaults to 100 subdivisions, which seems to be enough

# Calculate the mean and standard deviation of the normalized empirical distribution
f.M1 <- function(x) d.empirical(x) / Z * x
f.M2 <- function(x) d.empirical(x) / Z * x ^ 2
mean_moments <- integrate(f.M1, 0, 1000)$value
sd_moments <- sqrt(integrate(f.M2, 0, 1000)$value - mean_moments ^ 2)

# Calculate matching shape and scale of Gamma distribution
shape_moments <- mean_moments ^ 2 / sd_moments ^ 2
scale_moments <- sd_moments ^ 2 / mean_moments

# Plot comparative results
p + geom_line(
        data = data.frame(
            measurement.day = seq(to = 35, by = 0.5)
        ) %>% 
            mutate(measurement = Z * dgamma(x = measurement.day, shape = shape_moments, scale = scale_moments)),
        aes(linetype = "Gamma distribution fit")
    )
# Plot simpler results
ggplot(
    data = data_filtered,
    aes(x = measurement.day, y = measurement)
) + geom_point() +
    geom_line(
        data = data.frame(
            measurement.day = seq(to = 35, by = 0.5)
        ) %>% 
            mutate(measurement = Z * dgamma(x = measurement.day, shape = shape_moments, scale = scale_moments)),
        aes(linetype = "Gamma distribution fit")
    ) + theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank()) + 
    labs(x = "Days after innoculation", y = "Genome copies / gram feces")
ggsave("figures/shedding_profile_fit_fecal.png", width = 4, height = 3, units = "in")

# Write out fitted parameters
write.csv(
  x = data.frame(
    mean = mean_moments, sd = sd_moments, shape = shape_moments, scale = scale_moments,
    empirical_dist_integral = Z,
    source_data = "Lit review", method = "Moments"
  ),
  file = "data/shedding_profile_fit_fecal.csv",
  row.names = F
)
