# This script is to estimate the symptom score distribution from a digitized figure

library(ggplot2)
library(dplyr)
library(MASS)
library(fitdistrplus)

data <- read.csv("data/symptoms_score_carrat_2008.csv")

ggplot(
    data = data,
    aes(x = days_after_innoculation, y = symptom_score)
) + geom_point()

# Method: as used by EAWAG, fit by moments

# Get a function that returns linearly interpolated data points according to empirical data
d.empirical <- approxfun(
    x = data$days_after_innoculation, 
    y = data$symptom_score, 
    yleft = 0,  # assumes shedding is 0 before innoculation
    yright = 0  # assumes end of shedding captured by the data (at 9 days)
)

# Test the fuction
# plot(d.empirical(seq(to = 10, by = 0.1)))

# Compute the integral of that function
Z <- integrate(f = d.empirical, lower = 0, upper = 10)$value  # defaults to 100 subdivisions, which seems to be enough

# Calculate the mean and standard deviation of the normalized empirical distribution
f.M1 <- function(x) d.empirical(x) / Z * x
f.M2 <- function(x) d.empirical(x) / Z * x ^ 2
mean <- integrate(f.M1, 0, 1000)$value
sd <- sqrt(integrate(f.M2, 0, 1000)$value - mean^2)

# Calculate matching shape and scale of Gamma distribution
scale <- sd^2 / mean
shape <- mean^2/ sd ^2

# Add to summary data
fit <- data.frame(
    mean = mean, sd = sd, shape = shape, scale = scale,
    empirical_dist_integral = Z,
    source_data = "Carra 2008", method = "Moments"
)

# Plot comparative results
ggplot(
    data = data,
    aes(x = days_after_innoculation, y = symptom_score)
) + geom_point(aes(shape = "Carrat 2008 data")) +
    geom_line(
        data = data.frame(days_after_innoculation = seq(from = 0, to = 10, by = 0.1)) %>% 
            mutate(symptom_score = Z * dgamma(x = days_after_innoculation, shape = fit$shape, scale = fit$scale)),
        aes(linetype = "Gamma distribution fit")
    ) +
    labs(x = "Days after innoculation", y = "Symptom score") +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 10, 1)) +
    scale_y_continuous(limits = c(0, NA), oob = scales::oob_keep) +
    theme(legend.position = "bottom", legend.title = element_blank())
ggsave("figures/symptom_score_fit.png", width = 4, height = 3, units = "in")

# Write out fitted parameters
write.csv(
    x = fit,
    file = "data/symptom_score_fit.csv",
    row.names = F
)
