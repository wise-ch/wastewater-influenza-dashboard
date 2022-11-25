# This script is to estimate the shedding load distribution from a digitized figure

library(ggplot2)
library(dplyr)
library(MASS)
library(fitdistrplus)

data <- read.csv("data/data_used_in_manuscript/shedding_profile_carrat_2008.csv") %>%
    mutate(viral_titer = 10^log_viral_titer)

ggplot(
    data = data,
    aes(x = days_after_innoculation, y = viral_titer)
) + geom_point()

# Reconstruct a fake dataset from plot
fake_data <- c()
for (i in seq_len(nrow(data))) {
    measurement <- data[i, "days_after_innoculation"]
    n_measurement <- round(data[i, "viral_titer"])
    fake_data <- c(fake_data, rep(measurement, n_measurement))
}

# Plot fake data and digitized data correspondance
ggplot(data = data) +
    geom_point(aes(x = days_after_innoculation, y = viral_titer, color = "Viral titer from plot")) +
    geom_bar(data = data.frame(obs = fake_data), aes(x = obs, fill = "Fake observations to fit ditribution to"), alpha = 0.5) +
    theme_bw() +
    theme(legend.position = "bottom")

# Fit density function to fake dataset that follows plot
fit <- fitdist(fake_data[fake_data > 0], "gamma", method = "mle")  # excludes only one datapoint at 0
fitted_shape <- fit$estimate["shape"]
fitted_rate <- fit$estimate["rate"]

# Print fitted parameters
mean <- round(fitted_shape / fitted_rate, 2)
sd <- round(sqrt(fitted_shape / (fitted_rate^2)), 2)
shape <- round(fitted_shape, 2)
rate <- round(fitted_rate, 2)
scale <- round(1 / fitted_rate, 2)
print(paste("Mean of gamma shedding distribution is", mean, "days"))
print(paste("Standard deviation of gamma shedding distribution is", sd, "days"))
print(paste("Shape of gamma shedding distribution is", shape))
print(paste("Rate of gamma shedding distribution is", rate))
print(paste("Scale of gamma shedding distribution is", scale))

fit <- data.frame(
    mean = mean, sd = sd, shape = shape, rate = rate, scale = scale,
    empirical_dist_integral = NA,
    source_data = "Carrat 2008", method = "MLE"
)

# Method 2: as used by EAWAG, fit by moments

# Get a function that returns linearly interpolated data points according to empirical data
d.empirical <- approxfun(
    x = data$days_after_innoculation,
    y = data$viral_titer,
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
fit <- fit %>% bind_rows(data.frame(
    mean = mean, sd = sd, shape = shape, rate = rate, scale = scale,
    empirical_dist_integral = Z,
    source_data = "Carra 2008", method = "Moments"
))

# Plot comparative results
ggplot(
    data = data,
    aes(x = days_after_innoculation, y = viral_titer)
) + geom_point(aes(shape = "Carrat 2008 data")) +
    # geom_line(
    #     data = data.frame(days_after_innoculation = seq(from = 0, to = 10, by = 0.1)) %>%
    #         mutate(viral_titer = Z * dgamma(x = days_after_innoculation, shape = fit$shape[1], scale = fit$scale[1])),
    #     aes(linetype = "Gamma shedding distribution fit by MLE")
    # ) +
    geom_line(
        data = data.frame(days_after_innoculation = seq(from = 0, to = 10, by = 0.1)) %>%
            mutate(viral_titer = Z * dgamma(x = days_after_innoculation, shape = fit$shape[2], scale = fit$scale[2])),
        aes(linetype = "Gamma distribution fit")
    ) +
    labs(x = "Days after innoculation", y = "log10 Viral RNA") +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 10, 1)) +
    scale_y_continuous(limits = c(0, NA), oob = scales::oob_keep) +
    theme(legend.position = "bottom", legend.title = element_blank())
ggsave("figures/shedding_profile_fit.png", width = 4, height = 3, units = "in")

# Write out fitted parameters
write.csv(
    x = fit,
    file = "data/raw_data/shedding_profile_fit.csv",
    row.names = F
)
