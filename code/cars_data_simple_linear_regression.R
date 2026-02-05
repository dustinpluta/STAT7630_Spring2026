# Load data
data(cars)

# Exploratory plot
plot(dist ~ speed, data = cars,
     main = "Stopping Distance vs Speed",
     xlab = "Speed (mph)",
     ylab = "Stopping Distance (ft)")

# Fit simple linear regression
fit_cars <- lm(dist ~ speed, data = cars)

# Add regression line
abline(fit_cars, col = "red", lwd = 2)

# Model summary
summary(fit_cars)

# Diagnostics
par(mfrow = c(2, 2))
plot(fit_cars)
par(mfrow = c(1, 1))
