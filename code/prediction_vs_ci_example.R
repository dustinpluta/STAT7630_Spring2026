set.seed(123)

# -----------------------
# Fake data
# -----------------------
n <- 60
x <- runif(n, 0, 10)
y <- 2 + 1.3 * x + rnorm(n, sd = 2)

# -----------------------
# Best-fit line
# -----------------------
fit <- lm(y ~ x)

# Grid of x values for smooth line/intervals
x0 <- seq(min(x), max(x), length.out = 200)
newdat <- data.frame(x = x0)

# Mean response CI: E[Y | X=x0]
ci_mean <- predict(fit, newdata = newdat, interval = "confidence", level = 0.95)

# Prediction interval: Y_new | X=x0
pi_pred <- predict(fit, newdata = newdat, interval = "prediction", level = 0.95)

# -----------------------
# Plot
# -----------------------
plot(x, y, pch = 19, main = "Linear regression with mean CI and prediction interval",
     xlab = "x", ylab = "y")

# Best-fit line
lines(x0, ci_mean[, "fit"], lwd = 2)

# Mean confidence band (dashed)
lines(x0, ci_mean[, "lwr"], lty = 2)
lines(x0, ci_mean[, "upr"], lty = 2)

# Prediction band (dotted)
lines(x0, pi_pred[, "lwr"], lty = 3)
lines(x0, pi_pred[, "upr"], lty = 3)

  legend("topleft", bty = "n",
       legend = c("Best-fit line", "95% CI for mean E[Y|x]", "95% prediction interval for Y_new"),
       lty = c(1, 2, 3), lwd = c(2, 1, 1))
