set.seed(42)

# -----------------------------
# 1) Simulate housing-like data
# -----------------------------
n <- 120
sqft <- runif(n, 800, 3500)

# Multiplicative noise => right-skewed prices + heteroskedasticity on original scale
price <- 120 * sqft^1.05 * exp(rnorm(n, mean = 0, sd = 0.25))

dat <- data.frame(sqft = sqft, price = price)

# -----------------------------
# 2) Fit naive model on original scale
# -----------------------------
fit_raw <- lm(price ~ sqft, data = dat)
par(mfrow = c(2, 2))
plot(fit_raw)

#############################
# What next?                #
#############################



# -----------------------------
# 3) Log-transform response (common fix)
# -----------------------------
fit_logY <- lm(log(price) ~ sqft, data = dat)

# Is this a good fit?

par(mfrow = c(2, 2))
plot(fit_logY)

# (Optional) Log-log model can be even more natural here:
fit_loglog <- lm(log(price) ~ log(sqft), data = dat)
par(mfrow = c(2, 2))
plot(fit_loglog)
# -----------------------------
# 4) Compare diagnostics visually
# -----------------------------
par(mfrow = c(2, 2))
plot(fit_raw, which = 1, main = "Raw: Residuals vs Fitted")      # expect funnel pattern
plot(fit_raw, which = 2, main = "Raw: Normal Q-Q")              # expect tail issues
plot(fit_logY, which = 1, main = "log(Price): Residuals vs Fitted")
plot(fit_logY, which = 2, main = "log(Price): Normal Q-Q")

# -----------------------------
# 5) Summaries and interpretation
# -----------------------------
summary(fit_raw)
summary(fit_logY)
summary(fit_loglog)

# Interpretation helper for log(Y) ~ X:
# A 1-unit increase in X changes expected Y by about 100*beta percent (for small beta).
coef(fit_logY)

# Interpretation helper for log(Y) ~ log(X):
# beta is elasticity: 1% increase in X -> beta% increase in Y (approximately).
coef(fit_loglog)
confint(fit_loglog)
