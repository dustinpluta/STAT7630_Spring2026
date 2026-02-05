############################################################
# Simple Linear Regression with Full Diagnostics (R)
# Applied setting: effect of study hours on exam score
############################################################

set.seed(123)

# -----------------------------
# 1. Simulate data
# -----------------------------
n <- 40
hours <- runif(n, 0, 20)                       # hours studied
exam_score <- 50 + 2.5 * hours +               # true linear effect
  rnorm(n, mean = 0, sd = 8)                   # random noise

dat <- data.frame(hours, exam_score)

# -----------------------------
# 2. Fit simple linear regression
# -----------------------------
fit <- lm(exam_score ~ hours, data = dat)

# View model summary
summary(fit)

# -----------------------------
# 3. Standard diagnostic plots
# -----------------------------
# These four plots assess:
# (1) Linearity and mean-zero errors
# (2) Normality of residuals
# (3) Constant variance (homoskedasticity)
# (4) Influence and leverage

par(mfrow = c(2, 2))     # arrange 4 plots in one window
plot(fit)
par(mfrow = c(1, 1))     # reset plotting layout

# -----------------------------
# 4. Focused diagnostics with annotations
# -----------------------------

par(mfrow = c(1, 2))

# Residuals vs Fitted:
# Look for random scatter around 0.
# Curvature suggests nonlinearity.
# Funnel shape suggests heteroskedasticity.
plot(fit, which = 1)
abline(h = 0, lty = 2, col = "gray")

# Cook's distance:
# Points with large Cook's distance may be influential.
# id.n labels the most influential observations.
plot(fit, which = 4, id.n = 5)

par(mfrow = c(1, 1))

# -----------------------------
# 5. Optional numerical diagnostics
# -----------------------------

# Confidence interval for slope
confint(fit, "hours")

# Standardized residuals (useful for outlier detection)
rstandard(fit)

# Leverage values (hat values)
hatvalues(fit)

############################################################
# End of example
############################################################
