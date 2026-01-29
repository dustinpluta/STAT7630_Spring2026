############################################################
# Regression diagnostics in simple linear regression (R)
# We illustrate:
#  1. Well-behaved data (assumptions hold)
#  2. Heteroskedasticity
#  3. Nonlinearity
#  4. Outliers and high leverage
#
# Diagnostics examined:
#  - Residuals vs fitted
#  - Normal Q–Q plot
#  - Scale–Location
#  - Residuals vs leverage (Cook's distance)
############################################################

set.seed(42)

par(mfrow = c(2, 2))

############################################################
# 1. Well-behaved linear regression
############################################################
n <- 100
x1 <- runif(n, 0, 10)
y1 <- 2 + 1.5 * x1 + rnorm(n, sd = 1)

m1 <- lm(y1 ~ x1)

plot(m1, main = "Well-behaved data")

summary(m1)

############################################################
# 2. Heteroskedastic errors
############################################################
x2 <- runif(n, 0, 10)
y2 <- 2 + 1.5 * x2 + rnorm(n, sd = 0.3 * x2)

m2 <- lm(y2 ~ x2)

par(mfrow = c(2, 2))
plot(m2, main = "Heteroskedasticity")

summary(m2)

############################################################
# 3. Nonlinear mean structure
############################################################
x3 <- runif(n, 0, 10)
y3 <- 2 + 0.5 * x3^2 + rnorm(n, sd = 1)

m3 <- lm(y3 ~ x3)

plot(m3, main = "Nonlinearity")

summary(m3)

############################################################
# 4. Outliers and high-leverage points
############################################################
x4 <- runif(n, 0, 10)
y4 <- 2 + 1.5 * x4 + rnorm(n, sd = 1)

# Add a vertical outlier
y4[5] <- y4[5] + 10

# Add a high-leverage point
x4[100] <- 25
y4[100] <- 2 + 1.5 * 25 + 8

m4 <- lm(y4 ~ x4)

plot(m4, main = "Outliers & leverage")

summary(m4)

############################################################
# Optional: explicit diagnostic quantities
############################################################

diag_table <- function(model) {
  data.frame(
    fitted = fitted(model),
    resid = resid(model),
    std_resid = rstandard(model),
    leverage = hatvalues(model),
    cooks = cooks.distance(model)
  )
}

head(diag_table(m4))
