## ============================================================
## SPLINE REGRESSION ON REAL DATA: Wage data from ISLR
## Goal:
##   1. Fit spline models to a real dataset
##   2. Compare linear, polynomial, and spline fits
##   3. Produce plots that are appropriate for interpretation
##   4. Show diagnostics and model comparison
##
## Dataset:
##   Wage from the ISLR package
##
## Scientific question:
##   How does wage vary with age?
##
## Notes:
##   - We use age as the predictor and wage as the response.
##   - This is a standard example because the age-wage relationship
##     is nonlinear and easy to interpret visually.
## ============================================================

## install.packages("ISLR")   # uncomment if needed
library(ISLR)
library(splines)

data(Wage)

## ------------------------------------------------------------
## 1. Prepare data
## ------------------------------------------------------------
dat <- Wage[, c("wage", "age")]
dat <- na.omit(dat)

str(dat)
summary(dat)

cat("\nNumber of observations:", nrow(dat), "\n")

## ------------------------------------------------------------
## 2. Initial exploratory plots
## ------------------------------------------------------------
par(mfrow = c(1, 2))

hist(dat$age,
     breaks = 25,
     main = "Distribution of age",
     xlab = "Age")

plot(dat$age, dat$wage,
     pch = 19, cex = 0.5,
     xlab = "Age",
     ylab = "Wage",
     main = "Scatterplot: Wage vs Age")

par(mfrow = c(1, 1))

cat("\nInitial interpretation:\n")
cat("* Wage appears to increase with age at younger ages, then level off or decline.\n")
cat("* A purely linear fit is likely too restrictive.\n")

## ------------------------------------------------------------
## 3. Fit candidate models
## ------------------------------------------------------------
## Linear model
m_lin <- lm(wage ~ age, data = dat)

## Cubic polynomial
m_poly3 <- lm(wage ~ age + I(age^2) + I(age^3), data = dat)

## Natural cubic spline with chosen df
m_ns4 <- lm(wage ~ ns(age, df = 4), data = dat)
m_ns6 <- lm(wage ~ ns(age, df = 6), data = dat)

## Natural cubic spline with manual knots
knots_manual <- c(33, 42, 55)
m_ns_manual <- lm(wage ~ ns(age, knots = knots_manual), data = dat)

models <- list(
  linear = m_lin,
  poly3 = m_poly3,
  ns_df4 = m_ns4,
  ns_df6 = m_ns6,
  ns_manual = m_ns_manual
)

## ------------------------------------------------------------
## 4. Generate fitted curves and confidence bands
## ------------------------------------------------------------
age_grid <- seq(min(dat$age), max(dat$age), length.out = 400)
newdat <- data.frame(age = age_grid)

pred_fun <- function(model, newdata) {
  p <- predict(model, newdata = newdata, se.fit = TRUE)
  data.frame(
    age = newdata$age,
    fit = p$fit,
    lower = p$fit - 1.96 * p$se.fit,
    upper = p$fit + 1.96 * p$se.fit
  )
}

pred_lin <- pred_fun(m_lin, newdat)
pred_poly3 <- pred_fun(m_poly3, newdat)
pred_ns4 <- pred_fun(m_ns4, newdat)
pred_ns6 <- pred_fun(m_ns6, newdat)
pred_ns_manual <- pred_fun(m_ns_manual, newdat)

## ------------------------------------------------------------
## 5. Main interpretation plots
## ------------------------------------------------------------
## Plot 1: linear vs spline
plot(dat$age, dat$wage,
     pch = 19, cex = 0.35,
     xlab = "Age",
     ylab = "Wage",
     main = "Linear fit vs natural spline")
lines(pred_lin$age, pred_lin$fit, lwd = 2, lty = 2)
lines(pred_ns6$age, pred_ns6$fit, lwd = 3)
legend("topright",
       legend = c("Linear", "Natural spline (df=6)"),
       lty = c(2, 1), lwd = c(2, 3), bty = "n")

## Plot 2: spline with confidence band
plot(dat$age, dat$wage,
     pch = 19, cex = 0.35,
     xlab = "Age",
     ylab = "Wage",
     main = "Natural spline (df=6) with 95% CI")

lines(pred_ns6$age, pred_ns6$fit, lwd = 3)
lines(pred_ns6$age, pred_ns6$lower, lwd = 1, lty = 2)
lines(pred_ns6$age, pred_ns6$upper, lwd = 1, lty = 2)

cat("\nInterpretation of the spline plot:\n")
cat("* The fitted curve represents the estimated mean wage as a smooth function of age.\n")
cat("* The dashed curves are pointwise 95% confidence bands for the mean function.\n")
cat("* Interpretation should focus on the shape of the fitted relationship, not on basis coefficients.\n")

## ------------------------------------------------------------
## 6. Compare different spline choices
## ------------------------------------------------------------
par(mfrow = c(1, 2))

plot(dat$age, dat$wage,
     pch = 19, cex = 0.3,
     xlab = "Age",
     ylab = "Wage",
     main = "Natural splines with different df")
lines(pred_ns4$age, pred_ns4$fit, lwd = 2, lty = 2)
lines(pred_ns6$age, pred_ns6$fit, lwd = 2, lty = 1)
legend("topright",
       legend = c("ns(df=4)", "ns(df=6)"),
       lty = c(2, 1), lwd = 2, bty = "n")

plot(dat$age, dat$wage,
     pch = 19, cex = 0.3,
     xlab = "Age",
     ylab = "Wage",
     main = "Natural spline with manual knots")
lines(pred_ns_manual$age, pred_ns_manual$fit, lwd = 3)
abline(v = knots_manual, lty = 3)
legend("topright",
       legend = c("Spline fit", "Knots"),
       lty = c(1, 3), lwd = c(3, 1), bty = "n")

par(mfrow = c(1, 1))

cat("\nInterpretation of flexibility and knots:\n")
cat("* Increasing df allows more local flexibility.\n")
cat("* Manual knots show where the spline is allowed to bend more flexibly.\n")
cat("* Interpretation should ask whether extra wiggliness reflects signal or noise.\n")

## ------------------------------------------------------------
## 7. Plot basis functions (pedagogically useful)
## ------------------------------------------------------------
B_ns <- ns(age_grid, knots = knots_manual)

matplot(age_grid, B_ns,
        type = "l", lty = 1, lwd = 2,
        xlab = "Age",
        ylab = "Basis value",
        main = "Natural spline basis functions")
abline(v = knots_manual, lty = 3)

cat("\nBasis-function interpretation:\n")
cat("* The spline fit is a linear combination of these basis functions.\n")
cat("* These basis coefficients are generally not interpreted directly.\n")
cat("* The fitted curve is the meaningful object for interpretation.\n")

## ------------------------------------------------------------
## 8. Model comparison
## ------------------------------------------------------------
mse <- function(y, yhat) mean((y - yhat)^2)

perf_tab <- data.frame(
  model = names(models),
  AIC = sapply(models, AIC),
  train_MSE = sapply(models, function(m) mse(dat$wage, fitted(m)))
)

perf_tab <- perf_tab[order(perf_tab$AIC), ]

cat("\n--- Model comparison ---\n")
print(round(perf_tab, 3), row.names = FALSE)

cat("\nInterpretation:\n")
cat("* Linear regression provides a baseline.\n")
cat("* Polynomial and spline models allow nonlinearity.\n")
cat("* AIC can be used to compare overall fit-complexity tradeoffs.\n")

## ------------------------------------------------------------
## 9. Diagnostics for selected spline model
## ------------------------------------------------------------
cat("\n--- Diagnostics for natural spline model (df=6) ---\n")
print(summary(m_ns6))

par(mfrow = c(2, 2))
plot(m_ns6)
par(mfrow = c(1, 1))

## Residuals vs age: especially useful for functional form assessment
plot(dat$age, resid(m_ns6),
     pch = 19, cex = 0.4,
     xlab = "Age",
     ylab = "Residuals",
     main = "Residuals vs Age: spline model")
abline(h = 0, lwd = 2)

ord <- order(dat$age)
smooth_vals <- predict(loess(resid(m_ns6) ~ age, data = dat))
lines(dat$age[ord], smooth_vals[ord], lwd = 2)

cat("\nDiagnostic guidance:\n")
cat("* Residuals vs fitted and residuals vs age help assess remaining structure.\n")
cat("* If strong systematic curvature remains, the mean model may still be inadequate.\n")
cat("* QQ plot checks approximate normality of residuals.\n")
cat("* Scale-location plot checks homoskedasticity.\n")

## ------------------------------------------------------------
## 10. Optional: derivative-style interpretation via finite differences
## ------------------------------------------------------------
## Approximate slope of the fitted spline
dx <- 0.1
grid1 <- data.frame(age = age_grid)
grid2 <- data.frame(age = age_grid + dx)

fit1 <- predict(m_ns6, newdata = grid1)
fit2 <- predict(m_ns6, newdata = grid2)

deriv_approx <- (fit2 - fit1) / dx

plot(age_grid, deriv_approx,
     type = "l", lwd = 2,
     xlab = "Age",
     ylab = "Approximate slope of fitted curve",
     main = "Approximate derivative of spline fit")
abline(h = 0, lty = 2)

cat("\nInterpretation of derivative plot:\n")
cat("* Positive values indicate wage increasing with age locally.\n")
cat("* Negative values indicate wage decreasing with age locally.\n")
cat("* This is often a useful way to interpret nonlinear fits.\n")

## ------------------------------------------------------------
## 11. Final teaching summary
## ------------------------------------------------------------
cat("\n============================================================\n")
cat("FINAL TEACHING SUMMARY\n")
cat("============================================================\n")
cat("1. Splines let us model nonlinear relationships more flexibly than linear or polynomial regression.\n")
cat("2. The primary object of interpretation is the fitted curve, not the spline coefficients.\n")
cat("3. Confidence bands help assess uncertainty in the estimated mean function.\n")
cat("4. Knot placement and degrees of freedom control flexibility.\n")
cat("5. Residual diagnostics remain essential after fitting spline models.\n")
cat("6. The derivative plot can help interpret where the response is increasing, leveling off, or decreasing.\n")