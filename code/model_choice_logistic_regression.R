## ============================================================
## EXAMPLE 1: MODEL CHOICE FOR A BINARY RESPONSE
## Dataset: Default (ISLR)
## Goal: choose an appropriate regression model for default risk
## ============================================================

## install.packages("ISLR")   # uncomment if needed
library(ISLR)
library(splines)

data(Default)

## ------------------------------------------------------------
## 1. Prepare the data
## ------------------------------------------------------------
dat <- Default
dat$default01 <- ifelse(dat$default == "Yes", 1, 0)

str(dat)
summary(dat)

cat("\nEvent rate (default proportion):\n")
print(mean(dat$default01))

## ------------------------------------------------------------
## 2. Exploratory plots
## ------------------------------------------------------------
par(mfrow = c(1, 3))

boxplot(balance ~ default, data = dat,
        xlab = "Default status", ylab = "Balance",
        main = "Balance by default")

boxplot(income ~ default, data = dat,
        xlab = "Default status", ylab = "Income",
        main = "Income by default")

barplot(table(dat$student, dat$default),
        beside = TRUE, legend.text = TRUE,
        main = "Student status by default")

par(mfrow = c(1, 1))

cat("\nInitial model-choice reasoning:\n")
cat("* The response is binary, so linear regression is not appropriate as the main model.\n")
cat("* Logistic regression is the natural starting point.\n")
cat("* Balance appears strongly related to default risk.\n")

## ------------------------------------------------------------
## 3. Incorrect baseline: linear regression on binary data
## ------------------------------------------------------------
fit_lm <- lm(default01 ~ balance + income + student, data = dat)

lm_fitted <- fitted(fit_lm)

cat("\nRange of fitted values from linear regression:\n")
print(range(lm_fitted))

plot(dat$balance, dat$default01,
     pch = 19, col = rgb(0, 0, 0, 0.2),
     xlab = "Balance", ylab = "Default (0/1)",
     main = "Linear regression on binary data")
ord <- order(dat$balance)
lines(dat$balance[ord], lm_fitted[ord], lwd = 2)

cat("\nInterpretation:\n")
cat("* Linear regression can produce fitted values outside [0,1].\n")
cat("* That makes it unsuitable as the primary model for binary outcomes.\n")

## ------------------------------------------------------------
## 4. Fit logistic regression models
## ------------------------------------------------------------
## Main-effects logistic model
fit_logit1 <- glm(default ~ balance + income + student,
                  family = binomial, data = dat)

## Allow nonlinear effect of balance via spline
fit_logit2 <- glm(default ~ ns(balance, df = 4) + income + student,
                  family = binomial, data = dat)

cat("\n--- Logistic model (linear balance effect) ---\n")
print(summary(fit_logit1))

cat("\n--- Logistic model (spline balance effect) ---\n")
print(summary(fit_logit2))

## ------------------------------------------------------------
## 5. Compare candidate logistic models
## ------------------------------------------------------------
cat("\n--- Model comparison: linear vs spline balance ---\n")
print(anova(fit_logit1, fit_logit2, test = "Chisq"))

aic_tab <- data.frame(
  model = c("Logistic: linear balance", "Logistic: spline(balance, df=4)"),
  AIC = c(AIC(fit_logit1), AIC(fit_logit2))
)
print(aic_tab)

cat("\nModel-choice reasoning:\n")
cat("* If the spline model improves fit materially, that suggests nonlinear functional form.\n")
cat("* If improvement is minimal, the simpler linear-balance model may be preferable.\n")

## ------------------------------------------------------------
## 6. Predicted probabilities
## ------------------------------------------------------------
dat$phat1 <- predict(fit_logit1, type = "response")
dat$phat2 <- predict(fit_logit2, type = "response")

## Plot fitted probabilities against balance with other predictors varying
plot(dat$balance, dat$default01,
     pch = 19, col = rgb(0, 0, 0, 0.15),
     xlab = "Balance", ylab = "Default (0/1)",
     main = "Logistic fitted probabilities")
ord <- order(dat$balance)
lines(dat$balance[ord], dat$phat1[ord], lwd = 2, lty = 2)
lines(dat$balance[ord], dat$phat2[ord], lwd = 2)
legend("topleft",
       legend = c("Linear balance effect", "Spline balance effect"),
       lty = c(2, 1), lwd = 2, bty = "n")

## Cleaner effect plot: hold income at mean, student = "No"
bal_grid <- seq(min(dat$balance), max(dat$balance), length.out = 300)
newdat <- data.frame(
  balance = bal_grid,
  income = mean(dat$income),
  student = factor("No", levels = levels(dat$student))
)

p1_grid <- predict(fit_logit1, newdata = newdat, type = "response")
p2_grid <- predict(fit_logit2, newdata = newdat, type = "response")

plot(bal_grid, p1_grid, type = "l", lwd = 2, lty = 2,
     ylim = c(0, 1),
     xlab = "Balance",
     ylab = "Predicted probability of default",
     main = "Predicted probability vs balance\n(income fixed, student = No)")
lines(bal_grid, p2_grid, lwd = 2)
legend("topleft",
       legend = c("Linear balance effect", "Spline balance effect"),
       lty = c(2, 1), lwd = 2, bty = "n")

## ------------------------------------------------------------
## 7. Calibration plot
## ------------------------------------------------------------
calibration_plot <- function(y, phat, n_bins = 10, main = "Calibration Plot") {
  probs <- seq(0, 1, length.out = n_bins + 1)
  breaks <- quantile(phat, probs = probs, na.rm = TRUE)
  breaks <- unique(breaks)
  if (length(breaks) < 3) {
    breaks <- seq(min(phat), max(phat), length.out = n_bins + 1)
  }
  bin <- cut(phat, breaks = breaks, include.lowest = TRUE)
  mean_pred <- tapply(phat, bin, mean)
  obs_rate  <- tapply(y, bin, mean)
  counts    <- tapply(y, bin, length)
  
  plot(mean_pred, obs_rate,
       xlim = c(0, max(mean_pred, obs_rate, na.rm = TRUE)),
       ylim = c(0, max(mean_pred, obs_rate, na.rm = TRUE)),
       pch = 19,
       xlab = "Mean predicted probability",
       ylab = "Observed event rate",
       main = main)
  abline(0, 1, lwd = 2, lty = 2)
  lines(mean_pred, obs_rate, lwd = 2)
  text(mean_pred, obs_rate, labels = counts, pos = 3, cex = 0.8)
}

par(mfrow = c(1, 2))
calibration_plot(dat$default01, dat$phat1,
                 main = "Calibration: linear balance model")
calibration_plot(dat$default01, dat$phat2,
                 main = "Calibration: spline balance model")
par(mfrow = c(1, 1))

## ------------------------------------------------------------
## 8. Residual diagnostics
## ------------------------------------------------------------
pearson1 <- residuals(fit_logit1, type = "pearson")
pearson2 <- residuals(fit_logit2, type = "pearson")

par(mfrow = c(1, 2))
plot(fitted(fit_logit1), pearson1,
     pch = 19, col = rgb(0, 0, 0, 0.2),
     xlab = "Fitted probability", ylab = "Pearson residual",
     main = "Pearson residuals: linear balance")
abline(h = 0, lwd = 2)

plot(fitted(fit_logit2), pearson2,
     pch = 19, col = rgb(0, 0, 0, 0.2),
     xlab = "Fitted probability", ylab = "Pearson residual",
     main = "Pearson residuals: spline balance")
abline(h = 0, lwd = 2)
par(mfrow = c(1, 1))

## ------------------------------------------------------------
## 9. Final interpretation
## ------------------------------------------------------------
cat("\n============================================================\n")
cat("EXAMPLE 1 SUMMARY: MODEL CHOICE\n")
cat("============================================================\n")
cat("1. Because the response is binary, logistic regression is the correct model family.\n")
cat("2. Linear regression is not appropriate because it can predict outside [0,1].\n")
cat("3. A spline term can be used if the balance-default relationship is nonlinear.\n")
cat("4. Final model choice should balance fit improvement, calibration, and interpretability.\n")