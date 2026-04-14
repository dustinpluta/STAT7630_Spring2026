## ============================================================
## SIMPLE LOGISTIC REGRESSION DEMO IN R USING FAKE DATA
##
## This script shows:
## 1. Why linear regression is not appropriate for binary data
## 2. A well-calibrated logistic regression example
## 3. A poorly calibrated logistic regression example
##
## Included plots:
## - scatterplots of binary data with fitted probabilities
## - calibration plots
## - histograms of predicted probabilities
##
## ============================================================

set.seed(123)

## ------------------------------------------------------------
## 0. Helper functions
## ------------------------------------------------------------

inv_logit <- function(x) {
  1 / (1 + exp(-x))
}

calibration_plot <- function(y, phat, n_bins = 10, main = "Calibration Plot") {
  # Bin by quantiles of predicted probability
  probs <- seq(0, 1, length.out = n_bins + 1)
  breaks <- quantile(phat, probs = probs, na.rm = TRUE)
  breaks <- unique(breaks)
  
  # If too few unique breakpoints, fall back to equally spaced bins
  if (length(breaks) < 3) {
    breaks <- seq(min(phat), max(phat), length.out = n_bins + 1)
  }
  
  bin <- cut(phat, breaks = breaks, include.lowest = TRUE)
  
  mean_pred <- tapply(phat, bin, mean)
  obs_rate  <- tapply(y, bin, mean)
  counts    <- tapply(y, bin, length)
  
  plot(mean_pred, obs_rate,
       xlim = c(0, 1), ylim = c(0, 1),
       pch = 19,
       xlab = "Mean predicted probability",
       ylab = "Observed event rate",
       main = main)
  
  abline(0, 1, lwd = 2, lty = 2)   # ideal calibration
  lines(mean_pred, obs_rate, lwd = 2)
  
  text(mean_pred, obs_rate, labels = counts, pos = 3, cex = 0.8)
  
  invisible(data.frame(
    mean_pred = mean_pred,
    obs_rate = obs_rate,
    n = counts
  ))
}

classification_scatter <- function(x, y, phat_lm, phat_glm, main = "") {
  plot(x, y,
       pch = 19, col = rgb(0, 0, 0, 0.35),
       xlab = "x", ylab = "y (0/1)",
       main = main)
  
  ord <- order(x)
  lines(x[ord], phat_lm[ord], lwd = 2, lty = 2)
  lines(x[ord], phat_glm[ord], lwd = 2)
  
  abline(h = 0, lty = 3)
  abline(h = 1, lty = 3)
  
  legend("topleft",
         legend = c("Linear regression fit", "Logistic regression fit"),
         lty = c(2, 1), lwd = 2, bty = "n")
}

probability_plot <- function(x, phat, true_p = NULL, main = "Predicted probabilities") {
  ord <- order(x)
  plot(x[ord], phat[ord],
       type = "l", lwd = 2,
       ylim = c(0, 1),
       xlab = "x", ylab = "Probability",
       main = main)
  
  if (!is.null(true_p)) {
    lines(x[ord], true_p[ord], lwd = 2, lty = 2)
    legend("topleft",
           legend = c("Predicted", "True"),
           lty = c(1, 2), lwd = 2, bty = "n")
  }
}

## ------------------------------------------------------------
## 1. Comparison of linear regression to logistic regression
##    on binary data
## ------------------------------------------------------------

n <- 400
x <- runif(n, -3, 3)

# True probability model
eta_true <- -0.5 + 1.4 * x
p_true <- inv_logit(eta_true)
y <- rbinom(n, size = 1, prob = p_true)

dat <- data.frame(x = x, y = y)

# Fit linear regression (incorrect model for binary data)
fit_lm <- lm(y ~ x, data = dat)

# Fit logistic regression (appropriate model)
fit_glm <- glm(y ~ x, family = binomial, data = dat)

# Predicted values
phat_lm <- predict(fit_lm)
phat_glm <- predict(fit_glm, type = "response")

cat("\n============================================================\n")
cat("1. Linear regression vs logistic regression on binary data\n")
cat("============================================================\n")
cat("\n--- Linear regression summary ---\n")
print(summary(fit_lm))

cat("\n--- Logistic regression summary ---\n")
print(summary(fit_glm))

cat("\nRange of linear regression fitted values:\n")
print(range(phat_lm))

cat("\nRange of logistic regression fitted probabilities:\n")
print(range(phat_glm))

par(mfrow = c(1, 2))

classification_scatter(
  x, y,
  phat_lm = phat_lm,
  phat_glm = phat_glm,
  main = "Binary data: LM vs Logistic fit"
)

hist(phat_lm,
     breaks = 20,
     main = "Linear regression fitted values",
     xlab = "Fitted value")

par(mfrow = c(1, 1))

cat("\nInterpretation:\n")
cat("* Linear regression can produce fitted values below 0 or above 1.\n")
cat("* Logistic regression keeps fitted probabilities in [0, 1].\n")
cat("* Logistic regression also captures the nonlinear mean pattern implied by binary data.\n")

## ------------------------------------------------------------
## 2. Well-calibrated logistic regression example
## ------------------------------------------------------------

# Simulate a fresh dataset from the same model we will fit
n2 <- 1000
x1 <- rnorm(n2)
x2 <- rnorm(n2)

eta_well <- -0.8 + 1.0 * x1 - 1.2 * x2
p_well <- inv_logit(eta_well)
y_well <- rbinom(n2, size = 1, prob = p_well)

dat_well <- data.frame(y = y_well, x1 = x1, x2 = x2)

# Fit the correct logistic model
fit_well <- glm(y ~ x1 + x2, family = binomial, data = dat_well)
phat_well <- predict(fit_well, type = "response")

cat("\n============================================================\n")
cat("2. Well-calibrated logistic regression example\n")
cat("============================================================\n")
cat("\n--- Well-calibrated model summary ---\n")
print(summary(fit_well))

par(mfrow = c(2, 2))

# Predicted probabilities vs x1
probability_plot(
  x = dat_well$x1,
  phat = phat_well,
  true_p = p_well,
  main = "Well-calibrated model:\nPredicted probabilities vs x1"
)

# Predicted probability histogram
hist(phat_well,
     breaks = 20,
     main = "Well-calibrated model:\nPredicted probabilities",
     xlab = "Predicted probability")

# Calibration plot
calibration_plot(
  y = dat_well$y,
  phat = phat_well,
  n_bins = 10,
  main = "Well-calibrated model:\nCalibration plot"
)

# Pearson residuals vs fitted
pearson_resid_well <- residuals(fit_well, type = "pearson")
plot(phat_well, pearson_resid_well,
     pch = 19, col = rgb(0, 0, 0, 0.35),
     xlab = "Fitted probability",
     ylab = "Pearson residual",
     main = "Well-calibrated model:\nPearson residuals vs fitted")
abline(h = 0, lwd = 2)

par(mfrow = c(1, 1))

cat("\nInterpretation:\n")
cat("* This model is well calibrated because the fitted model matches the true data-generating mechanism.\n")
cat("* The calibration plot should lie close to the 45-degree line.\n")
cat("* Predicted probabilities should track the true probabilities reasonably well.\n")

## ------------------------------------------------------------
## 3. Poorly calibrated logistic regression example
## ------------------------------------------------------------

# Simulate data from a more complex truth
n3 <- 1000
x1_bad <- rnorm(n3)
x2_bad <- rnorm(n3)

# True model includes nonlinearity and interaction
eta_bad_true <- -0.6 + 1.1 * x1_bad - 1.0 * x2_bad + 1.2 * (x1_bad * x2_bad) - 0.9 * (x1_bad^2)
p_bad_true <- inv_logit(eta_bad_true)
y_bad <- rbinom(n3, size = 1, prob = p_bad_true)

dat_bad <- data.frame(y = y_bad, x1 = x1_bad, x2 = x2_bad)

# Fit a misspecified model that omits the interaction and quadratic term
fit_bad <- glm(y ~ x1 + x2, family = binomial, data = dat_bad)
phat_bad <- predict(fit_bad, type = "response")

cat("\n============================================================\n")
cat("3. Poorly calibrated logistic regression example\n")
cat("============================================================\n")
cat("\n--- Poorly calibrated model summary ---\n")
print(summary(fit_bad))

par(mfrow = c(2, 2))

# Predicted probabilities vs x1
probability_plot(
  x = dat_bad$x1,
  phat = phat_bad,
  true_p = p_bad_true,
  main = "Poorly calibrated model:\nPredicted probabilities vs x1"
)

# Predicted probability histogram
hist(phat_bad,
     breaks = 20,
     main = "Poorly calibrated model:\nPredicted probabilities",
     xlab = "Predicted probability")

# Calibration plot
calibration_plot(
  y = dat_bad$y,
  phat = phat_bad,
  n_bins = 10,
  main = "Poorly calibrated model:\nCalibration plot"
)

# Pearson residuals vs fitted
pearson_resid_bad <- residuals(fit_bad, type = "pearson")
plot(phat_bad, pearson_resid_bad,
     pch = 19, col = rgb(0, 0, 0, 0.35),
     xlab = "Fitted probability",
     ylab = "Pearson residual",
     main = "Poorly calibrated model:\nPearson residuals vs fitted")
abline(h = 0, lwd = 2)

par(mfrow = c(1, 1))

cat("\nInterpretation:\n")
cat("* The fitted model is misspecified: it omits important nonlinear and interaction structure.\n")
cat("* Because of this, predicted probabilities are systematically off.\n")
cat("* The calibration plot will typically deviate from the 45-degree line.\n")
cat("* This illustrates that even logistic regression can be poorly calibrated if the mean structure is wrong.\n")

## ------------------------------------------------------------
## 4. Optional comparison: improve the poorly calibrated model
## ------------------------------------------------------------

fit_bad_fixed <- glm(y ~ x1 + x2 + I(x1^2) + x1:x2,
                     family = binomial, data = dat_bad)
phat_bad_fixed <- predict(fit_bad_fixed, type = "response")

par(mfrow = c(1, 2))

calibration_plot(
  y = dat_bad$y,
  phat = phat_bad,
  n_bins = 10,
  main = "Misspecified logistic model"
)

calibration_plot(
  y = dat_bad$y,
  phat = phat_bad_fixed,
  n_bins = 10,
  main = "Improved logistic model"
)

par(mfrow = c(1, 1))

cat("\nOptional comparison:\n")
cat("* Adding the missing quadratic and interaction terms should improve calibration.\n")
cat("* This reinforces that calibration depends on model specification, not just on using a logistic link.\n")

## ------------------------------------------------------------
## 5. Final teaching summary
## ------------------------------------------------------------

cat("\n============================================================\n")
cat("FINAL TEACHING SUMMARY\n")
cat("============================================================\n")
cat("1. Linear regression is not appropriate for binary responses because fitted values can fall outside [0,1].\n")
cat("2. Logistic regression models probabilities through the logit link and keeps fitted values in [0,1].\n")
cat("3. A well-calibrated logistic model produces predicted probabilities that agree with observed event frequencies.\n")
cat("4. Calibration plots compare predicted risk to observed event rates.\n")
cat("5. Logistic regression can still be poorly calibrated if the linear predictor is misspecified.\n")
cat("6. Predicted probability plots are useful for interpreting how risk changes with predictors.\n")