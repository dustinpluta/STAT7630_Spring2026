## ============================================================
## FULLY WORKED LOGISTIC REGRESSION EXAMPLE USING REAL DATA
## Dataset: Default (from ISLR)
## Outcome: default (Yes/No)
## ============================================================

## install.packages("ISLR")   # uncomment if needed
library(ISLR)

## ------------------------------------------------------------
## 1. Load and inspect the data
## ------------------------------------------------------------
data(Default)

str(Default)
summary(Default)

## Convert response to 0/1 for some later calculations if useful
Default$default01 <- ifelse(Default$default == "Yes", 1, 0)

cat("\n--- First few rows ---\n")
print(head(Default))

cat("\n--- Proportion defaulting ---\n")
print(mean(Default$default01))

## ------------------------------------------------------------
## 2. Exploratory analysis
## ------------------------------------------------------------

par(mfrow = c(1, 3))

boxplot(balance ~ default, data = Default,
        xlab = "Default status", ylab = "Balance",
        main = "Balance by default status")

boxplot(income ~ default, data = Default,
        xlab = "Default status", ylab = "Income",
        main = "Income by default status")

barplot(table(Default$student, Default$default),
        beside = TRUE,
        legend.text = TRUE,
        main = "Student status by default")

par(mfrow = c(1, 1))

cat("\nExploratory interpretation:\n")
cat("* Customers who default tend to have much higher balances.\n")
cat("* Income may matter, but the relationship is often weaker than balance.\n")
cat("* Student status may also be associated with default risk.\n")

## ------------------------------------------------------------
## 3. Fit logistic regression models
## ------------------------------------------------------------

## Model 1: balance only
fit1 <- glm(default ~ balance, family = binomial, data = Default)

## Model 2: balance + income
fit2 <- glm(default ~ balance + income, family = binomial, data = Default)

## Model 3: balance + income + student
fit3 <- glm(default ~ balance + income + student, family = binomial, data = Default)

cat("\n============================================================\n")
cat("MODEL SUMMARIES\n")
cat("============================================================\n")

cat("\n--- Model 1: default ~ balance ---\n")
print(summary(fit1))

cat("\n--- Model 2: default ~ balance + income ---\n")
print(summary(fit2))

cat("\n--- Model 3: default ~ balance + income + student ---\n")
print(summary(fit3))

## ------------------------------------------------------------
## 4. Interpret coefficients via odds ratios
## ------------------------------------------------------------

cat("\n============================================================\n")
cat("ODDS RATIOS\n")
cat("============================================================\n")

or_fit3 <- exp(coef(fit3))
ci_fit3 <- exp(confint(fit3))

cat("\n--- Odds ratios for Model 3 ---\n")
print(or_fit3)

cat("\n--- 95% CI for odds ratios (Model 3) ---\n")
print(ci_fit3)

cat("\nInterpretation guidance:\n")
cat("* exp(beta) is the multiplicative change in odds for a one-unit increase in the predictor.\n")
cat("* For balance and income, a one-unit change is small, so it is often more meaningful\n")
cat("  to interpret per $100 or per $1000.\n")

## Example: odds ratio for a $100 increase in balance
beta_balance <- coef(fit3)["balance"]
or_balance_100 <- exp(100 * beta_balance)

cat("\n--- Odds ratio for a $100 increase in balance ---\n")
print(or_balance_100)

## ------------------------------------------------------------
## 5. Predicted probabilities
## ------------------------------------------------------------

Default$phat <- predict(fit3, type = "response")

cat("\n--- Range of predicted probabilities ---\n")
print(range(Default$phat))

## Plot predicted probabilities against balance
ord <- order(Default$balance)

plot(Default$balance, Default$default01,
     pch = 19, col = rgb(0, 0, 0, 0.25),
     xlab = "Balance",
     ylab = "Default (0/1)",
     main = "Observed defaults and fitted probabilities")

lines(Default$balance[ord], Default$phat[ord], lwd = 3)

cat("\nInterpretation:\n")
cat("* The fitted curve shows estimated default probability as a function of predictors.\n")
cat("* Since balance is a dominant predictor, default risk rises strongly with balance.\n")

## ------------------------------------------------------------
## 6. Predicted probability examples
## ------------------------------------------------------------

new_customers <- data.frame(
  balance = c(500, 1500, 2500),
  income = c(40000, 40000, 40000),
  student = factor(c("No", "No", "No"), levels = levels(Default$student))
)

pred_new <- predict(fit3, newdata = new_customers, type = "response")

cat("\n============================================================\n")
cat("PREDICTED PROBABILITIES FOR EXAMPLE CUSTOMERS\n")
cat("============================================================\n")
print(cbind(new_customers, predicted_prob = pred_new))

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
  
  invisible(data.frame(
    mean_pred = mean_pred,
    obs_rate = obs_rate,
    n = counts
  ))
}

calibration_plot(Default$default01, Default$phat,
                 n_bins = 10,
                 main = "Calibration plot: Model 3")

cat("\nCalibration interpretation:\n")
cat("* Points near the 45-degree line indicate good agreement between predicted\n")
cat("  probabilities and observed event rates.\n")
cat("* Points below the line indicate overprediction; above the line indicate underprediction.\n")

## ------------------------------------------------------------
## 8. Histograms of predicted probabilities by class
## ------------------------------------------------------------

par(mfrow = c(1, 2))

hist(Default$phat[Default$default == "No"],
     breaks = 25,
     main = "Predicted probabilities\nfor non-defaulters",
     xlab = "Predicted probability")

hist(Default$phat[Default$default == "Yes"],
     breaks = 25,
     main = "Predicted probabilities\nfor defaulters",
     xlab = "Predicted probability")

par(mfrow = c(1, 1))

cat("\nInterpretation:\n")
cat("* A useful model tends to assign low probabilities to non-defaulters\n")
cat("  and higher probabilities to defaulters.\n")
cat("* Overlap is expected, but strong separation indicates better discrimination.\n")

## ------------------------------------------------------------
## 9. Residual diagnostics
## ------------------------------------------------------------

pearson_resid <- residuals(fit3, type = "pearson")
deviance_resid <- residuals(fit3, type = "deviance")
lev <- hatvalues(fit3)

par(mfrow = c(2, 2))

plot(fitted(fit3), pearson_resid,
     pch = 19, col = rgb(0, 0, 0, 0.25),
     xlab = "Fitted probability",
     ylab = "Pearson residual",
     main = "Pearson residuals vs fitted")
abline(h = 0, lwd = 2)

plot(fitted(fit3), deviance_resid,
     pch = 19, col = rgb(0, 0, 0, 0.25),
     xlab = "Fitted probability",
     ylab = "Deviance residual",
     main = "Deviance residuals vs fitted")
abline(h = 0, lwd = 2)

qqnorm(deviance_resid, main = "Q-Q plot of deviance residuals")
qqline(deviance_resid, lwd = 2)

plot(lev, deviance_resid,
     pch = 19, col = rgb(0, 0, 0, 0.25),
     xlab = "Leverage",
     ylab = "Deviance residual",
     main = "Leverage vs deviance residual")

par(mfrow = c(1, 1))

cat("\nDiagnostic guidance:\n")
cat("* Residual plots help identify lack of fit, unusual observations, and influential points.\n")
cat("* In logistic regression, residuals are not expected to behave exactly like OLS residuals,\n")
cat("  but strong systematic patterns are still concerning.\n")

## ------------------------------------------------------------
## 10. Compare models with deviance and AIC
## ------------------------------------------------------------

cat("\n============================================================\n")
cat("MODEL COMPARISON\n")
cat("============================================================\n")

anova_fit <- anova(fit1, fit2, fit3, test = "Chisq")
print(anova_fit)

aic_tab <- data.frame(
  model = c("fit1: balance",
            "fit2: balance + income",
            "fit3: balance + income + student"),
  AIC = c(AIC(fit1), AIC(fit2), AIC(fit3))
)

print(aic_tab)

cat("\nInterpretation:\n")
cat("* The likelihood ratio test compares nested models through deviance differences.\n")
cat("* AIC compares fit and complexity.\n")
cat("* This helps decide whether adding predictors materially improves the model.\n")

## ------------------------------------------------------------
## 11. Classification table at a chosen threshold
## ------------------------------------------------------------

threshold <- 0.5
pred_class <- ifelse(Default$phat > threshold, "Yes", "No")

tab <- table(Predicted = pred_class, Observed = Default$default)

cat("\n============================================================\n")
cat("CLASSIFICATION TABLE (threshold = 0.5)\n")
cat("============================================================\n")
print(tab)

cat("\nImportant note:\n")
cat("* Logistic regression is fundamentally a probability model.\n")
cat("* Classification depends on an arbitrary threshold and should not replace\n")
cat("  interpretation of fitted probabilities.\n")

## ------------------------------------------------------------
## 12. Final teaching summary
## ------------------------------------------------------------

cat("\n============================================================\n")
cat("FINAL TEACHING SUMMARY\n")
cat("============================================================\n")
cat("1. Logistic regression is appropriate for binary outcomes such as default / no default.\n")
cat("2. Coefficients are interpreted on the log-odds scale; exponentiated coefficients are odds ratios.\n")
cat("3. Balance is typically the strongest predictor of default in this dataset.\n")
cat("4. Predicted probabilities are often more interpretable than raw coefficients.\n")
cat("5. Calibration plots assess whether predicted risks agree with observed event frequencies.\n")
cat("6. Deviance, AIC, and likelihood ratio tests can be used to compare logistic models.\n")