## ============================================================
## FULLY WORKED POISSON REGRESSION EXAMPLE USING REAL DATA
## Dataset: warpbreaks (built into R)
## Response: number of breaks
## Predictors: wool type and tension
## ============================================================

## ------------------------------------------------------------
## 1. Load and inspect the data
## ------------------------------------------------------------
data(warpbreaks)

str(warpbreaks)
summary(warpbreaks)
head(warpbreaks)

## breaks = number of yarn breaks
## wool   = wool type (A or B)
## tension = tension level (L, M, H)

warpbreaks$wool <- factor(warpbreaks$wool)
warpbreaks$tension <- factor(warpbreaks$tension)

cat("\n--- Basic summaries ---\n")
print(table(warpbreaks$wool, warpbreaks$tension))

## ------------------------------------------------------------
## 2. Exploratory plots
## ------------------------------------------------------------
par(mfrow = c(1, 3))

hist(warpbreaks$breaks,
     breaks = 12,
     main = "Histogram of breaks",
     xlab = "Breaks")

boxplot(breaks ~ wool, data = warpbreaks,
        main = "Breaks by wool",
        xlab = "Wool",
        ylab = "Breaks")

boxplot(breaks ~ tension, data = warpbreaks,
        main = "Breaks by tension",
        xlab = "Tension",
        ylab = "Breaks")

par(mfrow = c(1, 1))

cat("\nExploratory interpretation:\n")
cat("* The response is a nonnegative count, so Poisson regression is a natural starting point.\n")
cat("* The distribution is right-skewed, which is common for count data.\n")
cat("* Tension appears to affect the number of breaks substantially.\n")

## Interaction plot of group means
with(warpbreaks, interaction.plot(
  x.factor = tension,
  trace.factor = wool,
  response = breaks,
  type = "b",
  pch = 19,
  lwd = 2,
  ylab = "Mean number of breaks",
  xlab = "Tension",
  trace.label = "Wool",
  main = "Group means by wool and tension"
))

## ------------------------------------------------------------
## 3. Fit Poisson regression models
## ------------------------------------------------------------
## Model 1: main effects only
fit1 <- glm(breaks ~ wool + tension,
            family = poisson,
            data = warpbreaks)

## Model 2: include interaction
fit2 <- glm(breaks ~ wool * tension,
            family = poisson,
            data = warpbreaks)

cat("\n============================================================\n")
cat("MODEL SUMMARIES\n")
cat("============================================================\n")

cat("\n--- Model 1: main effects only ---\n")
print(summary(fit1))

cat("\n--- Model 2: with interaction ---\n")
print(summary(fit2))

## ------------------------------------------------------------
## 4. Interpret coefficients
## ------------------------------------------------------------
cat("\n============================================================\n")
cat("COEFFICIENT INTERPRETATION\n")
cat("============================================================\n")

coef_tab <- cbind(
  Estimate = coef(fit1),
  RateRatio = exp(coef(fit1))
)
print(round(coef_tab, 4))

cat("\nInterpretation:\n")
cat("* Poisson regression models log(expected count).\n")
cat("* Exponentiated coefficients are multiplicative effects on the expected count.\n")
cat("* For example, exp(beta) > 1 means the expected count increases multiplicatively.\n")
cat("* exp(beta) < 1 means the expected count decreases multiplicatively.\n")

## Confidence intervals for rate ratios
rr_ci <- exp(confint(fit1))
cat("\n--- 95% CI for rate ratios (Model 1) ---\n")
print(rr_ci)

## ------------------------------------------------------------
## 5. Fitted means by group
## ------------------------------------------------------------
newdat <- expand.grid(
  wool = levels(warpbreaks$wool),
  tension = levels(warpbreaks$tension)
)

newdat$mu_hat <- predict(fit1, newdata = newdat, type = "response")
newdat$mu_hat_int <- predict(fit2, newdata = newdat, type = "response")

cat("\n--- Fitted expected counts by group ---\n")
print(newdat)

## Compare observed and fitted means
obs_means <- aggregate(breaks ~ wool + tension, data = warpbreaks, mean)
comp_tab <- merge(obs_means, newdat, by = c("wool", "tension"))
colnames(comp_tab)[3] <- "observed_mean"

cat("\n--- Observed vs fitted means (main effects and interaction models) ---\n")
print(comp_tab)

## ------------------------------------------------------------
## 6. Plot fitted counts by group
## ------------------------------------------------------------
par(mfrow = c(1, 2))

with(comp_tab, {
  plot(1:nrow(comp_tab), observed_mean,
       pch = 19, xaxt = "n",
       xlab = "Group",
       ylab = "Mean number of breaks",
       main = "Observed vs fitted means (Model 1)")
  axis(1, at = 1:nrow(comp_tab),
       labels = paste(wool, tension, sep = ":"))
  points(1:nrow(comp_tab), mu_hat, pch = 17, col = 2)
  legend("topright",
         legend = c("Observed mean", "Fitted mean"),
         pch = c(19, 17), col = c(1, 2), bty = "n")
})

with(comp_tab, {
  plot(1:nrow(comp_tab), observed_mean,
       pch = 19, xaxt = "n",
       xlab = "Group",
       ylab = "Mean number of breaks",
       main = "Observed vs fitted means (Model 2)")
  axis(1, at = 1:nrow(comp_tab),
       labels = paste(wool, tension, sep = ":"))
  points(1:nrow(comp_tab), mu_hat_int, pch = 17, col = 4)
  legend("topright",
         legend = c("Observed mean", "Fitted mean"),
         pch = c(19, 17), col = c(1, 4), bty = "n")
})

par(mfrow = c(1, 1))

## ------------------------------------------------------------
## 7. Model comparison
## ------------------------------------------------------------
cat("\n============================================================\n")
cat("MODEL COMPARISON\n")
cat("============================================================\n")

cat("\n--- Likelihood ratio test for interaction ---\n")
print(anova(fit1, fit2, test = "Chisq"))

aic_tab <- data.frame(
  model = c("Main effects", "Interaction"),
  AIC = c(AIC(fit1), AIC(fit2))
)
print(aic_tab)

cat("\nInterpretation:\n")
cat("* The likelihood ratio test compares nested Poisson models using deviance.\n")
cat("* AIC compares fit and complexity.\n")

## ------------------------------------------------------------
## 8. Check for overdispersion
## ------------------------------------------------------------
pearson_resid <- residuals(fit1, type = "pearson")
phi_hat <- sum(pearson_resid^2) / fit1$df.residual

cat("\n============================================================\n")
cat("OVERDISPERSION CHECK\n")
cat("============================================================\n")
cat("Estimated dispersion (Pearson chi-square / df):", round(phi_hat, 3), "\n")

cat("\nInterpretation:\n")
cat("* A value near 1 is consistent with the Poisson mean-variance assumption.\n")
cat("* A value much larger than 1 suggests overdispersion.\n")

## ------------------------------------------------------------
## 9. Residual diagnostics
## ------------------------------------------------------------
par(mfrow = c(2, 2))

plot(fitted(fit1), residuals(fit1, type = "pearson"),
     pch = 19,
     xlab = "Fitted values",
     ylab = "Pearson residuals",
     main = "Pearson residuals vs fitted")
abline(h = 0, lwd = 2)

plot(fitted(fit1), residuals(fit1, type = "deviance"),
     pch = 19,
     xlab = "Fitted values",
     ylab = "Deviance residuals",
     main = "Deviance residuals vs fitted")
abline(h = 0, lwd = 2)

qqnorm(residuals(fit1, type = "deviance"),
       main = "Q-Q plot of deviance residuals")
qqline(residuals(fit1, type = "deviance"), lwd = 2)

plot(hatvalues(fit1), residuals(fit1, type = "deviance"),
     pch = 19,
     xlab = "Leverage",
     ylab = "Deviance residuals",
     main = "Leverage vs deviance residuals")
abline(h = 0, lwd = 2)

par(mfrow = c(1, 1))

## Residuals by predictor
plot(warpbreaks$tension, residuals(fit1, type = "pearson"),
     xlab = "Tension",
     ylab = "Pearson residual",
     main = "Pearson residuals by tension")
abline(h = 0, lwd = 2)

cat("\nDiagnostic guidance:\n")
cat("* Residuals should not show strong systematic patterns.\n")
cat("* Very large residuals may indicate unusual observations or lack of fit.\n")
cat("* In count models, some asymmetry is common.\n")

## ------------------------------------------------------------
## 10. Optional: quasi-Poisson fit if overdispersion is present
## ------------------------------------------------------------
fit_qp <- glm(breaks ~ wool + tension,
              family = quasipoisson,
              data = warpbreaks)

cat("\n============================================================\n")
cat("QUASI-POISSON COMPARISON\n")
cat("============================================================\n")

cat("\n--- Standard errors: Poisson vs quasi-Poisson ---\n")
se_compare <- cbind(
  Poisson_SE = summary(fit1)$coefficients[, "Std. Error"],
  QuasiPoisson_SE = summary(fit_qp)$coefficients[, "Std. Error"]
)
print(round(se_compare, 4))

cat("\nInterpretation:\n")
cat("* If overdispersion is present, quasi-Poisson inflates the standard errors.\n")
cat("* Coefficient estimates stay the same, but inference becomes more honest.\n")

## ------------------------------------------------------------
## 11. Final teaching summary
## ------------------------------------------------------------
cat("\n============================================================\n")
cat("FINAL TEACHING SUMMARY\n")
cat("============================================================\n")
cat("1. Poisson regression is appropriate for count data when the mean-variance relationship is plausible.\n")
cat("2. The model uses a log link, so coefficients are interpreted multiplicatively.\n")
cat("3. Exponentiated coefficients are rate ratios / expected-count ratios.\n")
cat("4. Deviance and likelihood ratio tests compare nested models.\n")
cat("5. Overdispersion should always be checked in applied count-data analysis.\n")
cat("6. Residual and fitted-mean plots help assess model adequacy.\n")