## ============================================================
## Classroom demo: random effects models using sleepstudy
## Data: lme4::sleepstudy
## Goals:
##   1. Visualize group structure and within-subject dependence
##   2. Use plots to motivate random intercepts and random slopes
##   3. Fit mixed models with REML
##   4. Compare candidate random-effects structures
##   5. Conduct diagnostics
## ============================================================

## install.packages("lme4")   # uncomment if needed
library(lme4)

## ------------------------------------------------------------
## 1. Load data and inspect structure
## ------------------------------------------------------------
data("sleepstudy", package = "lme4")

str(sleepstudy)
head(sleepstudy)

cat("\n--- Basic structure ---\n")
cat("Number of observations:", nrow(sleepstudy), "\n")
cat("Number of subjects:", length(unique(sleepstudy$Subject)), "\n")
cat("Days observed per subject:\n")
print(table(sleepstudy$Subject))

## Variables:
## Reaction = reaction time
## Days     = number of days of sleep deprivation
## Subject  = subject ID

## ------------------------------------------------------------
## 2. Basic exploratory plots
## ------------------------------------------------------------

## Overall scatterplot
plot(Reaction ~ Days, data = sleepstudy,
     pch = 19,
     xlab = "Days of sleep deprivation",
     ylab = "Reaction time",
     main = "Overall scatterplot: Reaction vs Days")

## Add pooled OLS line
abline(lm(Reaction ~ Days, data = sleepstudy), lwd = 2)

cat("\nInterpretation:\n")
cat("* There is an overall upward trend: reaction time tends to increase with Days.\n")
cat("* But this plot ignores that measurements are repeated within Subject.\n")

## ------------------------------------------------------------
## 3. Spaghetti plot: key plot for group structure
## ------------------------------------------------------------

subjects <- levels(sleepstudy$Subject)
cols <- rainbow(length(subjects))

plot(range(sleepstudy$Days), range(sleepstudy$Reaction),
     type = "n",
     xlab = "Days of sleep deprivation",
     ylab = "Reaction time",
     main = "Spaghetti plot by Subject")

for (i in seq_along(subjects)) {
  dsub <- subset(sleepstudy, Subject == subjects[i])
  lines(dsub$Days, dsub$Reaction, col = cols[i], lwd = 1.5)
  points(dsub$Days, dsub$Reaction, col = cols[i], pch = 19, cex = 0.7)
}

cat("\nInterpretation of spaghetti plot:\n")
cat("* Subjects clearly differ in baseline reaction time at Day 0.\n")
cat("* Subjects also appear to differ in the rate of increase over Days.\n")
cat("* This suggests both a random intercept and possibly a random slope.\n")

## ------------------------------------------------------------
## 4. Subject-specific OLS fits to visualize variation in slopes
## ------------------------------------------------------------

plot(range(sleepstudy$Days), range(sleepstudy$Reaction),
     type = "n",
     xlab = "Days of sleep deprivation",
     ylab = "Reaction time",
     main = "Subject-specific regression lines")

for (i in seq_along(subjects)) {
  dsub <- subset(sleepstudy, Subject == subjects[i])
  fit_sub <- lm(Reaction ~ Days, data = dsub)
  abline(fit_sub, col = cols[i], lwd = 1.5)
  points(dsub$Days, dsub$Reaction, col = cols[i], pch = 19, cex = 0.7)
}

cat("\nInterpretation of subject-specific lines:\n")
cat("* Intercepts vary noticeably across subjects.\n")
cat("* Slopes also vary, though perhaps less dramatically than intercepts.\n")
cat("* This motivates trying:\n")
cat("    1. random intercept model\n")
cat("    2. random intercept + random slope model\n")

## ------------------------------------------------------------
## 5. Boxplots by Subject to show between-subject variation
## ------------------------------------------------------------

boxplot(Reaction ~ Subject, data = sleepstudy,
        las = 2,
        xlab = "Subject",
        ylab = "Reaction time",
        main = "Reaction time distributions by Subject")

cat("\nInterpretation of boxplots:\n")
cat("* Distributions differ across subjects, supporting subject-level heterogeneity.\n")
cat("* This is incompatible with a simple iid-error linear regression model.\n")

## ------------------------------------------------------------
## 6. Compare to pooled OLS (incorrect independence assumption)
## ------------------------------------------------------------

fit_ols <- lm(Reaction ~ Days, data = sleepstudy)

cat("\n--- Pooled OLS summary ---\n")
print(summary(fit_ols))

par(mfrow = c(2, 2))
plot(fit_ols)
par(mfrow = c(1, 1))

cat("\nWhy pooled OLS is inadequate:\n")
cat("* It ignores repeated measures within Subject.\n")
cat("* Residuals are treated as independent, which is not appropriate here.\n")
cat("* Any subject-level structure is forced into the residuals.\n")

## ------------------------------------------------------------
## 7. Fit mixed models using REML
## ------------------------------------------------------------

## Random intercept model
fit_ri <- lmer(Reaction ~ Days + (1 | Subject),
               data = sleepstudy, REML = TRUE)

## Random intercept + random slope model
fit_ris <- lmer(Reaction ~ Days + (Days | Subject),
                data = sleepstudy, REML = TRUE)

cat("\n--- Random intercept model (REML) ---\n")
print(summary(fit_ri))

cat("\n--- Random intercept + random slope model (REML) ---\n")
print(summary(fit_ris))

cat("\nInterpretation:\n")
cat("* In the random intercept model, each subject gets their own baseline level.\n")
cat("* In the random slope model, each subject gets their own baseline and slope.\n")
cat("* Because the spaghetti plot showed both vertical shifts and slope differences,\n")
cat("  the random slope model is often more scientifically appropriate.\n")

## ------------------------------------------------------------
## 8. Extract and interpret variance components
## ------------------------------------------------------------

cat("\n--- Variance components: random intercept model ---\n")
print(VarCorr(fit_ri), comp = c("Variance", "Std.Dev."))

cat("\n--- Variance components: random intercept + slope model ---\n")
print(VarCorr(fit_ris), comp = c("Variance", "Std.Dev."))

## ICC for random intercept model
vc_ri <- as.data.frame(VarCorr(fit_ri))
sigma_u2 <- vc_ri$vcov[vc_ri$grp == "Subject"]
sigma_e2 <- vc_ri$vcov[vc_ri$grp == "Residual"]
icc <- sigma_u2 / (sigma_u2 + sigma_e2)

cat("\n--- Intraclass correlation (random intercept model) ---\n")
cat("ICC =", round(icc, 3), "\n")

cat("\nInterpretation of ICC:\n")
cat("* ICC is the correlation between two measurements from the same subject.\n")
cat("* A nontrivial ICC confirms within-subject dependence.\n")

## ------------------------------------------------------------
## 9. Visualize fitted subject-specific lines from mixed model
## ------------------------------------------------------------

## Extract subject-specific coefficients from random slope model
coefs_subject <- coef(fit_ris)$Subject

plot(range(sleepstudy$Days), range(sleepstudy$Reaction),
     type = "n",
     xlab = "Days of sleep deprivation",
     ylab = "Reaction time",
     main = "Fitted subject-specific lines from mixed model")

for (i in seq_along(subjects)) {
  dsub <- subset(sleepstudy, Subject == subjects[i])
  b0 <- coefs_subject[subjects[i], "(Intercept)"]
  b1 <- coefs_subject[subjects[i], "Days"]
  lines(sort(dsub$Days), b0 + b1 * sort(dsub$Days), col = cols[i], lwd = 1.5)
  points(dsub$Days, dsub$Reaction, col = cols[i], pch = 19, cex = 0.7)
}

cat("\nInterpretation:\n")
cat("* These are partially pooled subject-specific lines.\n")
cat("* Compared with separate OLS fits by subject, they are shrunk toward the population mean trend.\n")

## ------------------------------------------------------------
## 10. Compare random effects structures
## NOTE:
## To compare different random-effects structures, many instructors use REML
## if the fixed-effects structure is unchanged.
## ------------------------------------------------------------

cat("\n--- Model comparison by REML criterion ---\n")
cat("Random intercept model REML criterion:",
    deviance(fit_ri), "\n")
cat("Random intercept + slope model REML criterion:",
    deviance(fit_ris), "\n")

cat("\nCaution:\n")
cat("* Lower REML criterion suggests better fit when comparing models with the same fixed effects.\n")
cat("* For formal likelihood-ratio comparison of random-effects structures, one often refits using ML.\n")

## Optional formal comparison by ML for teaching
fit_ri_ml  <- lmer(Reaction ~ Days + (1 | Subject),
                   data = sleepstudy, REML = FALSE)
fit_ris_ml <- lmer(Reaction ~ Days + (Days | Subject),
                   data = sleepstudy, REML = FALSE)

cat("\n--- Formal comparison of random-effects structures using ML ---\n")
print(anova(fit_ri_ml, fit_ris_ml))

## ------------------------------------------------------------
## 11. Diagnostics for the final model
## We'll use the random intercept + slope model as the working final model
## ------------------------------------------------------------

fit_final <- fit_ris

## Marginal fitted values and residuals
fitted_vals <- fitted(fit_final)
resid_vals  <- resid(fit_final)

## Residuals vs fitted
plot(fitted_vals, resid_vals,
     pch = 19,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Mixed model: residuals vs fitted")
abline(h = 0, lwd = 2)

## QQ plot of residuals
qqnorm(resid_vals, main = "Mixed model: normal Q-Q plot of residuals")
qqline(resid_vals, lwd = 2)

## Histogram of residuals
hist(resid_vals, breaks = 20,
     main = "Mixed model: residual histogram",
     xlab = "Residuals")

## ------------------------------------------------------------
## 12. Diagnostics for random effects
## ------------------------------------------------------------

ran <- ranef(fit_final)$Subject

## QQ plot for random intercepts
qqnorm(ran[, "(Intercept)"],
       main = "Q-Q plot of random intercepts")
qqline(ran[, "(Intercept)"], lwd = 2)

## QQ plot for random slopes
qqnorm(ran[, "Days"],
       main = "Q-Q plot of random slopes")
qqline(ran[, "Days"], lwd = 2)

## Random intercept vs random slope
plot(ran[, "(Intercept)"], ran[, "Days"],
     pch = 19,
     xlab = "Random intercepts",
     ylab = "Random slopes",
     main = "Estimated random intercepts vs slopes")

cat("\nDiagnostic interpretation:\n")
cat("* Residuals vs fitted: check for nonlinearity and heteroskedasticity.\n")
cat("* Residual Q-Q: check approximate normality of level-1 errors.\n")
cat("* Random effects Q-Q plots: check approximate normality of subject effects.\n")
cat("* Random intercept vs slope plot: inspect dependence between subject baselines and subject slopes.\n")

## ------------------------------------------------------------
## 13. Subject-level residual patterns
## ------------------------------------------------------------

plot(range(sleepstudy$Days), range(resid_vals),
     type = "n",
     xlab = "Days",
     ylab = "Residuals",
     main = "Residuals by subject over time")

for (i in seq_along(subjects)) {
  dsub <- subset(sleepstudy, Subject == subjects[i])
  rsub <- resid(fit_final)[sleepstudy$Subject == subjects[i]]
  lines(dsub$Days, rsub, col = cols[i], lwd = 1.2)
  points(dsub$Days, rsub, col = cols[i], pch = 19, cex = 0.7)
}
abline(h = 0, lwd = 2)

cat("\nInterpretation:\n")
cat("* Look for systematic time patterns within subject that would suggest remaining misspecification.\n")
cat("* If residual trajectories are roughly centered around zero without strong structure,\n")
cat("  the model is doing a reasonable job.\n")

## ------------------------------------------------------------
## 14. Clean summary for class
## ------------------------------------------------------------

cat("\n============================================================\n")
cat("CLASSROOM TAKEAWAYS\n")
cat("============================================================\n")
cat("1. Spaghetti plots reveal repeated-measures structure and subject heterogeneity.\n")
cat("2. Variation in starting level suggests a random intercept.\n")
cat("3. Variation in subject-specific slopes suggests a random slope for Days.\n")
cat("4. Fit mixed models with REML for estimation of variance components.\n")
cat("5. Use residual and random-effects diagnostics just as in ordinary regression,\n")
cat("   but now at both the observation and group levels.\n")
cat("6. The mixed model captures within-subject dependence and yields subject-specific\n")
cat("   trajectories through partial pooling.\n")