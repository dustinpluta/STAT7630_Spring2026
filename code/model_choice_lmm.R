## ============================================================
## EXAMPLE 2: MODEL CHOICE FOR LONGITUDINAL CONTINUOUS DATA
## Dataset: sleepstudy (lme4)
## Goal: choose among pooled OLS, random intercept, random slope
## ============================================================

## install.packages("lme4")   # uncomment if needed
library(lme4)

data("sleepstudy", package = "lme4")

str(sleepstudy)
head(sleepstudy)

cat("\nNumber of subjects:\n")
print(length(unique(sleepstudy$Subject)))

## ------------------------------------------------------------
## 1. Exploratory plots
## ------------------------------------------------------------
## Overall scatterplot
plot(Reaction ~ Days, data = sleepstudy,
     pch = 19,
     xlab = "Days of sleep deprivation",
     ylab = "Reaction time",
     main = "Overall scatterplot")
abline(lm(Reaction ~ Days, data = sleepstudy), lwd = 2)

## Spaghetti plot by subject
subjects <- levels(sleepstudy$Subject)
cols <- rainbow(length(subjects))

plot(range(sleepstudy$Days), range(sleepstudy$Reaction),
     type = "n",
     xlab = "Days",
     ylab = "Reaction",
     main = "Spaghetti plot by Subject")

for (i in seq_along(subjects)) {
  dsub <- subset(sleepstudy, Subject == subjects[i])
  lines(dsub$Days, dsub$Reaction, col = cols[i], lwd = 1.5)
  points(dsub$Days, dsub$Reaction, col = cols[i], pch = 19, cex = 0.7)
}

cat("\nModel-choice reasoning from plots:\n")
cat("* Subjects clearly differ in baseline reaction time.\n")
cat("* Subjects also appear to differ in slope over Days.\n")
cat("* That suggests a mixed model, likely with random intercept and possibly random slope.\n")

## ------------------------------------------------------------
## 2. Incorrect baseline: pooled OLS
## ------------------------------------------------------------
fit_ols <- lm(Reaction ~ Days, data = sleepstudy)

cat("\n--- Pooled OLS summary ---\n")
print(summary(fit_ols))

par(mfrow = c(2, 2))
plot(fit_ols)
par(mfrow = c(1, 1))

cat("\nInterpretation:\n")
cat("* Pooled OLS ignores within-subject dependence.\n")
cat("* Standard errors and inference may be misleading.\n")
cat("* It also cannot represent subject-specific trajectories.\n")

## ------------------------------------------------------------
## 3. Fit mixed models
## ------------------------------------------------------------
## Random intercept model
fit_ri <- lmer(Reaction ~ Days + (1 | Subject),
               data = sleepstudy, REML = TRUE)

## Random intercept + random slope model
fit_ris <- lmer(Reaction ~ Days + (Days | Subject),
                data = sleepstudy, REML = TRUE)

cat("\n--- Random intercept model ---\n")
print(summary(fit_ri))

cat("\n--- Random intercept + random slope model ---\n")
print(summary(fit_ris))

## ------------------------------------------------------------
## 4. Visualize subject-specific lines
## ------------------------------------------------------------
coefs_subject <- coef(fit_ris)$Subject

plot(range(sleepstudy$Days), range(sleepstudy$Reaction),
     type = "n",
     xlab = "Days",
     ylab = "Reaction",
     main = "Subject-specific fitted lines\n(random intercept + slope model)")

for (i in seq_along(subjects)) {
  dsub <- subset(sleepstudy, Subject == subjects[i])
  b0 <- coefs_subject[subjects[i], "(Intercept)"]
  b1 <- coefs_subject[subjects[i], "Days"]
  lines(sort(dsub$Days), b0 + b1 * sort(dsub$Days), col = cols[i], lwd = 1.5)
  points(dsub$Days, dsub$Reaction, col = cols[i], pch = 19, cex = 0.7)
}

## ------------------------------------------------------------
## 5. Compare random-effects structures
## ------------------------------------------------------------
## Formal comparison should use ML (not REML) when comparing fitted likelihoods
fit_ri_ml  <- lmer(Reaction ~ Days + (1 | Subject),
                   data = sleepstudy, REML = FALSE)
fit_ris_ml <- lmer(Reaction ~ Days + (Days | Subject),
                   data = sleepstudy, REML = FALSE)

cat("\n--- Likelihood ratio comparison: RI vs RI+RS ---\n")
print(anova(fit_ri_ml, fit_ris_ml))

aic_tab <- data.frame(
  model = c("Pooled OLS", "Random intercept", "Random intercept + slope"),
  AIC = c(AIC(fit_ols), AIC(fit_ri_ml), AIC(fit_ris_ml))
)
print(aic_tab)

cat("\nModel-choice reasoning:\n")
cat("* If subject-specific slopes vary, the random slope model should fit better.\n")
cat("* The likelihood ratio test and AIC help assess whether that added complexity is warranted.\n")

## ------------------------------------------------------------
## 6. Diagnostics for final mixed model
## ------------------------------------------------------------
fit_final <- fit_ris

fitted_vals <- fitted(fit_final)
resid_vals <- resid(fit_final)

par(mfrow = c(2, 2))

plot(fitted_vals, resid_vals,
     pch = 19,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs fitted")
abline(h = 0, lwd = 2)

qqnorm(resid_vals, main = "Q-Q plot of residuals")
qqline(resid_vals, lwd = 2)

hist(resid_vals, breaks = 20,
     main = "Histogram of residuals",
     xlab = "Residuals")

ran <- ranef(fit_final)$Subject
plot(ran[, "(Intercept)"], ran[, "Days"],
     pch = 19,
     xlab = "Random intercepts",
     ylab = "Random slopes",
     main = "Estimated random effects")

par(mfrow = c(1, 1))

## ------------------------------------------------------------
## 7. ICC for random intercept model
## ------------------------------------------------------------
vc_ri <- as.data.frame(VarCorr(fit_ri))
sigma_u2 <- vc_ri$vcov[vc_ri$grp == "Subject"]
sigma_e2 <- vc_ri$vcov[vc_ri$grp == "Residual"]
icc <- sigma_u2 / (sigma_u2 + sigma_e2)

cat("\n--- Intraclass correlation from random intercept model ---\n")
print(icc)

cat("\nInterpretation:\n")
cat("* The ICC quantifies similarity of repeated measurements within subject.\n")
cat("* A nontrivial ICC confirms that a model for correlated data is needed.\n")

## ------------------------------------------------------------
## 8. Final interpretation
## ------------------------------------------------------------
cat("\n============================================================\n")
cat("EXAMPLE 2 SUMMARY: MODEL CHOICE\n")
cat("============================================================\n")
cat("1. Because the response is continuous but repeated within subject, ordinary regression is not enough.\n")
cat("2. The spaghetti plot reveals subject-level heterogeneity and dependence.\n")
cat("3. A mixed model is the appropriate model family.\n")
cat("4. The data then suggest choosing between random intercept only and random intercept + slope.\n")
cat("5. Final choice should be guided by design, plots, and model comparison.\n")