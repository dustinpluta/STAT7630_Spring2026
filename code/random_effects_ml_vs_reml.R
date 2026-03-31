## ============================================================
## ML vs REML demo in lme4 using simulated data
## Goal:
##   1. Simulate clustered data from a random intercept model
##   2. Fit the same mixed model with REML and with ML
##   3. Compare fixed effects, variance components, and fitted values
##   4. Show why ML is used for comparing models with different fixed effects
## ============================================================

## install.packages("lme4")   # uncomment if needed
library(lme4)

set.seed(123)

## ------------------------------------------------------------
## 1. Simulate clustered data
## ------------------------------------------------------------
n_groups <- 30
n_per_group <- 12
N <- n_groups * n_per_group

group <- factor(rep(1:n_groups, each = n_per_group))

# Predictor
x <- rnorm(N, mean = 0, sd = 1)

# True parameters
beta0 <- 2.0
beta1 <- 1.5
sigma_u <- 1.2   # SD of random intercepts
sigma_e <- 1.0   # residual SD

# Random intercepts
u <- rnorm(n_groups, mean = 0, sd = sigma_u)
u_obs <- u[group]

# Response
y <- beta0 + beta1 * x + u_obs + rnorm(N, mean = 0, sd = sigma_e)

dat <- data.frame(y = y, x = x, group = group)

cat("\n--- First few rows of simulated data ---\n")
print(head(dat))

## ------------------------------------------------------------
## 2. Fit the SAME model using REML and ML
## ------------------------------------------------------------
# Random intercept model
fit_reml <- lmer(y ~ x + (1 | group), data = dat, REML = TRUE)
fit_ml   <- lmer(y ~ x + (1 | group), data = dat, REML = FALSE)

cat("\n============================================================\n")
cat("Same fixed-effects model fit by REML and ML\n")
cat("============================================================\n")

cat("\n--- REML fit summary ---\n")
print(summary(fit_reml))

cat("\n--- ML fit summary ---\n")
print(summary(fit_ml))

## ------------------------------------------------------------
## 3. Compare fixed effects estimates
## ------------------------------------------------------------
cat("\n--- Fixed effects estimates ---\n")
fixef_tab <- cbind(
  REML = fixef(fit_reml),
  ML   = fixef(fit_ml)
)
print(round(fixef_tab, 4))

cat("\nNote:\n")
cat("For the same model, REML and ML usually give very similar fixed-effect estimates.\n")

## ------------------------------------------------------------
## 4. Compare variance component estimates
## ------------------------------------------------------------
cat("\n--- Variance components ---\n")
vc_reml <- as.data.frame(VarCorr(fit_reml))
vc_ml   <- as.data.frame(VarCorr(fit_ml))

vc_tab <- data.frame(
  Component = vc_reml$grp,
  Term = vc_reml$var1,
  REML_SD = c(attr(VarCorr(fit_reml)[[1]], "stddev"), sigma(fit_reml)),
  ML_SD   = c(attr(VarCorr(fit_ml)[[1]], "stddev"), sigma(fit_ml))
)
print(round(vc_tab, 4))

cat("\nNote:\n")
cat("REML often gives slightly larger variance-component estimates than ML.\n")
cat("This reflects the fact that ML does not adjust for estimation of fixed effects.\n")

## ------------------------------------------------------------
## 5. Compare fitted values
## ------------------------------------------------------------
fitted_diff <- max(abs(fitted(fit_reml) - fitted(fit_ml)))
cat("\n--- Max absolute difference in fitted values (same model) ---\n")
print(round(fitted_diff, 8))

cat("\nNote:\n")
cat("The fitted values are usually extremely similar for the same model.\n")

## ------------------------------------------------------------
## 6. Compare random effects (BLUPs)
## ------------------------------------------------------------
cat("\n--- First 10 random intercept estimates ---\n")
ran_reml <- ranef(fit_reml)$group
ran_ml   <- ranef(fit_ml)$group

ran_tab <- data.frame(
  group = rownames(ran_reml),
  REML = ran_reml[, 1],
  ML   = ran_ml[, 1]
)

print(round(head(ran_tab, 10), 4))

## ------------------------------------------------------------
## 7. Demonstrate model comparison:
##    compare different fixed-effects structures
## ------------------------------------------------------------
# Add an irrelevant fixed effect to the dataset
set.seed(456)
dat$z <- rnorm(N)

# Two nested models with different fixed effects
fit1_reml <- lmer(y ~ x + (1 | group), data = dat, REML = TRUE)
fit2_reml <- lmer(y ~ x + z + (1 | group), data = dat, REML = TRUE)

fit1_ml <- lmer(y ~ x + (1 | group), data = dat, REML = FALSE)
fit2_ml <- lmer(y ~ x + z + (1 | group), data = dat, REML = FALSE)

cat("\n============================================================\n")
cat("Comparing models with different fixed effects\n")
cat("============================================================\n")

cat("\n--- REML fits: anova comparison ---\n")
print(anova(fit1_reml, fit2_reml))

cat("\n--- ML fits: anova comparison ---\n")
print(anova(fit1_ml, fit2_ml))

cat("\nImportant point:\n")
cat("For comparing models with different fixed effects, use ML, not REML.\n")
cat("The REML criterion depends on the fixed-effects design matrix, so REML log-likelihoods\n")
cat("are not directly comparable across models with different fixed effects.\n")

## ------------------------------------------------------------
## 8. A cleaner side-by-side table
## ------------------------------------------------------------
compare_model <- function(fit, label) {
  vc <- as.data.frame(VarCorr(fit))
  data.frame(
    Method = label,
    beta0 = fixef(fit)[1],
    beta1 = fixef(fit)[2],
    SD_group = attr(VarCorr(fit)[[1]], "stddev"),
    SD_resid = sigma(fit)
  )
}

comp_tab <- rbind(
  compare_model(fit_reml, "REML"),
  compare_model(fit_ml, "ML")
)

cat("\n--- Side-by-side comparison table ---\n")
print(round(comp_tab, 4))

## ------------------------------------------------------------
## 9. Teaching summary
## ------------------------------------------------------------
cat("\n============================================================\n")
cat("Teaching summary\n")
cat("============================================================\n")
cat("1. REML and ML often give similar fixed-effect estimates for the same model.\n")
cat("2. REML usually gives slightly less biased variance-component estimates.\n")
cat("3. REML is preferred for estimating variance components in a fixed model.\n")
cat("4. ML should be used when comparing models with different fixed effects.\n")
cat("5. Random effects are estimated by shrinkage (BLUPs) under either approach.\n")