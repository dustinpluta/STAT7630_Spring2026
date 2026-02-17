## ============================================================
## Multicollinearity demo (single script)
## - Simulate near-collinear predictors
## - Fit full model
## - Compute VIFs (car + manual)
## - Check condition number
## - Drop a collinear covariate and show improvement
## ============================================================

set.seed(123)

n <- 200

# Core signal
X1 <- rnorm(n)

# Near-collinear predictor with X1
X2 <- 0.95 * X1 + rnorm(n, sd = 0.15)

# Mostly independent predictor
X3 <- rnorm(n)

# True model uses X1 and X3; X2 is redundant (nuisance / collinear)
beta0 <- 2
beta1 <- 1.5
beta2 <- 0.0
beta3 <- -1.0

eps <- rnorm(n, sd = 1.0)
Y <- beta0 + beta1 * X1 + beta2 * X2 + beta3 * X3 + eps

dat <- data.frame(Y, X1, X2, X3)

cat("\n--- Predictor correlations ---\n")
print(cor(dat[, c("X1","X2","X3")]))

# Fit full model
m_full <- lm(Y ~ X1 + X2 + X3, data = dat)

cat("\n--- Full model summary (Y ~ X1 + X2 + X3) ---\n")
print(summary(m_full))

# Manual VIF (no packages)
vif_manual <- function(df, predictors) {
  out <- numeric(length(predictors))
  names(out) <- predictors
  for (j in seq_along(predictors)) {
    xj <- predictors[j]
    others <- predictors[-j]
    form <- as.formula(paste(xj, "~", paste(others, collapse = " + ")))
    r2 <- summary(lm(form, data = df))$r.squared
    out[xj] <- 1 / (1 - r2)
  }
  out
}

cat("\n--- Manual VIFs (full model predictors) ---\n")
print(vif_manual(dat, c("X1","X2","X3")))

# car::vif (if available)
cat("\n--- car::vif (if package installed) ---\n")
if (requireNamespace("car", quietly = TRUE)) {
  print(car::vif(m_full))
} else {
  cat("Package 'car' not installed. To use: install.packages('car')\n")
}

# Condition number (design matrix without intercept)
X_full <- model.matrix(m_full)[, -1, drop = FALSE]
cat("\n--- Condition number (full design matrix, no intercept) ---\n")
print(kappa(X_full))

# Drop the collinear covariate X2 and refit
m_drop <- lm(Y ~ X1 + X3, data = dat)

cat("\n--- Reduced model summary (Y ~ X1 + X3) ---\n")
print(summary(m_drop))

cat("\n--- Manual VIFs (reduced model predictors) ---\n")
print(vif_manual(dat, c("X1","X3")))

cat("\n--- car::vif for reduced model (if installed) ---\n")
if (requireNamespace("car", quietly = TRUE)) {
  print(car::vif(m_drop))
}

X_drop <- model.matrix(m_drop)[, -1, drop = FALSE]
cat("\n--- Condition number (reduced design matrix, no intercept) ---\n")
print(kappa(X_drop))

# Side-by-side coefficient tables
coef_table <- function(model) {
  s <- summary(model)$coefficients
  data.frame(
    term = rownames(s),
    estimate = s[, "Estimate"],
    se = s[, "Std. Error"],
    t = s[, "t value"],
    p = s[, "Pr(>|t|)"],
    row.names = NULL
  )
}

tab_full <- coef_table(m_full)
tab_drop <- coef_table(m_drop)

cat("\n--- Coefficient table: Full model ---\n")
print(tab_full)

cat("\n--- Coefficient table: Reduced model ---\n")
print(tab_drop)

cat("\n--- Notes ---\n")
cat("* Expect large VIFs for X1 and X2 in the full model, inflated SEs, and possible sign instability.\n")
cat("* After dropping X2, VIFs should drop toward ~1 and SE for X1 should shrink noticeably.\n")
