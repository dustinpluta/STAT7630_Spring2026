## ============================================================
## MODEL SELECTION + COMPLEXITY CONTROL (SIMULATED DATA DEMO)
## Graduate applied regression workflow with known ground truth.
##
## Steps:
## 1) Theory-driven full model + a class of reduced models
## 2) Diagnose functional form (residual patterns / partial residual idea)
## 3) Partial F-tests for blocks (nested comparisons)
## 4) Compare AIC/BIC across models
## 5) Confirm with K-fold CV (predictive MSE)
## 6) Report model uncertainty (ΔAIC, Akaike weights, model-averaged preds,
##    and stability via bootstrap selection frequency)
## ============================================================

set.seed(2026)

## ----------------------------
## 0) Simulate data (truth includes: nonlinear term + interaction + categorical)
## ----------------------------
n <- 600

# Core continuous predictors
x1 <- rnorm(n)                 # "exposure" / main predictor
x2 <- rnorm(n)                 # "modifier" / another covariate

# Categorical predictor (3 levels)
g <- factor(sample(c("A","B","C"), n, replace = TRUE, prob = c(0.35, 0.40, 0.25)))

# Additional continuous covariates (some are noise, some weak signal)
z1 <- rnorm(n)
z2 <- rnorm(n)
z3 <- rnorm(n)
z4 <- rnorm(n)

# True mean structure:
# y = 2 + 1.2*x1 - 0.7*x2 + 0.8*x1*x2 + 0.9*(x2^2) + group effects + 0.3*z1 + noise
b0 <- 2
b1 <- 1.2
b2 <- -0.7
b12 <- 0.8        # interaction
b22 <- 0.9        # nonlinearity in x2
bz1 <- 0.3        # weak signal
sigma <- 1.2

# Group effects (baseline shifts)
g_eff <- ifelse(g == "A", 0,
                ifelse(g == "B", 1.0, -0.8))

y <- b0 +
  b1 * x1 +
  b2 * x2 +
  b12 * (x1 * x2) +
  b22 * (x2^2) +
  bz1 * z1 +
  g_eff +
  rnorm(n, sd = sigma)

dat <- data.frame(y, x1, x2, g, z1, z2, z3, z4)

## Split into train/test to mimic applied workflow
set.seed(99)
idx <- sample(seq_len(n), size = round(0.7 * n))
train <- dat[idx, ]
test  <- dat[-idx, ]

cat("\n====================\nGround truth:\n====================\n")
cat("True terms: x1, x2, x1:x2, x2^2, g, z1 (weak). z2,z3,z4 are noise.\n")

## ----------------------------
## 1) Theory-driven full model + reduced model class
## ----------------------------
cat("\n====================\n1) Candidate models\n====================\n")

# A plausible "full" model a subject-matter analyst might start with.
# (Includes main effects for everything, plus a plausible interaction and polynomial.)
m_full <- lm(y ~ x1 + x2 + I(x2^2) + x1:x2 + g + z1 + z2 + z3 + z4, data = train)

# Reduced candidates (theory-based simplifications / functional-form alternatives)
m_main_only   <- lm(y ~ x1 + x2 + g + z1 + z2 + z3 + z4, data = train)      # no interaction, no x2^2
m_add_poly    <- lm(y ~ x1 + x2 + I(x2^2) + g + z1 + z2 + z3 + z4, data = train)  # add curvature only
m_add_int     <- lm(y ~ x1 + x2 + x1:x2 + g + z1 + z2 + z3 + z4, data = train)   # add interaction only
m_core        <- lm(y ~ x1 + x2 + g + z1, data = train)                     # drop obvious noise
m_core_poly   <- lm(y ~ x1 + x2 + I(x2^2) + g + z1, data = train)
m_core_int    <- lm(y ~ x1 + x2 + x1:x2 + g + z1, data = train)
m_core_both   <- lm(y ~ x1 + x2 + I(x2^2) + x1:x2 + g + z1, data = train)    # close to truth

models <- list(
  full = m_full,
  main_only = m_main_only,
  add_poly = m_add_poly,
  add_int = m_add_int,
  core = m_core,
  core_poly = m_core_poly,
  core_int = m_core_int,
  core_both = m_core_both
)

## ----------------------------
## 2) Diagnose functional form (show what omission looks like)
## ----------------------------
cat("\n====================\n2) Functional form diagnostics\n====================\n")
cat("We fit a 'main-effects-only' model, then inspect residual structure vs x2 and x1*x2.\n")

par(mfrow = c(2, 2))
plot(m_main_only)   # base R diagnostic panel
par(mfrow = c(1, 1))

# Residuals vs x1
plot(train$x1, resid(m_main_only), pch = 19,
     xlab = "x1", ylab = "Residuals",
     main = "Residuals vs x1 (main-effects-only)")
abline(h = 0, lwd = 2)
ord <- order(train$x1)
lines(train$x1[ord], predict(loess(resid(m_main_only) ~ x1, data = train))[ord], lwd = 2)

# Residuals vs x2 (should show curvature if x2^2 is missing)
plot(train$x2, resid(m_main_only), pch = 19,
     xlab = "x2", ylab = "Residuals",
     main = "Residuals vs x2 (main-effects-only)")
abline(h = 0, lwd = 2)
ord <- order(train$x2)
lines(train$x2[ord], predict(loess(resid(m_main_only) ~ x2, data = train))[ord], lwd = 2)

# Residuals vs interaction proxy (x1*x2): should show structure if interaction missing
prod12 <- train$x1 * train$x2
plot(prod12, resid(m_main_only), pch = 19,
     xlab = "x1*x2", ylab = "Residuals",
     main = "Residuals vs x1*x2 (main-effects-only)")
abline(h = 0, lwd = 2)
ord2 <- order(prod12)
lines(prod12[ord2], predict(loess(resid(m_main_only) ~ prod12))[ord2], lwd = 2)

cat("\nInterpretation:\n")
cat("* Curvature in residuals vs x2 suggests missing nonlinear term like x2^2.\n")
cat("* Pattern vs x1*x2 suggests missing interaction x1:x2.\n")

## ----------------------------
## 3) Partial F-tests for blocks (nested model comparisons)
## ----------------------------
cat("\n====================\n3) Partial F-tests for blocks\n====================\n")

cat("\nBlock test: add x2^2 to main-effects-only (holding other terms fixed)\n")
print(anova(m_main_only, m_add_poly))  # tests I(x2^2)

cat("\nBlock test: add x1:x2 to main-effects-only\n")
print(anova(m_main_only, m_add_int))   # tests x1:x2

cat("\nBlock test: add BOTH {x2^2, x1:x2} to main-effects-only (2 df block)\n")
m_add_both <- lm(y ~ x1 + x2 + I(x2^2) + x1:x2 + g + z1 + z2 + z3 + z4, data = train)
print(anova(m_main_only, m_add_both))

cat("\nBlock test: noise covariates (z2,z3,z4) in presence of correct structure\n")
# compare core_both (no z2,z3,z4) to full (includes them)
print(anova(m_core_both, m_full))

cat("\nNotes:\n")
cat("* These are hypothesis tests about blocks; they do not directly optimize prediction.\n")
cat("* With large n, tiny effects can become significant; combine with AIC/CV.\n")

## ----------------------------
## 4) Compare AIC/BIC across models
## ----------------------------
cat("\n====================\n4) AIC/BIC comparison\n====================\n")

info <- data.frame(
  model = names(models),
  k = sapply(models, function(m) length(coef(m))),
  AIC = sapply(models, AIC),
  BIC = sapply(models, BIC),
  row.names = NULL
)

info$deltaAIC <- info$AIC - min(info$AIC)
w <- exp(-0.5 * info$deltaAIC)
info$akaike_wt <- w / sum(w)

info <- info[order(info$AIC), ]
print(info)

cat("\nΔAIC rule of thumb: 0–2 strong support; 4–7 less; >10 little.\n")

## ----------------------------
## 5) Confirm with K-fold CV (predictive performance)
## ----------------------------
cat("\n====================\n5) K-fold CV confirmation\n====================\n")

kfold_cv_mse <- function(form, data, K = 10, seed = 1) {
  set.seed(seed)
  n <- nrow(data)
  folds <- sample(rep(1:K, length.out = n))
  mse <- numeric(K)
  for (k in 1:K) {
    tr <- data[folds != k, , drop = FALSE]
    te <- data[folds == k, , drop = FALSE]
    fit <- lm(form, data = tr)
    pred <- predict(fit, newdata = te)
    mse[k] <- mean((te$y - pred)^2)
  }
  mean(mse)
}

cv <- data.frame(
  model = names(models),
  CV_MSE = sapply(models, function(m) kfold_cv_mse(formula(m), train, K = 10, seed = 2026)),
  row.names = NULL
)
cv <- cv[order(cv$CV_MSE), ]
print(cv)

cat("\nOptional: evaluate on the held-out TEST set as a final check.\n")
test_mse <- sapply(models, function(m) {
  pred <- predict(m, newdata = test)
  mean((test$y - pred)^2)
})
test_tab <- data.frame(model = names(models), TEST_MSE = as.numeric(test_mse))
test_tab <- test_tab[order(test_tab$TEST_MSE), ]
print(test_tab)

## ----------------------------
## 6) Report model uncertainty
## ----------------------------
cat("\n====================\n6) Model uncertainty reporting\n====================\n")

# (A) "Confidence set" of models with ΔAIC <= 2
conf_set <- subset(info, deltaAIC <= 2, select = c(model, k, AIC, deltaAIC, akaike_wt))
if (nrow(conf_set) == 0) {
  conf_set <- subset(info, deltaAIC <= 4, select = c(model, k, AIC, deltaAIC, akaike_wt))
  cat("\nNo models within ΔAIC <= 2; using ΔAIC <= 4.\n")
}
cat("\nAIC confidence set:\n")
print(conf_set)

# (B) Model-averaged prediction at a representative covariate profile
x0 <- data.frame(
  x1 = 0,
  x2 = 0,
  g  = factor("B", levels = levels(train$g)),
  z1 = 0, z2 = 0, z3 = 0, z4 = 0
)

# Align weights to models list order
wts <- info$akaike_wt[match(names(models), info$model)]
preds <- sapply(models, function(m) as.numeric(predict(m, newdata = x0)))
pred_avg <- sum(wts * preds)

cat("\nRepresentative x0:\n")
print(x0)
cat("\nPredictions by model:\n")
print(round(preds, 3))
cat("\nAkaike-weighted model-averaged prediction at x0:", round(pred_avg, 3), "\n")

# (C) Stability: bootstrap selection frequency (AIC winner over resamples)
cat("\nBootstrap stability: how often each model is best by AIC (train resamples)\n")

best_by_aic <- function(data_boot) {
  fits <- lapply(models, function(m) lm(formula(m), data = data_boot))
  aics <- sapply(fits, AIC)
  names(aics)[which.min(aics)]
}

set.seed(7)
B <- 200
winners <- character(B)
ntr <- nrow(train)
for (b in 1:B) {
  ii <- sample(seq_len(ntr), size = ntr, replace = TRUE)
  winners[b] <- best_by_aic(train[ii, , drop = FALSE])
}
freq <- sort(table(winners) / B, decreasing = TRUE)
print(freq)

cat("\nSuggested reporting language (template):\n")
cat("- We specified a theory-driven full model and a structured set of reduced alternatives.\n")
cat("- Diagnostics of the main-effects model suggested missing curvature in x2 and effect modification (x1:x2).\n")
cat("- Partial F-tests supported adding x2^2 and x1:x2; noise terms (z2–z4) were not supported once structure was included.\n")
cat("- AIC/BIC and CV/test MSE were used to balance fit vs complexity; multiple models may have comparable support.\n")
cat("- We report model uncertainty via ΔAIC/Akaike weights, model-averaged predictions, and bootstrap selection frequency.\n")

