## ============================================================
## Polynomial Regression Demo (Graduate Applied Regression)
## Goals:
## 1) Simulate data with a true quadratic relationship
## 2) Fit (misspecified) linear model and show diagnostics
## 3) Fit quadratic model and show diagnostics improve
## 4) Compare non-centered vs centered polynomial terms
## 5) Partial F-test for the quadratic term (nested model test)
## 6) Compare VIFs (multicollinearity) centered vs non-centered
## ============================================================

set.seed(123)

## ----------------------------
## 0) Simulate data (true quadratic)
## ----------------------------
n <- 200
X <- runif(n, min = 0, max = 10)

# True mean function: concave down (quadratic)
beta0 <- 5
beta1 <- 2.0
beta2 <- -0.25
sigma <- 2.0

Y <- beta0 + beta1 * X + beta2 * X^2 + rnorm(n, sd = sigma)

dat <- data.frame(Y = Y, X = X)

## Helpful: visualize the raw pattern
plot(dat$X, dat$Y, pch = 19,
     xlab = "X", ylab = "Y",
     main = "Data generated from a quadratic mean function")

## ----------------------------
## 1) Fit misspecified linear model (omit X^2)
## ----------------------------
m_lin <- lm(Y ~ X, data = dat)
cat("\n--- Linear model (misspecified): Y ~ X ---\n")
print(summary(m_lin))

## Diagnostics: residual vs fitted, scale-location, QQ, leverage
par(mfrow = c(2, 2))
plot(m_lin)
par(mfrow = c(1, 1))

## A targeted diagnostic: residuals vs X (should show curvature)
plot(dat$X, resid(m_lin), pch = 19,
     xlab = "X", ylab = "Residuals",
     main = "Residuals vs X (Linear model)")
abline(h = 0, lwd = 2)

## Add a smooth curve to highlight curvature (base R loess)
ord <- order(dat$X)
lines(dat$X[ord], predict(loess(resid(m_lin) ~ X, data = dat))[ord], lwd = 2)

## ----------------------------
## 2) Fit quadratic model (include X^2) - non-centered
## ----------------------------
m_quad_raw <- lm(Y ~ X + I(X^2), data = dat)
cat("\n--- Quadratic model (raw powers): Y ~ X + X^2 ---\n")
print(summary(m_quad_raw))

## Partial F-test for the quadratic term (nested test)
cat("\n--- Partial F-test: add X^2 term ---\n")
print(anova(m_lin, m_quad_raw))

## Diagnostics should improve (residual pattern reduced)
par(mfrow = c(2, 2))
plot(m_quad_raw)
par(mfrow = c(1, 1))

plot(dat$X, resid(m_quad_raw), pch = 19,
     xlab = "X", ylab = "Residuals",
     main = "Residuals vs X (Quadratic model)")
abline(h = 0, lwd = 2)
lines(dat$X[ord], predict(loess(resid(m_quad_raw) ~ X, data = dat))[ord], lwd = 2)

## Overlay fitted curves for linear vs quadratic
plot(dat$X, dat$Y, pch = 19,
     xlab = "X", ylab = "Y",
     main = "Fitted curve: Linear vs Quadratic")
xgrid <- seq(min(dat$X), max(dat$X), length.out = 200)

yhat_lin  <- predict(m_lin, newdata = data.frame(X = xgrid))
yhat_quad <- predict(m_quad_raw, newdata = data.frame(X = xgrid))

lines(xgrid, yhat_lin,  lwd = 2, lty = 2)  # dashed
lines(xgrid, yhat_quad, lwd = 2)           # solid
legend("topright",
       legend = c("Linear fit", "Quadratic fit"),
       lty = c(2, 1), lwd = 2, bty = "n")

## ----------------------------
## 3) Center X and refit quadratic model
## ----------------------------
dat$Xc <- dat$X - mean(dat$X)

m_quad_cent <- lm(Y ~ Xc + I(Xc^2), data = dat)
cat("\n--- Quadratic model (centered): Y ~ Xc + Xc^2 ---\n")
print(summary(m_quad_cent))

## Note: Fit statistics should be identical (same fitted values),
## but coefficient interpretation changes.

cat("\n--- Compare fitted values: raw vs centered quadratic ---\n")
cat("Max absolute difference in fitted values: ",
    max(abs(fitted(m_quad_raw) - fitted(m_quad_cent))), "\n")

cat("\nInterpretation note:\n")
cat("* In the raw model, the X coefficient is the slope at X = 0.\n")
cat("* In the centered model, the Xc coefficient is the slope at X = mean(X).\n")

## Partial F-test for Xc^2 term (centered version)
m_lin_cent <- lm(Y ~ Xc, data = dat)
cat("\n--- Partial F-test (centered): add Xc^2 term ---\n")
print(anova(m_lin_cent, m_quad_cent))

## ----------------------------
## 4) Multicollinearity comparison: VIF raw vs centered
## ----------------------------
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

# Build the columns explicitly so we can compute VIFs on the same dataframe
dat$X2  <- dat$X^2
dat$Xc2 <- dat$Xc^2

cat("\n--- Manual VIFs: raw polynomial terms (X, X^2) ---\n")
print(vif_manual(dat, c("X", "X2")))

cat("\n--- Manual VIFs: centered polynomial terms (Xc, Xc^2) ---\n")
print(vif_manual(dat, c("Xc", "Xc2")))

cat("\nVIF note:\n")
cat("* Raw (X, X^2) are usually highly collinear -> large VIFs.\n")
cat("* Centering reduces corr(Xc, Xc^2) -> smaller VIFs and more stable SEs.\n")

## ----------------------------
## 5) Optional: show correlation reduction directly
## ----------------------------
cat("\n--- Correlations: raw vs centered ---\n")
cat("cor(X, X^2)   =", cor(dat$X,  dat$X2),  "\n")
cat("cor(Xc, Xc^2) =", cor(dat$Xc, dat$Xc2), "\n")

## ----------------------------
## 6) Key takeaways printed
## ----------------------------
cat("\n--- Key Takeaways ---\n")
cat("1) Omitting X^2 when true mean is quadratic yields patterned residuals (curvature).\n")
cat("2) Adding X^2 typically removes structure in residual plots and improves fit.\n")
cat("3) Partial F-test compares nested models to test whether X^2 adds explanatory power.\n")
cat("4) Centering does not change fitted values or the F-test, but improves interpretation and reduces multicollinearity.\n")