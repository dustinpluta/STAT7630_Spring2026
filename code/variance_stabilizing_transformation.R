############################################################
# Fixing non-constant variance in simple linear regression:
#  (1) Weighted least squares (WLS)
#  (2) Variance-stabilizing transformations (log / sqrt / Box-Cox)
############################################################

set.seed(1)

############################################################
# Helper: quick diagnostic plots
############################################################
diag_plots <- function(fit, main_prefix = "") {
  op <- par(mfrow = c(1, 2))
  on.exit(par(op), add = TRUE)
  plot(fitted(fit), resid(fit),
       xlab = "Fitted", ylab = "Residuals",
       main = paste0(main_prefix, "Residuals vs Fitted"))
  abline(h = 0, lty = 2)
  plot(fitted(fit), sqrt(abs(rstandard(fit))),
       xlab = "Fitted", ylab = "Sqrt(|Std resid|)",
       main = paste0(main_prefix, "Scale-Location"))
}

############################################################
# Scenario A: Variance increases with x (Var(e|x) ∝ x^2)
# Best fix: WLS with weights ~ 1 / x^2
############################################################
n <- 250
x <- runif(n, 1, 10)  # start at 1 to avoid division issues
beta0 <- 2
beta1 <- 1.2

# Heteroskedastic errors: sd proportional to x
y <- beta0 + beta1 * x + rnorm(n, sd = 0.6 * x)

ols_A <- lm(y ~ x)
diag_plots(ols_A, "A (OLS): ")

# True/known variance function => WLS weights proportional to 1/Var(e|x)
# Here Var(e|x) ≈ (0.6*x)^2, so weights ∝ 1/x^2 (constant factors cancel).
wls_A <- lm(y ~ x, weights = 1 / (x^2))
diag_plots(wls_A, "A (WLS): ")

cat("\nScenario A coefficients:\n")
print(rbind(OLS = coef(ols_A), WLS = coef(wls_A)))

############################################################
# Scenario B: Variance increases with mean (Var(Y|x) ∝ E[Y|x]^2)
# Typical when response is positive and multiplicative noise:
#   Y = mu(x) * exp(error)
# Best fix: log transform (variance stabilizing), then OLS on log(Y)
############################################################

x2 <- runif(n, 0, 10)
mu <- exp(0.4 + 0.12 * x2)         # positive mean that grows with x2
y2 <- mu * exp(rnorm(n, sd = 0.35)) # multiplicative noise => Var grows with mean^2

ols_B <- lm(y2 ~ x2)
diag_plots(ols_B, "B (OLS): ")

# Variance-stabilizing: log transform makes multiplicative errors additive
logfit_B <- lm(log(y2) ~ x2)
diag_plots(logfit_B, "B (log-OLS): ")

cat("\nScenario B coefficients (on log scale):\n")
print(coef(logfit_B))

# Back-transform fitted mean on original scale (optional):
# For lognormal, E[Y|x] approx exp(eta + 0.5*sigma^2)
sigma2_hat <- summary(logfit_B)$sigma^2
pred_mu <- exp(predict(logfit_B) + 0.5 * sigma2_hat)

############################################################
# Scenario C: Count-like data (Var ≈ mean) - Poisson-ish
# Demonstrate sqrt transform (classic variance-stabilizing)
############################################################

x3 <- runif(n, 0, 10)
lambda <- exp(-0.2 + 0.18 * x3)     # mean of counts
y3 <- rpois(n, lambda = lambda)

ols_C <- lm(y3 ~ x3)
diag_plots(ols_C, "C (OLS): ")

# Variance-stabilizing for Poisson counts: sqrt(Y + 3/8) is a classic choice
sqrtfit_C <- lm(sqrt(y3 + 3/8) ~ x3)
diag_plots(sqrtfit_C, "C (sqrt-OLS): ")

cat("\nScenario C coefficients (sqrt scale):\n")
print(coef(sqrtfit_C))

############################################################
# Optional: data-driven transformation via Box-Cox (MASS)
# Useful when you suspect a power transform is needed but not sure which.
############################################################
if (!requireNamespace("MASS", quietly = TRUE)) {
  cat("\nInstall MASS for Box-Cox: install.packages('MASS')\n")
} else {
  library(MASS)
  
  # Box-Cox needs strictly positive response; use scenario B (positive y2)
  bc <- boxcox(lm(y2 ~ x2), plotit = FALSE)
  lambda_hat <- bc$x[which.max(bc$y)]
  cat("\nBox-Cox suggested lambda (Scenario B):", round(lambda_hat, 3), "\n")
  
  # Fit with suggested power transform
  # When lambda is near 0, interpret as log transform.
  y2_bc <- if (abs(lambda_hat) < 0.05) log(y2) else (y2^lambda_hat - 1) / lambda_hat
  bcfit <- lm(y2_bc ~ x2)
  diag_plots(bcfit, "B (Box-Cox OLS): ")
}

############################################################
# Notes / interpretation
############################################################
cat("
\nInterpretation notes:
- WLS is appropriate when you can model Var(e|x) up to a proportionality constant.
  You supply weights w_i ∝ 1 / Var(e_i|x_i). Inference is then for the same mean model,
  but with improved efficiency and corrected standard errors (under the variance model).
- Variance-stabilizing transforms (log, sqrt, power) are appropriate when the variance
  is a function of the mean (multiplicative noise, counts, etc.). You change the model
  scale so that errors are closer to homoskedastic and Gaussian.
- Always re-check diagnostics after the fix, and interpret coefficients on the correct scale.
\n")
