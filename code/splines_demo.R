## ============================================================
## SPLINE FITTING AND DIAGNOSTICS DEMO USING SYNTHETIC DATA
## Goals:
##   1. Simulate nonlinear data with heteroskedastic noise
##   2. Fit linear, polynomial, and spline models
##   3. Compare fitted curves visually
##   4. Examine residual diagnostics
##   5. Compare models using test MSE and AIC
##   6. Illustrate effect of spline flexibility (degrees of freedom)
##
## Requires:
##   splines  (comes with base R recommended packages)
## ============================================================

set.seed(123)

library(splines)

## ------------------------------------------------------------
## 1. Simulate synthetic data
## ------------------------------------------------------------
n <- 250
x <- sort(runif(n, 0, 10))

## True mean function: nonlinear, with local features
f_true <- function(x) {
  2 +
    0.6 * x -
    0.08 * x^2 +
    1.8 * exp(-0.5 * (x - 3.0)^2) -
    1.2 * exp(-0.8 * (x - 7.5)^2)
}

mu <- f_true(x)

## Add mild heteroskedasticity so diagnostics are interesting
sigma_x <- 0.5 + 0.12 * x
y <- mu + rnorm(n, sd = sigma_x)

dat <- data.frame(x = x, y = y, mu = mu, sigma_x = sigma_x)

## Create train/test split
set.seed(456)
train_id <- sample(seq_len(n), size = round(0.7 * n))
train <- dat[train_id, ]
test  <- dat[-train_id, ]

## ------------------------------------------------------------
## 2. Initial visualization
## ------------------------------------------------------------
plot(dat$x, dat$y,
     pch = 19, cex = 0.7,
     xlab = "x", ylab = "y",
     main = "Synthetic nonlinear data")
lines(dat$x, dat$mu, lwd = 3)

cat("\nThe solid curve is the true mean function used to generate the data.\n")

## ------------------------------------------------------------
## 3. Fit candidate models
## ------------------------------------------------------------
## Linear model
m_lin <- lm(y ~ x, data = train)

## Quadratic polynomial
m_quad <- lm(y ~ x + I(x^2), data = train)

## Cubic polynomial
m_cubic <- lm(y ~ x + I(x^2) + I(x^3), data = train)

## Natural cubic splines with different degrees of freedom
m_ns_4 <- lm(y ~ ns(x, df = 4), data = train)
m_ns_6 <- lm(y ~ ns(x, df = 6), data = train)
m_ns_10 <- lm(y ~ ns(x, df = 10), data = train)

## B-spline example
m_bs_6 <- lm(y ~ bs(x, df = 6), data = train)

models <- list(
  linear = m_lin,
  quadratic = m_quad,
  cubic = m_cubic,
  ns_df4 = m_ns_4,
  ns_df6 = m_ns_6,
  ns_df10 = m_ns_10,
  bs_df6 = m_bs_6
)

## ------------------------------------------------------------
## 4. Overlay fitted curves
## ------------------------------------------------------------
x_grid <- seq(min(dat$x), max(dat$x), length.out = 400)
grid_df <- data.frame(x = x_grid)

pred_lin   <- predict(m_lin,   newdata = grid_df)
pred_quad  <- predict(m_quad,  newdata = grid_df)
pred_cubic <- predict(m_cubic, newdata = grid_df)
pred_ns4   <- predict(m_ns_4,  newdata = grid_df)
pred_ns6   <- predict(m_ns_6,  newdata = grid_df)
pred_ns10  <- predict(m_ns_10, newdata = grid_df)
pred_bs6   <- predict(m_bs_6,  newdata = grid_df)

par(mfrow = c(2, 2))

plot(dat$x, dat$y,
     pch = 19, cex = 0.5,
     xlab = "x", ylab = "y",
     main = "Linear vs quadratic vs cubic")
lines(x_grid, f_true(x_grid), lwd = 3)
lines(x_grid, pred_lin, lwd = 2, lty = 2)
lines(x_grid, pred_quad, lwd = 2, lty = 3)
lines(x_grid, pred_cubic, lwd = 2, lty = 4)
legend("topright",
       legend = c("True mean", "Linear", "Quadratic", "Cubic"),
       lty = c(1, 2, 3, 4), lwd = c(3, 2, 2, 2), bty = "n", cex = 0.8)

plot(dat$x, dat$y,
     pch = 19, cex = 0.5,
     xlab = "x", ylab = "y",
     main = "Natural splines with varying df")
lines(x_grid, f_true(x_grid), lwd = 3)
lines(x_grid, pred_ns4,  lwd = 2, lty = 2)
lines(x_grid, pred_ns6,  lwd = 2, lty = 3)
lines(x_grid, pred_ns10, lwd = 2, lty = 4)
legend("topright",
       legend = c("True mean", "ns(df=4)", "ns(df=6)", "ns(df=10)"),
       lty = c(1, 2, 3, 4), lwd = c(3, 2, 2, 2), bty = "n", cex = 0.8)

plot(dat$x, dat$y,
     pch = 19, cex = 0.5,
     xlab = "x", ylab = "y",
     main = "Natural spline vs B-spline")
lines(x_grid, f_true(x_grid), lwd = 3)
lines(x_grid, pred_ns6, lwd = 2, lty = 2)
lines(x_grid, pred_bs6, lwd = 2, lty = 3)
legend("topright",
       legend = c("True mean", "ns(df=6)", "bs(df=6)"),
       lty = c(1, 2, 3), lwd = c(3, 2, 2), bty = "n", cex = 0.8)

plot(train$x, resid(m_ns_6),
     pch = 19, cex = 0.7,
     xlab = "x", ylab = "Residuals",
     main = "Residuals vs x for ns(df=6)")
abline(h = 0, lwd = 2)

par(mfrow = c(1, 1))

cat("\nVisual comparison points:\n")
cat("* Linear and low-order polynomial fits may miss local structure.\n")
cat("* Splines adapt locally and usually capture the shape better.\n")
cat("* Very flexible splines can begin to chase noise if df is too large.\n")

## ------------------------------------------------------------
## 5. Numeric comparison: AIC and test MSE
## ------------------------------------------------------------
mse <- function(y, yhat) mean((y - yhat)^2)

perf_tab <- data.frame(
  model = names(models),
  AIC = sapply(models, AIC),
  train_MSE = sapply(models, function(m) mse(train$y, fitted(m))),
  test_MSE = sapply(models, function(m) mse(test$y, predict(m, newdata = test)))
)

perf_tab <- perf_tab[order(perf_tab$test_MSE), ]
cat("\n--- Model performance summary ---\n")
print(round(perf_tab, 3), row.names = FALSE)

cat("\nInterpretation:\n")
cat("* Compare train MSE and test MSE to assess out-of-sample behavior.\n")
cat("* The best spline often balances flexibility and generalization.\n")

## ------------------------------------------------------------
## 6. Detailed diagnostics for selected models
## ------------------------------------------------------------
## We'll compare a misspecified model (linear), a reasonable spline,
## and an overly flexible spline.
selected_models <- list(
  linear = m_lin,
  ns_df6 = m_ns_6,
  ns_df10 = m_ns_10
)

for (nm in names(selected_models)) {
  fit <- selected_models[[nm]]
  
  cat("\n============================================================\n")
  cat("Diagnostics for model:", nm, "\n")
  cat("============================================================\n")
  print(summary(fit))
  
  par(mfrow = c(2, 2))
  plot(fit, main = nm)
  par(mfrow = c(1, 1))
  
  ## Residuals vs x
  plot(train$x, resid(fit),
       pch = 19, cex = 0.7,
       xlab = "x", ylab = "Residuals",
       main = paste("Residuals vs x:", nm))
  abline(h = 0, lwd = 2)
  
  ## Add loess smooth to emphasize pattern
  ord <- order(train$x)
  smooth_vals <- predict(loess(resid(fit) ~ x, data = train))
  lines(train$x[ord], smooth_vals[ord], lwd = 2)
  
  ## Absolute residuals vs fitted to inspect variance pattern
  plot(fitted(fit), abs(resid(fit)),
       pch = 19, cex = 0.7,
       xlab = "Fitted values", ylab = "|Residuals|",
       main = paste("|Residuals| vs fitted:", nm))
}

cat("\nDiagnostic guidance:\n")
cat("* If residuals vs x show systematic curvature, the mean model is misspecified.\n")
cat("* If residual spread grows with fitted values, variance may be non-constant.\n")
cat("* QQ plot assesses approximate normality of residuals.\n")
cat("* Residuals vs leverage helps identify influential observations.\n")

## ------------------------------------------------------------
## 7. Confidence bands for a chosen spline fit
## ------------------------------------------------------------
## We compute pointwise confidence intervals for the mean function.
pred_obj <- predict(m_ns_6, newdata = grid_df, se.fit = TRUE)

upper <- pred_obj$fit + 1.96 * pred_obj$se.fit
lower <- pred_obj$fit - 1.96 * pred_obj$se.fit

plot(dat$x, dat$y,
     pch = 19, cex = 0.5,
     xlab = "x", ylab = "y",
     main = "Natural spline fit with pointwise 95% CI")
lines(x_grid, f_true(x_grid), lwd = 3)
lines(x_grid, pred_obj$fit, lwd = 2, lty = 2)
lines(x_grid, upper, lwd = 1, lty = 3)
lines(x_grid, lower, lwd = 1, lty = 3)
legend("topright",
       legend = c("True mean", "Spline fit", "95% pointwise CI"),
       lty = c(1, 2, 3), lwd = c(3, 2, 1), bty = "n", cex = 0.8)

## ------------------------------------------------------------
## 8. Show basis functions for natural spline and B-spline
## ------------------------------------------------------------
## This helps students see that splines are linear models in basis functions.
X_ns <- ns(train$x, df = 6)
X_bs <- bs(train$x, df = 6)

par(mfrow = c(1, 2))

matplot(train$x, X_ns,
        type = "l", lty = 1, lwd = 2,
        xlab = "x", ylab = "Basis value",
        main = "Natural spline basis (df=6)")

matplot(train$x, X_bs,
        type = "l", lty = 1, lwd = 2,
        xlab = "x", ylab = "Basis value",
        main = "B-spline basis (df=6)")

par(mfrow = c(1, 1))

cat("\nBasis-function interpretation:\n")
cat("* Splines are still linear models: y = B(x) beta + error.\n")
cat("* The nonlinearity comes from the basis expansion, not from nonlinear estimation.\n")

## ------------------------------------------------------------
## 9. Explicit design matrix regression check
## ------------------------------------------------------------
## Show equivalence between lm(y ~ ns(x, df=6)) and regressing on basis matrix
B <- ns(train$x, df = 6)
fit_basis <- lm(train$y ~ B)

cat("\n--- Coefficient comparison: direct spline fit vs basis-matrix fit ---\n")
coef_compare <- cbind(
  direct_fit = coef(m_ns_6),
  basis_fit = coef(fit_basis)
)
print(round(coef_compare, 6))

cat("\nThe fits match because spline regression is ordinary linear regression on a spline basis.\n")

## ------------------------------------------------------------
## 10. Optional formal comparison among nested natural spline dfs
## Note: comparing df here is not a classical nested polynomial hierarchy in
## a simple coefficient sense, but AIC/test MSE are often more informative.
## ------------------------------------------------------------

cat("\n--- Final remarks for class ---\n")
cat("1. Polynomial regression is global; splines are locally flexible.\n")
cat("2. Natural splines stabilize boundary behavior.\n")
cat("3. Model choice can be guided by plots, diagnostics, AIC, and test error.\n")
cat("4. Coefficients of spline basis functions are not usually interpreted directly.\n")
cat("5. Interpretation should focus on the fitted curve, derivatives, and predictions.\n")