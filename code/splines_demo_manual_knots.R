## ============================================================
## SPLINE DEMO WITH MANUAL KNOT PLACEMENT
## Goals:
##   1. Simulate nonlinear data
##   2. Fit piecewise cubic spline models with manually chosen knots
##   3. Compare different knot placements
##   4. Visualize spline basis functions
##   5. Examine diagnostics
##   6. Compare models using test MSE and AIC
##
## Uses:
##   splines::bs()   for B-splines with manual knot placement
##   splines::ns()   for natural cubic splines with manual knot placement
## ============================================================

set.seed(123)

library(splines)

## ------------------------------------------------------------
## 1. Simulate synthetic data
## ------------------------------------------------------------
n <- 240
x <- sort(runif(n, 0, 10))

## True nonlinear mean with local features
f_true <- function(x) {
  1.5 +
    0.5 * x -
    0.06 * x^2 +
    2.0 * exp(-0.8 * (x - 2.5)^2) -
    1.5 * exp(-1.1 * (x - 7.2)^2)
}

mu <- f_true(x)
sigma <- 0.6
y <- mu + rnorm(n, sd = sigma)

dat <- data.frame(x = x, y = y, mu = mu)

## Train/test split
set.seed(456)
train_id <- sample(seq_len(n), size = round(0.7 * n))
train <- dat[train_id, ]
test  <- dat[-train_id, ]

## ------------------------------------------------------------
## 2. Initial plot
## ------------------------------------------------------------
plot(dat$x, dat$y,
     pch = 19, cex = 0.6,
     xlab = "x", ylab = "y",
     main = "Synthetic data for manual-knot spline demo")
lines(dat$x, dat$mu, lwd = 3)

cat("\nWe will fit spline models with manually chosen knots.\n")
cat("The solid curve is the true mean function.\n")

## ------------------------------------------------------------
## 3. Choose manual knot locations
## ------------------------------------------------------------
## A reasonable set: place knots near regions where curvature changes
knots_good <- c(2.5, 5.0, 7.0)

## A poorer set: knots less aligned with curvature
knots_poor <- c(1.0, 4.0, 9.0)

## A denser set
knots_dense <- c(2.0, 3.5, 5.0, 6.5, 8.0)

cat("\nManual knot choices:\n")
cat("Good knots :", paste(knots_good, collapse = ", "), "\n")
cat("Poor knots :", paste(knots_poor, collapse = ", "), "\n")
cat("Dense knots:", paste(knots_dense, collapse = ", "), "\n")

## ------------------------------------------------------------
## 4. Fit spline models with manual knots
## ------------------------------------------------------------
## Cubic B-splines
m_bs_good  <- lm(y ~ bs(x, knots = knots_good, degree = 3), data = train)
m_bs_poor  <- lm(y ~ bs(x, knots = knots_poor, degree = 3), data = train)
m_bs_dense <- lm(y ~ bs(x, knots = knots_dense, degree = 3), data = train)

## Natural cubic splines
m_ns_good  <- lm(y ~ ns(x, knots = knots_good), data = train)
m_ns_poor  <- lm(y ~ ns(x, knots = knots_poor), data = train)
m_ns_dense <- lm(y ~ ns(x, knots = knots_dense), data = train)

## Polynomial benchmark
m_cubic <- lm(y ~ x + I(x^2) + I(x^3), data = train)

models <- list(
  cubic_poly = m_cubic,
  bs_good = m_bs_good,
  bs_poor = m_bs_poor,
  bs_dense = m_bs_dense,
  ns_good = m_ns_good,
  ns_poor = m_ns_poor,
  ns_dense = m_ns_dense
)

## ------------------------------------------------------------
## 5. Plot fitted curves with knots shown
## ------------------------------------------------------------
x_grid <- seq(min(dat$x), max(dat$x), length.out = 400)
grid_df <- data.frame(x = x_grid)

pred_cubic    <- predict(m_cubic, newdata = grid_df)
pred_bs_good  <- predict(m_bs_good, newdata = grid_df)
pred_bs_poor  <- predict(m_bs_poor, newdata = grid_df)
pred_bs_dense <- predict(m_bs_dense, newdata = grid_df)
pred_ns_good  <- predict(m_ns_good, newdata = grid_df)
pred_ns_poor  <- predict(m_ns_poor, newdata = grid_df)
pred_ns_dense <- predict(m_ns_dense, newdata = grid_df)

par(mfrow = c(2, 2))

plot(dat$x, dat$y,
     pch = 19, cex = 0.5,
     xlab = "x", ylab = "y",
     main = "Cubic polynomial vs B-spline (good knots)")
lines(x_grid, f_true(x_grid), lwd = 3)
lines(x_grid, pred_cubic, lwd = 2, lty = 2)
lines(x_grid, pred_bs_good, lwd = 2, lty = 3)
abline(v = knots_good, lty = 3)
legend("topright",
       legend = c("True mean", "Cubic polynomial", "B-spline"),
       lty = c(1, 2, 3), lwd = c(3, 2, 2), bty = "n", cex = 0.8)

plot(dat$x, dat$y,
     pch = 19, cex = 0.5,
     xlab = "x", ylab = "y",
     main = "B-spline: good vs poor knots")
lines(x_grid, f_true(x_grid), lwd = 3)
lines(x_grid, pred_bs_good, lwd = 2, lty = 2)
lines(x_grid, pred_bs_poor, lwd = 2, lty = 3)
abline(v = knots_good, lty = 2)
abline(v = knots_poor, lty = 3)
legend("topright",
       legend = c("True mean", "Good knots", "Poor knots"),
       lty = c(1, 2, 3), lwd = c(3, 2, 2), bty = "n", cex = 0.8)

plot(dat$x, dat$y,
     pch = 19, cex = 0.5,
     xlab = "x", ylab = "y",
     main = "Natural spline: good vs dense knots")
lines(x_grid, f_true(x_grid), lwd = 3)
lines(x_grid, pred_ns_good, lwd = 2, lty = 2)
lines(x_grid, pred_ns_dense, lwd = 2, lty = 3)
abline(v = knots_good, lty = 2)
abline(v = knots_dense, lty = 3)
legend("topright",
       legend = c("True mean", "Good knots", "Dense knots"),
       lty = c(1, 2, 3), lwd = c(3, 2, 2), bty = "n", cex = 0.8)

plot(train$x, resid(m_ns_good),
     pch = 19, cex = 0.6,
     xlab = "x", ylab = "Residuals",
     main = "Residuals vs x: natural spline (good knots)")
abline(h = 0, lwd = 2)

par(mfrow = c(1, 1))

cat("\nTeaching point:\n")
cat("* Knot placement determines where the spline is allowed extra flexibility.\n")
cat("* Good knots are placed where the function bends or changes behavior.\n")
cat("* Poor knot placement may miss local structure or waste flexibility.\n")

## ------------------------------------------------------------
## 6. Plot basis functions directly
## ------------------------------------------------------------
## This is very useful pedagogically.
B_bs_good <- bs(x_grid, knots = knots_good, degree = 3)
B_ns_good <- ns(x_grid, knots = knots_good)

par(mfrow = c(1, 2))

matplot(x_grid, B_bs_good,
        type = "l", lty = 1, lwd = 2,
        xlab = "x", ylab = "Basis value",
        main = "B-spline basis functions\n(manual knots)")
abline(v = knots_good, lty = 3)

matplot(x_grid, B_ns_good,
        type = "l", lty = 1, lwd = 2,
        xlab = "x", ylab = "Basis value",
        main = "Natural spline basis functions\n(manual knots)")
abline(v = knots_good, lty = 3)

par(mfrow = c(1, 1))

cat("\nBasis-function interpretation:\n")
cat("* The fitted spline is a linear combination of these basis functions.\n")
cat("* Different knot placements change the basis functions, and therefore the fit.\n")

## ------------------------------------------------------------
## 7. Numeric model comparison
## ------------------------------------------------------------
mse <- function(y, yhat) mean((y - yhat)^2)

perf_tab <- data.frame(
  model = names(models),
  AIC = sapply(models, AIC),
  train_MSE = sapply(models, function(m) mse(train$y, fitted(m))),
  test_MSE = sapply(models, function(m) mse(test$y, predict(m, newdata = test)))
)

perf_tab <- perf_tab[order(perf_tab$test_MSE), ]

cat("\n--- Model comparison ---\n")
print(round(perf_tab, 3), row.names = FALSE)

cat("\nInterpretation:\n")
cat("* Compare AIC and test MSE together.\n")
cat("* A flexible spline can outperform a cubic polynomial if the truth has local features.\n")
cat("* But too many knots may increase variance and hurt test performance.\n")

## ------------------------------------------------------------
## 8. Diagnostics for selected models
## ------------------------------------------------------------
diagnose_model <- function(fit, name) {
  cat("\n============================================================\n")
  cat("Diagnostics for:", name, "\n")
  cat("============================================================\n")
  print(summary(fit))
  
  par(mfrow = c(2, 2))
  plot(fit)
  par(mfrow = c(1, 1))
  
  plot(train$x, resid(fit),
       pch = 19, cex = 0.6,
       xlab = "x", ylab = "Residuals",
       main = paste("Residuals vs x:", name))
  abline(h = 0, lwd = 2)
  
  ord <- order(train$x)
  smooth_vals <- predict(loess(resid(fit) ~ x, data = train))
  lines(train$x[ord], smooth_vals[ord], lwd = 2)
}

diagnose_model(m_cubic, "Cubic polynomial")
diagnose_model(m_ns_good, "Natural spline (good knots)")
diagnose_model(m_ns_poor, "Natural spline (poor knots)")

cat("\nDiagnostic guidance:\n")
cat("* Residual curvature suggests mean misspecification.\n")
cat("* A good spline should remove systematic residual structure better than a poor knot choice.\n")
cat("* If a model is too flexible, watch for unstable boundary behavior or leverage issues.\n")

## ------------------------------------------------------------
## 9. Confidence intervals for a manually-knotted natural spline
## ------------------------------------------------------------
pred_obj <- predict(m_ns_good, newdata = grid_df, se.fit = TRUE)
upper <- pred_obj$fit + 1.96 * pred_obj$se.fit
lower <- pred_obj$fit - 1.96 * pred_obj$se.fit

plot(dat$x, dat$y,
     pch = 19, cex = 0.5,
     xlab = "x", ylab = "y",
     main = "Natural spline with manual knots and 95% CI")
lines(x_grid, f_true(x_grid), lwd = 3)
lines(x_grid, pred_obj$fit, lwd = 2, lty = 2)
lines(x_grid, upper, lwd = 1, lty = 3)
lines(x_grid, lower, lwd = 1, lty = 3)
abline(v = knots_good, lty = 3)
legend("topright",
       legend = c("True mean", "Spline fit", "95% CI", "Knots"),
       lty = c(1, 2, 3, 3), lwd = c(3, 2, 1, 1), bty = "n", cex = 0.8)

## ------------------------------------------------------------
## 10. Manual knot placement via truncated power basis
## Optional advanced section: show equivalence in spirit
## ------------------------------------------------------------
## A cubic spline truncated power basis:
## y ~ x + x^2 + x^3 + (x-k1)_+^3 + (x-k2)_+^3 + ...
## This is pedagogically useful even if bs()/ns() are preferred in practice.

tp_basis <- function(x, knots) {
  out <- cbind(
    x,
    x^2,
    x^3
  )
  for (k in knots) {
    out <- cbind(out, pmax(x - k, 0)^3)
  }
  colnames(out) <- c("x", "x2", "x3", paste0("tp_", knots))
  out
}

B_tp <- tp_basis(train$x, knots_good)
tp_df <- data.frame(y = train$y, B_tp)

m_tp <- lm(y ~ ., data = tp_df)

## Predict on grid
B_tp_grid <- tp_basis(x_grid, knots_good)
grid_tp <- data.frame(B_tp_grid)
pred_tp <- predict(m_tp, newdata = grid_tp)

plot(dat$x, dat$y,
     pch = 19, cex = 0.5,
     xlab = "x", ylab = "y",
     main = "Truncated power basis spline (manual knots)")
lines(x_grid, f_true(x_grid), lwd = 3)
lines(x_grid, pred_tp, lwd = 2, lty = 2)
abline(v = knots_good, lty = 3)
legend("topright",
       legend = c("True mean", "Truncated power spline", "Knots"),
       lty = c(1, 2, 3), lwd = c(3, 2, 1), bty = "n", cex = 0.8)

cat("\nAdvanced point:\n")
cat("* The truncated power basis gives a direct mathematical construction of a spline-like fit.\n")
cat("* In practice, bs() and ns() are preferred for numerical stability.\n")

## ------------------------------------------------------------
## 11. Final teaching summary
## ------------------------------------------------------------
cat("\n============================================================\n")
cat("FINAL TEACHING SUMMARY\n")
cat("============================================================\n")
cat("1. Manual knot placement lets you decide where the model needs local flexibility.\n")
cat("2. Knots should usually be placed where the function bends, changes slope, or has local features.\n")
cat("3. Poor knot placement can miss important structure or waste flexibility.\n")
cat("4. Natural splines generally behave better at the boundaries than unrestricted cubic splines.\n")
cat("5. Spline regression is still linear regression on transformed basis functions.\n")
cat("6. Use plots, residual diagnostics, and test error to assess spline quality.\n")