## ============================================================
## OLS vs Ridge vs LASSO demo
## Goal:
##   1. Simulate correlated predictors
##   2. Fit OLS, ridge, and lasso
##   3. Compare predictive performance on test data
##   4. Show that lasso performs variable selection
## ============================================================

## install.packages("glmnet")   # uncomment if needed
library(glmnet)

set.seed(123)

## ----------------------------
## 1. Simulate data
## ----------------------------
n_train <- 120
n_test  <- 1000
p <- 20

make_X <- function(n) {
  z1 <- rnorm(n)
  z2 <- rnorm(n)
  z3 <- rnorm(n)
  z4 <- rnorm(n)
  z5 <- rnorm(n)
  
  X <- cbind(
    z1 + rnorm(n, sd = 0.2),
    z1 + rnorm(n, sd = 0.2),
    z1 + rnorm(n, sd = 0.2),
    z1 + rnorm(n, sd = 0.2),
    
    z2 + rnorm(n, sd = 0.2),
    z2 + rnorm(n, sd = 0.2),
    z2 + rnorm(n, sd = 0.2),
    z2 + rnorm(n, sd = 0.2),
    
    z3 + rnorm(n, sd = 0.2),
    z3 + rnorm(n, sd = 0.2),
    z3 + rnorm(n, sd = 0.2),
    z3 + rnorm(n, sd = 0.2),
    
    z4 + rnorm(n, sd = 0.2),
    z4 + rnorm(n, sd = 0.2),
    z4 + rnorm(n, sd = 0.2),
    z4 + rnorm(n, sd = 0.2),
    
    z5 + rnorm(n, sd = 0.2),
    z5 + rnorm(n, sd = 0.2),
    z5 + rnorm(n, sd = 0.2),
    z5 + rnorm(n, sd = 0.2)
  )
  
  colnames(X) <- paste0("x", 1:ncol(X))
  X
}

X_train <- make_X(n_train)
X_test  <- make_X(n_test)

# Sparse true coefficient vector:
# only a few predictors actually matter
beta_true <- c(3.0, 0, 0, 0,
               -2.5, 0, 0, 0,
               2.0, 0, 0, 0,
               0, 0, 0, 0,
               1.5, 0, 0, 0)

sigma <- 3

y_train <- as.vector(X_train %*% beta_true + rnorm(n_train, sd = sigma))
y_test  <- as.vector(X_test  %*% beta_true + rnorm(n_test,  sd = sigma))

train_df <- data.frame(y = y_train, X_train)
test_df  <- data.frame(y = y_test, X_test)

cat("\n--- True nonzero coefficients ---\n")
print(beta_true)

## ----------------------------
## 2. Fit OLS
## ----------------------------
ols_fit <- lm(y ~ ., data = train_df)

## ----------------------------
## 3. Fit ridge and lasso via cv.glmnet
## ----------------------------
x_train_mat <- as.matrix(X_train)
x_test_mat  <- as.matrix(X_test)

set.seed(123)
cv_ridge <- cv.glmnet(
  x = x_train_mat,
  y = y_train,
  alpha = 0,          # ridge
  family = "gaussian",
  nfolds = 10
)

set.seed(123)
cv_lasso <- cv.glmnet(
  x = x_train_mat,
  y = y_train,
  alpha = 1,          # lasso
  family = "gaussian",
  nfolds = 10
)

cat("\n--- Chosen lambdas ---\n")
cat("Ridge lambda.min:", cv_ridge$lambda.min, "\n")
cat("Ridge lambda.1se:", cv_ridge$lambda.1se, "\n")
cat("LASSO lambda.min:", cv_lasso$lambda.min, "\n")
cat("LASSO lambda.1se:", cv_lasso$lambda.1se, "\n")

## ----------------------------
## 4. Predictions and test performance
## ----------------------------
mse <- function(y, yhat) mean((y - yhat)^2)

# OLS
ols_pred_train <- predict(ols_fit, newdata = train_df)
ols_pred_test  <- predict(ols_fit, newdata = test_df)

# Ridge
ridge_pred_train <- predict(cv_ridge, newx = x_train_mat, s = "lambda.min")
ridge_pred_test  <- predict(cv_ridge, newx = x_test_mat,  s = "lambda.min")

# LASSO
lasso_pred_train <- predict(cv_lasso, newx = x_train_mat, s = "lambda.min")
lasso_pred_test  <- predict(cv_lasso, newx = x_test_mat,  s = "lambda.min")

results <- data.frame(
  Model = c("OLS", "Ridge", "LASSO"),
  Train_MSE = c(
    mse(y_train, ols_pred_train),
    mse(y_train, ridge_pred_train),
    mse(y_train, lasso_pred_train)
  ),
  Test_MSE = c(
    mse(y_test, ols_pred_test),
    mse(y_test, ridge_pred_test),
    mse(y_test, lasso_pred_test)
  )
)

cat("\n--- Performance comparison ---\n")
print(results)

## ----------------------------
## 5. Compare coefficients
## ----------------------------
ols_coef <- coef(ols_fit)
ridge_coef <- as.matrix(coef(cv_ridge, s = "lambda.min"))
lasso_coef <- as.matrix(coef(cv_lasso, s = "lambda.min"))

coef_tab <- data.frame(
  Variable = rownames(ridge_coef),
  OLS = c(ols_coef[1], ols_coef[-1]),
  Ridge = ridge_coef[, 1],
  LASSO = lasso_coef[, 1]
)

cat("\n--- First several coefficients ---\n")
print(round(coef_tab[1:12, 2:4], 3))

## ----------------------------
## 6. Show variable selection by lasso
## ----------------------------
lasso_selected <- rownames(lasso_coef)[lasso_coef[, 1] != 0]
lasso_selected <- setdiff(lasso_selected, "(Intercept)")

ridge_nonzero <- rownames(ridge_coef)[abs(ridge_coef[, 1]) > 1e-8]
ridge_nonzero <- setdiff(ridge_nonzero, "(Intercept)")

cat("\n--- Variables selected by LASSO ---\n")
print(lasso_selected)

cat("\n--- Number of nonzero coefficients ---\n")
cat("Ridge:", length(ridge_nonzero), "out of", p, "\n")
cat("LASSO:", length(lasso_selected), "out of", p, "\n")

## ----------------------------
## 7. Plot coefficient paths
## ----------------------------
par(mfrow = c(1, 2))
plot(glmnet(x_train_mat, y_train, alpha = 0), xvar = "lambda",
     main = "Ridge coefficient paths")
plot(glmnet(x_train_mat, y_train, alpha = 1), xvar = "lambda",
     main = "LASSO coefficient paths")
par(mfrow = c(1, 1))

## ----------------------------
## 8. Plot CV curves
## ----------------------------
par(mfrow = c(1, 2))
plot(cv_ridge, main = "Ridge CV curve")
plot(cv_lasso, main = "LASSO CV curve")
par(mfrow = c(1, 1))

## ----------------------------
## 9. Clean summary for teaching
## ----------------------------
cat("\n--- Teaching summary ---\n")
cat("1. OLS uses all predictors and can be unstable under collinearity.\n")
cat("2. Ridge shrinks coefficients toward zero but typically keeps all variables.\n")
cat("3. LASSO shrinks coefficients and can set some exactly to zero.\n")
cat("4. The zero coefficients illustrate variable selection.\n")
cat("5. Compare test MSE to see which method predicts best in this simulation.\n")