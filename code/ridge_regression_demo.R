## ============================================================
## Ridge regression demo with glmnet
## Goal: show improved predictive performance relative to OLS
## ============================================================

## install.packages("glmnet")   # uncomment if needed
library(glmnet)

set.seed(123)

## ----------------------------
## 1. Simulate correlated predictors
## ----------------------------
n_train <- 150
n_test  <- 1000
p <- 20

# Create 5 latent factors to induce strong collinearity
make_X <- function(n, p) {
  F1 <- rnorm(n)
  F2 <- rnorm(n)
  F3 <- rnorm(n)
  F4 <- rnorm(n)
  F5 <- rnorm(n)
  
  X <- cbind(
    F1 + rnorm(n, sd = 0.15),
    F1 + rnorm(n, sd = 0.15),
    F1 + rnorm(n, sd = 0.15),
    F1 + rnorm(n, sd = 0.15),
    
    F2 + rnorm(n, sd = 0.15),
    F2 + rnorm(n, sd = 0.15),
    F2 + rnorm(n, sd = 0.15),
    F2 + rnorm(n, sd = 0.15),
    
    F3 + rnorm(n, sd = 0.15),
    F3 + rnorm(n, sd = 0.15),
    F3 + rnorm(n, sd = 0.15),
    F3 + rnorm(n, sd = 0.15),
    
    F4 + rnorm(n, sd = 0.15),
    F4 + rnorm(n, sd = 0.15),
    F4 + rnorm(n, sd = 0.15),
    F4 + rnorm(n, sd = 0.15),
    
    F5 + rnorm(n, sd = 0.15),
    F5 + rnorm(n, sd = 0.15),
    F5 + rnorm(n, sd = 0.15),
    F5 + rnorm(n, sd = 0.15)
  )
  
  X[, 1:p, drop = FALSE]
}

X_train <- make_X(n_train, p)
X_test  <- make_X(n_test, p)

colnames(X_train) <- paste0("x", 1:p)
colnames(X_test)  <- paste0("x", 1:p)

# True coefficients: only a few matter
beta_true <- c(3, 0, 0, 0,
               -2.5, 0, 0, 0,
               2, 0, 0, 0,
               0, 0, 0, 0,
               1.5, 0, 0, 0)

sigma <- 3

y_train <- as.vector(X_train %*% beta_true + rnorm(n_train, sd = sigma))
y_test  <- as.vector(X_test  %*% beta_true + rnorm(n_test,  sd = sigma))

## ----------------------------
## 2. Fit OLS
## ----------------------------
train_df <- data.frame(y = y_train, X_train)
test_df  <- data.frame(y = y_test, X_test)

ols_fit <- lm(y ~ ., data = train_df)

ols_pred_train <- predict(ols_fit, newdata = train_df)
ols_pred_test  <- predict(ols_fit, newdata = test_df)

mse <- function(y, yhat) mean((y - yhat)^2)

ols_train_mse <- mse(y_train, ols_pred_train)
ols_test_mse  <- mse(y_test, ols_pred_test)

cat("\n--- OLS performance ---\n")
cat("Train MSE:", round(ols_train_mse, 3), "\n")
cat("Test  MSE:", round(ols_test_mse, 3), "\n")

## Optional: inspect coefficient instability
cat("\n--- OLS coefficients (first 10) ---\n")
print(round(coef(ols_fit)[1:11], 3))

## ----------------------------
## 3. Fit ridge with cross-validation
## ----------------------------
# glmnet wants x as a matrix and y as a vector
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

cat("\n--- Ridge CV results ---\n")
cat("lambda.min:", cv_ridge$lambda.min, "\n")
cat("lambda.1se:", cv_ridge$lambda.1se, "\n")

# Fit ridge predictions at lambda.min and lambda.1se
ridge_pred_train_min <- predict(cv_ridge, newx = x_train_mat, s = "lambda.min")
ridge_pred_test_min  <- predict(cv_ridge, newx = x_test_mat,  s = "lambda.min")

ridge_pred_train_1se <- predict(cv_ridge, newx = x_train_mat, s = "lambda.1se")
ridge_pred_test_1se  <- predict(cv_ridge, newx = x_test_mat,  s = "lambda.1se")

ridge_train_mse_min <- mse(y_train, ridge_pred_train_min)
ridge_test_mse_min  <- mse(y_test, ridge_pred_test_min)

ridge_train_mse_1se <- mse(y_train, ridge_pred_train_1se)
ridge_test_mse_1se  <- mse(y_test, ridge_pred_test_1se)

cat("\n--- Ridge performance (lambda.min) ---\n")
cat("Train MSE:", round(ridge_train_mse_min, 3), "\n")
cat("Test  MSE:", round(ridge_test_mse_min, 3), "\n")

cat("\n--- Ridge performance (lambda.1se) ---\n")
cat("Train MSE:", round(ridge_train_mse_1se, 3), "\n")
cat("Test  MSE:", round(ridge_test_mse_1se, 3), "\n")

## ----------------------------
## 4. Compare coefficient magnitudes
## ----------------------------
ridge_coef_min <- as.matrix(coef(cv_ridge, s = "lambda.min"))
ridge_coef_1se <- as.matrix(coef(cv_ridge, s = "lambda.1se"))

cat("\n--- Ridge coefficients at lambda.min (first 10) ---\n")
print(round(ridge_coef_min[1:11, , drop = FALSE], 3))

cat("\n--- Ridge coefficients at lambda.1se (first 10) ---\n")
print(round(ridge_coef_1se[1:11, , drop = FALSE], 3))

## ----------------------------
## 5. Visualize the CV curve
## ----------------------------
plot(cv_ridge)

## ----------------------------
## 6. Simple summary table
## ----------------------------
results <- data.frame(
  Model = c("OLS", "Ridge (lambda.min)", "Ridge (lambda.1se)"),
  Train_MSE = c(ols_train_mse, ridge_train_mse_min, ridge_train_mse_1se),
  Test_MSE  = c(ols_test_mse,  ridge_test_mse_min,  ridge_test_mse_1se)
)

cat("\n--- Summary table ---\n")
print(round(results, 3))

## ----------------------------
## 7. Correlation check (illustrates collinearity)
## ----------------------------
cat("\n--- Example predictor correlations ---\n")
print(round(cor(X_train[, 1:6]), 2))