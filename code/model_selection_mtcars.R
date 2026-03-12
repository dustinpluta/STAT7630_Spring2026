## ============================================================
## Applied model selection example using built-in R data: mtcars
## Response: mpg
## Goal: select a reasonable regression model for fuel economy
## ============================================================

## ----------------------------
## 1. Load and prepare data
## ----------------------------
data(mtcars)

dat <- mtcars

# Treat some variables as categorical where appropriate
dat$cyl <- factor(dat$cyl)
dat$am  <- factor(dat$am, labels = c("Automatic", "Manual"))
dat$vs  <- factor(dat$vs, labels = c("V-shaped", "Straight"))

str(dat)

## ----------------------------
## 2. Theory-driven full model
## ----------------------------
# Applied story:
# mpg may depend on vehicle weight, horsepower, engine size,
# number of cylinders, transmission, and rear axle ratio.

m_full <- lm(mpg ~ wt + hp + disp + cyl + am + drat, data = dat)

cat("\n--- Full model summary ---\n")
print(summary(m_full))

## ----------------------------
## 3. Candidate reduced models
## ----------------------------
m1 <- lm(mpg ~ wt + hp + cyl + am, data = dat)
m2 <- lm(mpg ~ wt + hp + am, data = dat)
m3 <- lm(mpg ~ wt + cyl + am, data = dat)
m4 <- lm(mpg ~ wt + hp + disp + cyl, data = dat)
m5 <- lm(mpg ~ wt + hp, data = dat)

models <- list(
  full = m_full,
  m1 = m1,
  m2 = m2,
  m3 = m3,
  m4 = m4,
  m5 = m5
)

## ----------------------------
## 4. Diagnostic plots for the full model
## ----------------------------
par(mfrow = c(2, 2))
plot(m_full)
par(mfrow = c(1, 1))

## ----------------------------
## 5. Partial F-tests for nested models
## ----------------------------
cat("\n--- Partial F-tests ---\n")

# Test whether disp and drat add to model m1
cat("\nTest: add disp and drat to model wt + hp + cyl + am\n")
print(anova(m1, m_full))

# Test whether cyl adds to model wt + hp + am
cat("\nTest: add cyl to model wt + hp + am\n")
print(anova(m2, m1))

# Test whether hp adds to model wt + cyl + am
cat("\nTest: add hp to model wt + cyl + am\n")
print(anova(m3, m1))

## ----------------------------
## 6. Compare AIC and BIC
## ----------------------------
info_tab <- data.frame(
  model = names(models),
  p = sapply(models, function(m) length(coef(m))),
  AIC = sapply(models, AIC),
  BIC = sapply(models, BIC)
)

info_tab$delta_AIC <- info_tab$AIC - min(info_tab$AIC)
info_tab$delta_BIC <- info_tab$BIC - min(info_tab$BIC)

# Akaike weights
w <- exp(-0.5 * info_tab$delta_AIC)
info_tab$Akaike_weight <- w / sum(w)

info_tab <- info_tab[order(info_tab$AIC), ]

cat("\n--- AIC/BIC comparison ---\n")
print(info_tab, row.names = FALSE)

## ----------------------------
## 7. 10-fold cross-validation
## ----------------------------
cv_mse <- function(formula, data, K = 10, seed = 123) {
  set.seed(seed)
  n <- nrow(data)
  folds <- sample(rep(1:K, length.out = n))
  mse <- numeric(K)
  
  for (k in 1:K) {
    train <- data[folds != k, , drop = FALSE]
    test  <- data[folds == k, , drop = FALSE]
    
    fit <- lm(formula, data = train)
    pred <- predict(fit, newdata = test)
    mse[k] <- mean((test$mpg - pred)^2)
  }
  
  mean(mse)
}

cv_tab <- data.frame(
  model = names(models),
  CV_MSE = sapply(models, function(m) cv_mse(formula(m), dat, K = 10, seed = 123))
)

cv_tab <- cv_tab[order(cv_tab$CV_MSE), ]

cat("\n--- 10-fold CV results ---\n")
print(cv_tab, row.names = FALSE)

## ----------------------------
## 8. Summarize top models
## ----------------------------
cat("\n--- Top models by AIC ---\n")
print(info_tab[1:3, ], row.names = FALSE)

cat("\n--- Top models by CV ---\n")
print(cv_tab[1:3, ], row.names = FALSE)

## ----------------------------
## 9. Final chosen model
## ----------------------------
# Choose a final model by balancing:
# - interpretability
# - AIC/BIC
# - CV performance
# - nested F-tests
#
# In many runs, m1 is a strong compromise:
# mpg ~ wt + hp + cyl + am

m_final <- m1

cat("\n--- Final selected model summary ---\n")
print(summary(m_final))

confint(m_final)

## ----------------------------
## 10. Brief uncertainty summary
## ----------------------------
cat("\n--- Model uncertainty summary ---\n")
cat("Models with delta AIC <= 2 are often considered similarly supported.\n")
print(subset(info_tab, delta_AIC <= 2), row.names = FALSE)

cat("\nInterpretation template:\n")
cat("1. Start with the full scientifically motivated model.\n")
cat("2. Use partial F-tests for nested comparisons.\n")
cat("3. Compare AIC/BIC across plausible candidate models.\n")
cat("4. Confirm predictive performance with CV.\n")
cat("5. If several models perform similarly, prefer the simpler/interpretable one.\n")