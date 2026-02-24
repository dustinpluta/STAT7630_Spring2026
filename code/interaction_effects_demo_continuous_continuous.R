## ============================================================
## Interaction Effects Demonstration
## - Simulate data with a true interaction
## - Compare main-effects-only vs interaction model
## - Interpret coefficients
## - Perform partial F-test
## - Visualize results
## - Show centering for interpretation
## ============================================================

set.seed(123)

n <- 250

# Two continuous predictors
X1 <- rnorm(n, mean = 5, sd = 2)
X2 <- rnorm(n, mean = 10, sd = 3)

# True model includes interaction
beta0 <- 3
beta1 <- 1.0
beta2 <- -0.5
beta3 <- 0.8   # interaction term

eps <- rnorm(n, sd = 2)

Y <- beta0 + beta1*X1 + beta2*X2 + beta3*(X1*X2) + eps

dat <- data.frame(Y, X1, X2)

## ------------------------------------------------------------
## 1. Main-effects-only model
## ------------------------------------------------------------

m_main <- lm(Y ~ X1 + X2, data = dat)

cat("\n--- Main Effects Only Model ---\n")
print(summary(m_main))

## ------------------------------------------------------------
## 2. Interaction model
## ------------------------------------------------------------

m_int <- lm(Y ~ X1 * X2, data = dat)  # includes X1 + X2 + X1:X2

cat("\n--- Interaction Model ---\n")
print(summary(m_int))

## ------------------------------------------------------------
## 3. Model comparison (partial F-test)
## ------------------------------------------------------------

cat("\n--- Model Comparison (Partial F-test) ---\n")
print(anova(m_main, m_int))

cat("\n--- R-squared comparison ---\n")
cat("Main effects R^2:", summary(m_main)$r.squared, "\n")
cat("Interaction R^2:", summary(m_int)$r.squared, "\n")

## ------------------------------------------------------------
## 4. Interpretation of interaction
## ------------------------------------------------------------

coef_int <- coef(m_int)

cat("\n--- Interpretation ---\n")
cat("Slope of X1 given X2 is:\n")
cat("dE[Y]/dX1 = beta1 + beta3 * X2\n\n")
cat("Estimated slope function:\n")
cat("dE[Y]/dX1 =", round(coef_int["X1"],3), 
    "+", round(coef_int["X1:X2"],3), "* X2\n\n")

## ------------------------------------------------------------
## 5. Visualization
## ------------------------------------------------------------

# Plot colored by high vs low X2
high_X2 <- X2 > median(X2)

plot(X1, Y,
     col = ifelse(high_X2, "red", "blue"),
     pch = 19,
     xlab = "X1",
     ylab = "Y",
     main = "Interaction Effect: Non-Parallel Slopes")

legend("topleft",
       legend = c("Low X2", "High X2"),
       col = c("blue", "red"),
       pch = 19)

# Add regression lines at two representative X2 values
X2_low <- quantile(X2, 0.25)
X2_high <- quantile(X2, 0.75)

abline(a = coef_int[1] + coef_int[3]*X2_low,
       b = coef_int[2] + coef_int[4]*X2_low,
       col = "blue", lwd = 2)

abline(a = coef_int[1] + coef_int[3]*X2_high,
       b = coef_int[2] + coef_int[4]*X2_high,
       col = "red", lwd = 2)

## ------------------------------------------------------------
## 6. Centering for interpretation
## ------------------------------------------------------------

dat$X1_c <- dat$X1 - mean(dat$X1)
dat$X2_c <- dat$X2 - mean(dat$X2)

m_centered <- lm(Y ~ X1_c * X2_c, data = dat)

cat("\n--- Interaction Model with Centered Predictors ---\n")
print(summary(m_centered))

cat("\nNote:\n")
cat("* Centering does NOT change model fit or interaction test.\n")
cat("* It changes interpretation:\n")
cat("  Now beta1 is effect of X1 at average X2.\n")
cat("  Now beta2 is effect of X2 at average X1.\n")

