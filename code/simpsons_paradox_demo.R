## ============================================================
## Simpson's Paradox Demonstration
## ============================================================

set.seed(123)

n <- 200

# Create a grouping variable (e.g., two hospitals)
group <- rep(c("A", "B"), each = n/2)

# Generate X values that differ by group
# Group B has systematically higher X
X_A <- rnorm(n/2, mean = 2, sd = 1)
X_B <- rnorm(n/2, mean = 8, sd = 1)

X <- c(X_A, X_B)

# Within each group, Y increases with X (positive slope)
# But groups have different intercepts:
# Group A has higher baseline Y
beta_within <- 1.5

Y_A <- 15 + beta_within * X_A + rnorm(n/2, sd = 2)
Y_B <- 5  + beta_within * X_B + rnorm(n/2, sd = 2)

Y <- c(Y_A, Y_B)

dat <- data.frame(Y, X, group)

## ------------------------------------------------------------
## 1. Overall regression (ignoring group)
## ------------------------------------------------------------

m_marginal <- lm(Y ~ X, data = dat)

cat("\n--- Marginal Model (Ignoring Group) ---\n")
print(summary(m_marginal))

## ------------------------------------------------------------
## 2. Stratified regressions
## ------------------------------------------------------------

m_A <- lm(Y ~ X, data = dat[dat$group == "A", ])
m_B <- lm(Y ~ X, data = dat[dat$group == "B", ])

cat("\n--- Group A Model ---\n")
print(summary(m_A))

cat("\n--- Group B Model ---\n")
print(summary(m_B))

## ------------------------------------------------------------
## 3. Adjusted model including group (confounder)
## ------------------------------------------------------------

m_adjusted <- lm(Y ~ X + group, data = dat)

cat("\n--- Adjusted Model (Controlling for Group) ---\n")
print(summary(m_adjusted))

## ------------------------------------------------------------
## 4. Visualization
## ------------------------------------------------------------

plot(dat$X, dat$Y,
     col = ifelse(dat$group == "A", "blue", "red"),
     pch = 19,
     xlab = "X",
     ylab = "Y",
     main = "Simpson's Paradox")

legend("topleft",
       legend = c("Group A", "Group B"),
       col = c("blue", "red"),
       pch = 19)

# Add group-specific regression lines
abline(m_A, col = "blue", lwd = 2)
abline(m_B, col = "red", lwd = 2)

# Add overall regression line
abline(m_marginal, col = "black", lwd = 2, lty = 2)

cat("\n--- Interpretation ---\n")
cat("* Within each group, the slope of X is positive.\n")
cat("* When ignoring group, the overall slope may appear negative.\n")
cat("* Including group in the model restores the correct conditional relationship.\n")