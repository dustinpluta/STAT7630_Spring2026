## ============================================================
## Interaction demo: Categorical (binary) x Continuous
## - Simulate data where slope of X differs by group
## - Fit main-effects-only vs interaction model
## - Partial F-test for interaction
## - Interpret coefficients as group-specific intercepts/slopes
## - Visualize non-parallel lines
## - Show centering of X for interpretability
## ============================================================

set.seed(123)

n <- 240

# Binary categorical predictor (e.g., Treatment vs Control)
group <- factor(sample(c("Control", "Treatment"), size = n, replace = TRUE))

# Continuous predictor (e.g., dosage, age, study time)
X <- rnorm(n, mean = 50, sd = 10)

# True DGP: different intercepts AND different slopes by group
# Control:  E[Y|X] = (beta0) + (beta1)*X
# Treat:    E[Y|X] = (beta0+beta2) + (beta1+beta3)*X
beta0 <- 10
beta1 <- 0.40   # slope in Control
beta2 <- -8     # intercept shift for Treatment vs Control
beta3 <- 0.25   # slope difference (Treatment - Control)

eps <- rnorm(n, sd = 5)

Y <- beta0 +
  beta1 * X +
  beta2 * (group == "Treatment") +
  beta3 * X * (group == "Treatment") +
  eps

dat <- data.frame(Y, X, group)

## ------------------------------------------------------------
## 1) Main-effects-only model (assumes parallel slopes)
## ------------------------------------------------------------

m_main <- lm(Y ~ X + group, data = dat)

cat("\n--- Main-effects-only model: Y ~ X + group ---\n")
print(summary(m_main))

## ------------------------------------------------------------
## 2) Interaction model (allows different slopes by group)
## ------------------------------------------------------------

m_int <- lm(Y ~ X * group, data = dat)  # X + group + X:group

cat("\n--- Interaction model: Y ~ X * group ---\n")
print(summary(m_int))

## ------------------------------------------------------------
## 3) Partial F-test: does interaction improve fit?
##     H0: slope difference = 0  (no interaction)
## ------------------------------------------------------------

cat("\n--- Partial F-test (main vs interaction) ---\n")
print(anova(m_main, m_int))

cat("\n--- R-squared comparison ---\n")
cat("Main effects R^2:", summary(m_main)$r.squared, "\n")
cat("Interaction R^2:", summary(m_int)$r.squared, "\n")

## ------------------------------------------------------------
## 4) Interpret coefficients as group-specific lines
## ------------------------------------------------------------

b <- coef(m_int)

# With R's default treatment coding, "Control" is the reference (alphabetical order).
# Check baseline level:
cat("\n--- Reference level for group ---\n")
print(levels(dat$group))

# Predicted lines:
# Control:    E[Y|X, Control]   = b0 + bX * X
# Treatment:  E[Y|X, Treatment] = (b0 + bG) + (bX + bInt) * X
b0   <- b["(Intercept)"]
bX   <- b["X"]
bG   <- b[paste0("group", levels(dat$group)[2])]            # groupTreatment if Control is ref
bInt <- b[paste0("X:group", levels(dat$group)[2])]          # X:groupTreatment

cat("\n--- Fitted group-specific equations (from interaction model) ---\n")
cat("Control:   E[Y|X] =", round(b0,3), "+", round(bX,3), "* X\n")
cat("Treatment: E[Y|X] =",
    round(b0 + bG,3), "+", round(bX + bInt,3), "* X\n")

cat("\nInterpretation:\n")
cat("* bG is the intercept difference (Treatment - Control) when X = 0.\n")
cat("* bInt is the slope difference (Treatment - Control).\n")
cat("* If bInt = 0, lines are parallel (no interaction).\n")

## ------------------------------------------------------------
## 5) Visualization: non-parallel fitted lines
## ------------------------------------------------------------

cols <- ifelse(dat$group == "Control", "blue", "red")
plot(dat$X, dat$Y,
     col = cols, pch = 19,
     xlab = "X (continuous predictor)",
     ylab = "Y (response)",
     main = "Categorical × Continuous Interaction: Non-Parallel Slopes")

legend("topleft",
       legend = c("Control", "Treatment"),
       col = c("blue", "red"),
       pch = 19)

# Add fitted regression lines per group
xgrid <- seq(min(dat$X), max(dat$X), length.out = 100)

# Control line
yhat_control <- (b0) + (bX) * xgrid
lines(xgrid, yhat_control, col = "blue", lwd = 2)

# Treatment line
yhat_treat <- (b0 + bG) + (bX + bInt) * xgrid
lines(xgrid, yhat_treat, col = "red", lwd = 2)

## ------------------------------------------------------------
## 6) Center X to make intercept comparisons meaningful
## ------------------------------------------------------------

dat$Xc <- dat$X - mean(dat$X)

m_int_centered <- lm(Y ~ Xc * group, data = dat)

cat("\n--- Interaction model with centered X: Y ~ Xc * group ---\n")
print(summary(m_int_centered))

cat("\nCentering note:\n")
cat("* Centering does NOT change fitted values, R^2, or the interaction test.\n")
cat("* It changes interpretation: group coefficient is now the mean difference at X = mean(X).\n")