############################################################
# Improving Linear Regression for the faithful Dataset
# (No mixture modeling)
############################################################

# -----------------------------
# 1. Load data and inspect
# -----------------------------
data(faithful)

# Basic exploratory plot
plot(eruptions ~ waiting, data = faithful,
     pch = 16, col = "gray",
     xlab = "Waiting Time (minutes)",
     ylab = "Eruption Duration (minutes)",
     main = "Old Faithful: Eruption Duration vs Waiting Time")

# -----------------------------
# 2. Naive simple linear regression
# -----------------------------
fit_lm <- lm(eruptions ~ waiting, data = faithful)
summary(fit_lm)

# Diagnostics for naive model
par(mfrow = c(2, 2))
plot(fit_lm)
par(mfrow = c(1, 1))

# Interpretation:
# - Strong linear association
# - High R^2
# - Clear residual structure and non-normality

# -----------------------------
# 3. Allow nonlinear mean structure
# -----------------------------
fit_poly <- lm(eruptions ~ poly(waiting, 2), data = faithful)
summary(fit_poly)

# Diagnostics for polynomial model
par(mfrow = c(2, 2))
plot(fit_poly)
par(mfrow = c(1, 1))

# Interpretation:
# - Captures curvature in mean
# - Does not resolve clustering
# - Interpretation less transparent

# -----------------------------
# 4. Regime-based linear regression
# -----------------------------
# Create a regime indicator based on waiting time
faithful$regime <- ifelse(faithful$waiting < 70, "short", "long")
faithful$regime <- factor(faithful$regime)

# Visualize regimes
plot(eruptions ~ waiting, data = faithful,
     col = c("blue", "red")[faithful$regime],
     pch = 16,
     xlab = "Waiting Time (minutes)",
     ylab = "Eruption Duration (minutes)",
     main = "Old Faithful with Regimes")
legend("topleft",
       legend = levels(faithful$regime),
       col = c("blue", "red"),
       pch = 16,
       bty = "n")

# Fit interaction model
fit_regime <- lm(eruptions ~ waiting * regime, data = faithful)
summary(fit_regime)

# Diagnostics for regime-based model
par(mfrow = c(2, 2))
plot(fit_regime)
par(mfrow = c(1, 1))

# Interpretation:
# - Separate linear trends by regime
# - Substantial improvement in residual behavior
# - Model remains within linear regression framework

# -----------------------------
# 5. Model comparison
# -----------------------------
anova(fit_lm, fit_poly, fit_regime)

# -----------------------------
# 6. Takeaways
# -----------------------------
# - High R^2 alone is not sufficient
# - Residual diagnostics reveal unmodeled structure
# - Substantive modeling choices outperform cosmetic fixes
############################################################
