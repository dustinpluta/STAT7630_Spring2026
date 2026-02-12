## Confounding narrative demo: smoking vs FEV confounded by age + height
## Data file: fev.txt (tab-delimited)
## Goal: show how the *unadjusted* smoking effect differs from the *adjusted* effect
## because smokers differ in age/height, and age/height strongly predict FEV.

# ---------- 0) Load + minimal cleaning ----------
fev_path <- "/mnt/data/fev.txt"
dat <- read.table(fev_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Standardize smoking coding
dat$smoke_bin <- ifelse(tolower(dat$smoke) == "smoker", 1, 0)
dat$smoke_fac <- factor(dat$smoke_bin, levels = c(0,1), labels = c("nonsmoker","smoker"))

stopifnot(all(c("fev","age","height","smoke_bin") %in% names(dat)))

# ---------- 1) Tell the story up front ----------
cat("\n====================\nCONFUNDING NARRATIVE\n====================\n")
cat("We want the effect of smoking on lung function (FEV).\n")
cat("But age and height affect FEV, and smoking status is associated with age/height.\n")
cat("So, comparing mean FEV between smokers and nonsmokers mixes:\n")
cat("  (i) smoking's effect, plus\n")
cat(" (ii) differences in age/height between groups.\n")
cat("Multiple regression aims to isolate smoking's effect by adjusting for age + height.\n\n")

# ---------- 2) Show (a) smoking is associated with the outcome (marginally) ----------
cat("STEP A: Marginal association (smoking -> FEV) via group means\n")
group_means <- aggregate(cbind(fev, age, height) ~ smoke_fac, data = dat, FUN = mean)
group_sds   <- aggregate(cbind(fev, age, height) ~ smoke_fac, data = dat, FUN = sd)
group_n     <- as.data.frame(table(dat$smoke_fac)); names(group_n) <- c("smoke_fac","n")

print(merge(group_n, group_means, by = "smoke_fac"))
cat("\n(Reference) SDs within groups:\n")
print(group_sds)

cat("\nInterpretation prompt:\n")
cat("- If mean(FEV) differs by smoking group, that is a marginal association.\n")
cat("- But it does NOT yet imply a causal smoking effect due to possible confounding.\n\n")

# ---------- 3) Show (b) smoking is associated with confounders (age/height) ----------
cat("STEP B: Smoking is associated with age/height (the confounders)\n")
cat("If smokers are systematically older/taller (or younger/shorter),\n")
cat("then age/height differences can induce or distort the smoking–FEV association.\n\n")

# Quick numeric comparisons
cat("Mean age by smoking:\n");    print(with(dat, tapply(age, smoke_fac, mean)))
cat("Mean height by smoking:\n"); print(with(dat, tapply(height, smoke_fac, mean)))

# Visual: distributions by smoking
op <- par(mfrow = c(1,2))
boxplot(age ~ smoke_fac, data = dat, main = "Age by smoking status", xlab = "", ylab = "Age")
boxplot(height ~ smoke_fac, data = dat, main = "Height by smoking status", xlab = "", ylab = "Height")
par(op)

cat("\nInterpretation prompt:\n")
cat("- If age and/or height differ by smoking group, smoking is associated with the confounders.\n")
cat("- Since age/height also predict FEV, confounding is plausible.\n\n")

# ---------- 4) Show (c) confounders predict the outcome ----------
cat("STEP C: Age and height predict FEV\n")
m_age    <- lm(fev ~ age, data = dat)
m_height <- lm(fev ~ height, data = dat)

cat("\nSLR: fev ~ age\n");    print(coef(summary(m_age)))
cat("\nSLR: fev ~ height\n"); print(coef(summary(m_height)))

cat("\nInterpretation prompt:\n")
cat("- Strong age/height effects mean group imbalances can strongly affect mean FEV.\n\n")

# ---------- 5) The key comparison: unadjusted vs adjusted smoking coefficient ----------
cat("STEP D: Unadjusted vs adjusted smoking effect\n")
m_smoke <- lm(fev ~ smoke_bin, data = dat)                    # unadjusted
m_adj   <- lm(fev ~ smoke_bin + age + height, data = dat)     # adjusted

b_unadj <- coef(m_smoke)["smoke_bin"]
se_unadj <- coef(summary(m_smoke))["smoke_bin","Std. Error"]
p_unadj  <- coef(summary(m_smoke))["smoke_bin","Pr(>|t|)"]

b_adj <- coef(m_adj)["smoke_bin"]
se_adj <- coef(summary(m_adj))["smoke_bin","Std. Error"]
p_adj  <- coef(summary(m_adj))["smoke_bin","Pr(>|t|)"]

cat("\nUnadjusted model: fev ~ smoke\n")
cat(sprintf("  smoke effect = %0.4f (SE %0.4f), p = %0.4g\n", b_unadj, se_unadj, p_unadj))

cat("\nAdjusted model: fev ~ smoke + age + height\n")
cat(sprintf("  smoke effect = %0.4f (SE %0.4f), p = %0.4g\n", b_adj, se_adj, p_adj))

cat("\nChange in smoke coefficient after adjustment:\n")
cat(sprintf("  adjusted - unadjusted = %0.4f\n", b_adj - b_unadj))

cat("\nInterpretation prompt:\n")
cat("- If the smoking coefficient changes meaningfully after adjusting for age/height,\n")
cat("  that is the operational signature of confounding.\n")
cat("- The adjusted coefficient is the 'holding age and height fixed' comparison.\n\n")

# ---------- 6) Make the adjusted effect visual (added-variable / partial regression plot) ----------
cat("STEP E: Visualize the adjusted smoking effect (partial regression)\n")
r_fev  <- resid(lm(fev ~ age + height, data = dat))
r_smok <- resid(lm(smoke_bin ~ age + height, data = dat))

plot(r_smok, r_fev, pch = 19,
     main = "Smoking effect adjusted for age + height\n(added-variable plot)",
     xlab = "Residual smoke | age,height",
     ylab = "Residual FEV | age,height")
abline(lm(r_fev ~ r_smok), lwd = 2)

cat("\nHow to explain this plot in words:\n")
cat("- First remove (regress out) age+height from both FEV and smoking.\n")
cat("- Then relate what's left: does smoking still predict FEV once age/height are accounted for?\n\n")

# ---------- 7) Optional: one figure that shows why marginal comparisons can mislead ----------
# Scatter with color to show group separation and the age/height structure
op <- par(mfrow = c(1,2))
plot(fev ~ age, data = dat, pch = 19,
     col = ifelse(dat$smoke_bin==1, "red", "black"),
     main = "FEV vs Age (color = smoking)", xlab = "Age", ylab = "FEV")
legend("topleft", legend = c("nonsmoker","smoker"), col = c("black","red"), pch = 19, bty = "n")

plot(fev ~ height, data = dat, pch = 19,
     col = ifelse(dat$smoke_bin==1, "red", "black"),
     main = "FEV vs Height (color = smoking)", xlab = "Height", ylab = "FEV")
legend("topleft", legend = c("nonsmoker","smoker"), col = c("black","red"), pch = 19, bty = "n")
par(op)

cat("DONE.\n")
cat("Suggested write-up structure for students:\n")
cat("1) Show marginal smoking–FEV difference.\n")
cat("2) Show smokers differ in age/height.\n")
cat("3) Show age/height predict FEV.\n")
cat("4) Compare unadjusted vs adjusted smoking coefficients and interpret confounding.\n")
