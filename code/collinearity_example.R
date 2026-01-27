# Fake regression data with strong multicollinearity
# - x1 and x2 are ~0.98-0.999 correlated (by construction)
# - y depends on x1 and x3; x2 is mostly redundant with x1

set.seed(20260125)

n <- 250

x1 <- rnorm(n)
x2 <- x1 + rnorm(n, sd = 0.05)   # small noise => strong collinearity with x1
x3 <- rnorm(n)

# True model: y = 2 + 3*x1 - 1.5*x3 + noise
eps <- rnorm(n, sd = 1)
y <- 2 + 3*x1 - 1.5*x3 + eps

dat <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

# Quick checks
cor(dat[, c("x1", "x2", "x3")])              # x1-x2 should be very high
summary(lm(y ~ x1 + x2 + x3, data = dat))    # unstable x1/x2 coefficients expected

# Optional: VIF (requires the 'car' package)
# install.packages("car")
# library(car)
# vif(lm(y ~ x1 + x2 + x3, data = dat))

# Assume the data frame 'dat' has already been created as in the previous example

# Write to CSV
write.csv(
  dat,
  file = "collinear_linear_regression_data.csv",
  row.names = FALSE
)
