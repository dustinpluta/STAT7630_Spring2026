### HW3: After variance fix, check functional form

set.seed(303)

# ----- Data generation -----
n <- 160
beta0 <- 3
beta1 <- 1.0

x <- runif(n, 0.5, 10)  # start at 0.5 to keep mu away from 0
mu <- beta0 + beta1 * x

eps <- rnorm(n, 0, sd = sqrt(mu))
y <- mu + eps

dat <- data.frame(x = x, y = y, mu = mu)

# ----- TODO 1: Naive OLS fit -----

# ----- TODO 2: Diagnostics for fit0 -----

# ----- TODO 3: Apply a variance-stabilizing transformation -----

# ----- TODO 4: Diagnostics for fit1 -----

# ----- TODO 5: Check for curvature after transformation -----

# ----- TODO 6: If curvature is present, refit with a simple extension -----

# ----- TODO 7: Diagnostics for your final fit (fit1 or fit2) -----

# ----- TODO 8: 95% CI for the slope parameter associated with x in FINAL model -----
