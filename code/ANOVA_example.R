Y1 <- rpois(5, 2)
Y2 <- rpois(5, 4)
Y3 <- rpois(5, 1)
groups <- rep(c(1, 2, 3), each = 5)
Y <- c(Y1, Y2, Y3)

fit <- lm(Y ~ factor(groups))
summary(fit)
anova(fit)

summary.aov(aov(Y ~ factor(groups)))
            
            