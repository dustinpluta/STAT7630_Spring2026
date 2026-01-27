library(alr4)
library(ggplot2)

data(UN11)

dat <- UN11

head(dat)
summary(dat$region)
summary(dat$group)
summary(dat$pctUrban)
summary(dat$ppgdp)
summary(dat$lifeExpF)

colnames(dat)
####

# Exploratory data analysis

# Univariate Plots

# LE: looks left-skewed
ggplot(dat) + 
  geom_histogram(aes(x = lifeExpF))

# summary of group
summary(dat$group)
summary(dat$group) / nrow(dat)

# Other covariates
ggplot(dat) + 
  geom_histogram(aes(x = fertility))
ggplot(dat) + 
  geom_histogram(aes(x = ppgdp))
ggplot(dat) + 
  geom_histogram(aes(x = pctUrban))

# Bivariate plots
ggplot(dat) + 
  geom_boxplot(aes(x = group, y = lifeExpF,
                   fill = group))
ggplot(dat) + 
  geom_histogram(aes(x = lifeExpF, fill = group))
ggplot(dat) + 
  geom_density(aes(x = lifeExpF, fill = group))

ggplot(dat) + 
  geom_point(aes(x = fertility, y = lifeExpF))
ggplot(dat) + 
  geom_histogram(aes(x = fertility, fill = group))

ggplot(dat) + 
  geom_point(aes(x = ppgdp, y = lifeExpF))
ggplot(dat) + 
  geom_point(aes(x = log(ppgdp), y = lifeExpF))

ggplot(dat) + 
  geom_histogram(aes(x = fertility, fill = group))
ggplot(dat) + 
  geom_point(aes(x = fertility, y = lifeExpF))

ggplot(dat) + 
  geom_histogram(aes(x = pctUrban, fill = group))
ggplot(dat) + 
  geom_point(aes(x = pctUrban, y = lifeExpF))
cor(dat$pctUrban, dat$lifeExpF)

#### Modeling

dat$loggdp <- log(dat$ppgdp)

# Model 1: LE ~ group
fit1 <- lm(lifeExpF ~ group, data = dat)
summary(fit1)
plot(fit1)

fit1b <- lm(lifeExpF ~ group - 1, data = dat)
summary(fit1b)

# Model 2: LE ~ group + fertility + log(gdp)
fit2 <- lm(lifeExpF ~ group + loggdp + fertility, 
           data = dat)
summary(fit2)

# Modelling interactions in the UN11 data

fit3 <- lm(lifeExpF ~ group + loggdp - 1, data = dat)
summary(fit3)

fit4 <- lm(lifeExpF ~ group + loggdp + group:loggdp - 1, data = dat)
summary(fit4)
