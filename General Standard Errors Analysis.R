# Standard Error Analysis

require(dplyr)
require(tidyverse)
require(Ecdat)

data(Fatality)

# Massage Data

FatalityData <- Fatality %>%
                  rename(fatalityrate = mrall,
                         milesperdriver = vmiles,
                         unemployment = unrate) %>%
                  mutate(state = as.factor(state))

# Create summy variables from categorical
Fatalities <- model.matrix(data = FatalityData, jaild ~ year + fatalityrate + beertax + mlda + milesperdriver + unemployment + perinc + state)

# Drop created intercept
Fatalities <- data.frame(Fatalities[, 2:ncol(Fatalities)])

# Baseline regression
fit <- lm(data = Fatalities, year ~ .)

# Verify regression output
X <- model.matrix(fit)
y <- Fatalities$year

XPrimeXInv <- solve(t(X)%*%X)
XPrimeXInv %*% t(x) %*% y
# All check out
data.frame(XPrimeXInv %*% t(X) %*% y, fit$coefficients)

# Conventional covariance matrix estimate
OmegaConv <- XPrimeXInv * mean(fit$residuals^2)

# White Robust covariance matrix estimate
OmegaRob <- XPrimeXInv %*% t(X) %*% diag(fit$residuals^2) %*% X %*% XPrimeXInv

# DOF-corrected covariance matrix estimate
OmegaDOF <- XPrimeXInv * (sum(fit$residuals^2)/fit$df.residual)

# Resulting Standard Errors
StdErr <- data.frame(Conventional = sqrt(diag(OmegaConv)), DOFCorrected = sqrt(diag(OmegaDOF)), Robust = sqrt(diag(OmegaRob)))

# Projection Matrix
H <- X %*% XPrimeXInv %*% t(X)

# Leverage Matrix - where 0 < L(i) < 1 and sum(L(i)) = count of regressors
# L(i) is the weight of a set of observations on the regression line
# Indicates the presence of influencial observations
L <- data.frame(diag(H))











