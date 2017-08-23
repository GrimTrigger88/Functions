# Conceptual FGLS & WLS
require(dplyr)
require(tidyverse)

data(longley)

##### Conceptual FGLS
# Run Baseline OLS
fit <- lm(data = longley, Employed ~ GNP + Population)

# Check correlation between variables and coefficients estimates
summary(fit, cor = TRUE)

# Check autocorrelation in errors

rho <- cor(fit$residuals, lag(fit$residuals), use = "pairwise.complete.obs")

# Use estimate of correlation (rho) to construct Sigma (ith,jth value of Sigma = rho^abs(i-j)) 
# Extract X matrix from model
X <- model.matrix(fit)

Diagonal <- diag(nrow(X)) 
Sigma <- rho^abs(row(Diagonal) - col(Diagonal))

# Invert Sigma
SigmaInv <- solve(Sigma)

# Invert Weighted X Transpose X
XTransXInv <- solve(t(X) %*% SigmaInv %*% X)

# Estimate Beta vector = (X' Sigma^-1 X)^-1 X' Sigma^-1 y)

Beta <- XTransXInv %*% t(X) %*% SigmaInv %*% longley$Employed

# Estimate standard errors
Residuals <- longley$Employed - X %*% Beta
# DF-adjusted RMSE
RMSE <- sqrt(sum(fit$residuals^2)/fit$df.residual)
# Standard Errors
sqrt(diag(XTransXInv)) * RMSE

# New estimate of rho (iterate process to convergence)
cor(Residuals, lag(Residuals), use = "pairwise.complete.obs")


### Altervatively, triangular matrices from the Cholesky decomposition can be used to acieve the same result
# Using Sigma = SS', multiply the S^-1 through both sides of the regression
SInv <- solve(t(chol(Sigma)))

SInvX <- SInv %*% X
SInvY <- SInv %*% longley$Employed

# Run regression, removing the constant term since X already includes it
# rho estimate would be the same, as X and Beta are unchanged
fit2 <- lm(SInvY ~ SInvX-1)







