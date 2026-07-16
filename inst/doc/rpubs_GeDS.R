## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  dpi = 300,
  fig.align = "center",
  out.width = "80%",
  echo = TRUE,
  message = FALSE,
  warning = FALSE
)


## ----message=FALSE------------------------------------------------------------
# install.packages("GeDS")
library("GeDS")

## -----------------------------------------------------------------------------
# Generate a data sample for the response variable
# Y and the single covariate X
set.seed(123)
N <- 500
f_1 <- function(x) (10*x/(1+100*x^2))*4+4
X <- sort(runif(N, min = -2, max = 2))
# Specify a model for the mean of Y to include only a component
# non-linear in X, defined by the function f_1
means <- f_1(X)
# Add (Normal) noise to the mean of Y
Y <- rnorm(N, means, sd = 0.2)

## ----fig.width=10, fig.height=6-----------------------------------------------
plot(X, Y, pch = 20, col = "darkgrey")

## -----------------------------------------------------------------------------
# Fit a Normal GeDS regression using NGeDS
(Gmod <- NGeDS(Y ~ f(X), beta = 0.6, phi = 0.995, Xextr = c(-2,2)))

## ----fig.width=10, fig.height=5-----------------------------------------------
layout(matrix(c(1,2), nrow=1, byrow=TRUE))
plot(Gmod, n = 2, which = 1:(Gmod$Nintknots+1), legend.pos = NA, pch = 20, col = "darkgrey")

## ----fig.width=10, fig.height=6-----------------------------------------------
layout(matrix(c(1), nrow=1, byrow=TRUE))
plot(Gmod, f = f_1, n = 3, pch = 20, col = "darkgrey")

## ----fig.width=10, fig.height=6-----------------------------------------------
# Generate a data sample for the response variable Y and the covariate X
# See section 4.1 in Dimitrova et al. (2023)
set.seed(123)
N <- 500
f_1 <- function(x) (10*x/(1+100*x^2))*4+4
X <- sort(runif(N, min = -2, max = 2))
# Specify a model for the mean of Y to include only a component
# non-linear in X, defined by the function f_1
means <- exp(f_1(X))

## ----fig.width=10, fig.height=6-----------------------------------------------
#########################################
## (A) Y ~ Poisson + log link function ##
#########################################
# Generate Poisson distributed Y according to the mean model
Y <- rpois(N, means)
# Fit a Poisson GeDS regression using GGeDS
Gmod <- GGeDS(Y ~ f(X), beta = 0.2, phi = 0.995, family = "poisson",
              Xextr = c(-2,2))
plot(Gmod, f = function(x) exp(f_1(x)), n = 3, pch = 20, col = "darkgrey")


## ----fig.width=10, fig.height=6-----------------------------------------------
#######################################
## (B) Y ~ Gamma + log link function ##
#######################################
# Generate Gamma distributed Y according to the mean model
Y <- rgamma(N, shape = means, rate = 0.1)
# Fit a Gamma GeDS regression using GGeDS
Gmod <- GGeDS(Y ~ f(X), beta = 0.1, phi = 0.995, family =  Gamma(log),
              Xextr = c(-2,2))
plot(Gmod, f = function(x) exp(f_1(x))/0.1, n = 3, pch = 20, col = "darkgrey")


## ----fig.width=10, fig.height=6-----------------------------------------------
############################################
## (C) Y ~ Binomial + logit link function ##
############################################
# Generate Binomial distributed Y according to the mean model
eta <- f_1(X) - 4
means <- exp(eta)/(1+exp(eta))
Y <- rbinom(N, size = 50, prob = means) / 50
# Fit a Binomial GeDS regression using GGeDS
Gmod <- GGeDS(Y ~ f(X), beta = 0.1, phi = 0.995, family =  "binomial",
              Xextr = c(-2,2))
plot(Gmod, f = function(x) exp(f_1(x) - 4)/(1 + exp(f_1(x) - 4)),
     n = 3, pch = 20, col = "darkgrey")

## ----fig.width=10, fig.height=8-----------------------------------------------
# bivariate example
# See Dimitrova et al. (2023), section 5

# Generate a data sample for the response variable
# Z and the covariates X and Y assuming Normal noise
set.seed(123)
doublesin <- function(x){
 sin(2*x[,1])*sin(2*x[,2])
}

X <- (round(runif(400, min = 0, max = 3),2))
Y <- (round(runif(400, min = 0, max = 3),2))
Z <- doublesin(cbind(X,Y))
Z <- Z + rnorm(400, 0, sd = 0.2)
# Fit a two dimensional GeDS model using NGeDS
(BivGeDS <- NGeDS(Z ~ f(X, Y), beta = 0.3, phi = 0.95,
Xextr = c(0, 3), Yextr = c(0, 3)))

# Extract quadratic coefficients/knots/deviance
coef(BivGeDS, n = 3)
knots(BivGeDS, n = 3)
deviance(BivGeDS, n = 3)

# RSS w.r.t true function
f_XY <- apply(cbind(X, Y), 1, function(row) doublesin(matrix(row, ncol = 2)))
mean((f_XY- Gmod$Quadratic.Fit$Predicted)^2)

# Surface plot of the generating function (doublesin)
plot(BivGeDS, f = doublesin)

## ----fig.width=10, fig.height=8-----------------------------------------------
# bivariate example
# See Dimitrova et al. (2023), section 5

# Generate a data sample for the response variable
# Z and the covariates X and Y assuming Poisson distributed error
set.seed(123)
doublesin <- function(x) {
# Adjusting the output to ensure it's positive
exp(sin(2*x[,1]) + sin(2*x[,2]))
}
X <- round(runif(400, min = 0, max = 3), 2)
Y <- round(runif(400, min = 0, max = 3), 2)
# Calculate lambda for Poisson distribution
lambda <- doublesin(cbind(X,Y))
# Generate Z from Poisson distribution
Z <- rpois(400, lambda)
data <- data.frame(X, Y, Z)

# Fit a Poisson GeDS regression using GGeDS
(BivGeDS <- GGeDS(Z ~ f(X,Y), beta = 0.2, phi = 0.99, family = "poisson"))

# Extract quadratic coefficients/knots/deviance
coef(BivGeDS, n = 3)
knots(BivGeDS, n = 3)
deviance(BivGeDS, n = 3)

# Poisson deviance w.r.t true function
f_XY <- apply(cbind(X, Y), 1, function(row) doublesin(matrix(row, ncol = 2)))
sum(poisson()$dev.resids(f_XY, BivGeDS$Quadratic.Fit$Predicted, wt = 1))

# Surface plot of the generating function (doublesin)
plot(BivGeDS, f = doublesin)

