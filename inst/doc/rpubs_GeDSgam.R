## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  dpi = 300,
  fig.align = "center",
  out.width = "80%",
  echo = TRUE,
  message = FALSE,
  warning = FALSE
)


## ----message=FALSE, warning=FALSE---------------------------------------------
# install.packages("GeDS")
library("GeDS")

## ----message=FALSE, warning=FALSE---------------------------------------------
library(rpart)
car_data <- car.test.frame
Gmodgam <- NGeDSgam(Mileage ~ f(Price) + Country + Type + f(Weight) + f(Disp.) + f(HP),
                    data = car_data, phi = 0.95)



## ----results = 'hide'---------------------------------------------------------
# Linear GAM-GeDS Fit
coef(Gmodgam, n = 2)
# Quadratic GAM-GeDS Fit
coef(Gmodgam, n = 3)
# Cubic GAM-GeDS Fit
coef(Gmodgam, n = 4)

## -----------------------------------------------------------------------------
# Linear GAM-GeDS Fit
knots(Gmodgam, n = 2)
# Quadratic GAM-GeDS Fit
knots(Gmodgam, n = 3)
# Cubic GAM-GeDS Fit
knots(Gmodgam, n = 4)

## -----------------------------------------------------------------------------
# Linear GAM-GeDS Fit
deviance(Gmodgam, n = 2)
# Quadratic GAM-GeDS Fit
deviance(Gmodgam, n = 3)
# Cubic GAM-GeDS Fit
deviance(Gmodgam, n = 4)

## ----fig.width=10, fig.height=6-----------------------------------------------
# Linear GAM-GeDS Fit
layout(matrix(c(1,2), nrow=1, byrow=TRUE))
plot(Gmodgam, n = 2, col = "steelblue")

# Quadratic GAM-GeDS Fit
layout(matrix(c(1,2), nrow=1, byrow=TRUE))
plot(Gmodgam, n = 3, col = "steelblue")

# Cubic GAM-GeDS Fit
layout(matrix(c(1,2), nrow=1, byrow=TRUE))
plot(Gmodgam, n = 4, col = "steelblue")

## ----message=FALSE, warning=FALSE, results = 'hide'---------------------------
# Set seed for reproducibility
set.seed(123)
# Determine the size of the dataset
n <- nrow(car_data)
# Create a random sample of row indices for the training set
trainIndex <- sample(1:n, size = floor(0.8 * n))
# Subset the data into training and test sets
train <- car_data[trainIndex, ]
test <- car_data[-trainIndex, ]

Gmodgam <- NGeDSgam(Mileage ~ f(Price) + Country + Type + f(Weight) + f(Disp.) + f(HP),
                    data = train, phi = 0.9)


## -----------------------------------------------------------------------------
mean((test$Mileage - predict(Gmodgam, newdata = test, n = 2))^2)
mean((test$Mileage - predict(Gmodgam, newdata = test, n = 3))^2)
mean((test$Mileage - predict(Gmodgam, newdata = test, n = 4))^2)


## ----message=FALSE, warning=FALSE, results = 'hide'---------------------------
Gmodgam <- NGeDSgam(Price ~ f(Mileage) + Country + Type + f(Weight) + f(Disp.) + f(HP),
                    data = train, family = Gamma(link=log), phi = 0.9)

