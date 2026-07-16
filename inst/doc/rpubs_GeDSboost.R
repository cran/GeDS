## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  dpi=300,
  fig.align = "center",
  out.width = "80%",
  echo = TRUE,
  message = FALSE,
  warning = FALSE
)

## ----message=FALSE, warning=FALSE---------------------------------------------
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
data = data.frame(X, Y)

Gmodboost <- NGeDSboost(Y ~ f(X), data = data)

## ----fig.width=16, fig.height=8-----------------------------------------------
layout(matrix(c(1,2), nrow=1, byrow=TRUE))
visualize_boosting(Gmodboost, 0:N.boost.iter(Gmodboost), final_fits = TRUE)

## -----------------------------------------------------------------------------
# Load the processed version of the Ames dataset
library(AmesHousing)
ames_data <- make_ames()

# Keep only numeric variables
ames_data <- ames_data[sapply(ames_data, is.numeric)]

# Define the response variable and predictors
response <- "Sale_Price"
predictors <- setdiff(names(ames_data), response)

## -----------------------------------------------------------------------------
# Calculate correlations
correlations <- sapply(predictors, function(predictor) {
  cor(ames_data[[predictor]], ames_data[[response]])
})

predictors <- names(correlations[abs(correlations) > 0.3])

ames_data <- ames_data[c(response, predictors)]

print(names(ames_data))

## -----------------------------------------------------------------------------
# Construct the formula for numeric predictors with function `f` applied
predictors_formula_part <- paste0("f(", predictors, ")", collapse = " + ")
# Combine both parts into the final formula
ames_formula <- as.formula(paste(response, "~", predictors_formula_part))

print(ames_formula)

## ----message=FALSE------------------------------------------------------------
# Set the seed for reproducibility
set.seed(123)
# Determine the size of the dataset
n <- nrow(ames_data)
# Create a random sample of row indices for the training set
trainIndex <- sample(1:n, size = floor(0.8 * n))
# Subset the data into training and test sets
train <- ames_data[trainIndex, ]
test <- ames_data[-trainIndex, ]

Gmodboost <- NGeDSboost(formula = ames_formula, data = train,
                        initial_learner = FALSE,
                        phi_boost_exit = 0.999, phi = 0.95)


## -----------------------------------------------------------------------------
sqrt(mean((test[[response]] - predict(Gmodboost, newdata = test, n = 2))^2))
sqrt(mean((test[[response]] - predict(Gmodboost, newdata = test, n = 3))^2))
sqrt(mean((test[[response]] - predict(Gmodboost, newdata = test, n = 4))^2))

## ----fig.width=10, fig.height=6-----------------------------------------------
bl_imp <- bl_imp(Gmodboost)
plot(bl_imp)

## ----fig.width=10, fig.height=6-----------------------------------------------
# Linear FGB-GeDS Fit
layout(matrix(c(1,2), nrow=1, byrow=TRUE))
plot(Gmodboost, n = 2)
# Quadratic FGB-GeDS Fit
plot(Gmodboost, n = 3)
# Cubic FGB-GeDS Fit
plot(Gmodboost, n = 4)

## -----------------------------------------------------------------------------
# Set the seed for reproducibility
set.seed(123)
# Determine the size of the dataset
n <- nrow(ames_data)
# Create a random sample of row indices for the training set
trainIndex <- sample(1:n, size = floor(0.8 * n))
# Subset the data into training and test sets
train <- ames_data[trainIndex, ]
test <- ames_data[-trainIndex, ]

Gmodboost <- NGeDSboost(formula = ames_formula, data = train,
                        initial_learner = FALSE,
                        phi = 0.95, shrinkage = 0.4)

# RMSE
sqrt(mean((test$Sale_Price - predict(Gmodboost, n = 2, newdata = test))^2))

## -----------------------------------------------------------------------------
# Define order of the GeDS models
ord = 3

# versicolor vs setosa:
iris_subset <- subset(iris, Species %in% c("setosa", "versicolor"))
iris_subset$Species <- factor(iris_subset$Species)
mod <- glm(Species ~ Sepal.Length,
             family = binomial(link = "logit"),
             data = iris_subset)
# GeDS
Gmod <- suppressWarnings(GGeDS(Species ~ f(Sepal.Length), data = iris_subset, family = binomial(link = "logit"), phi = 0.8))
# GAM-GeDS
Gmodgam <- suppressWarnings(NGeDSgam(Species ~ f(Sepal.Length), data = iris_subset, family = binomial(link = "logit"), phi = 0.8))
# FGB-GeDS
Gmodboost <- suppressWarnings(NGeDSboost(Species ~ f(Sepal.Length), data = iris_subset, family = mboost::Binomial()))

# Predicted multinomial probabilities
pred_glm <- predict(mod, type = "response")
pred_GeDS <- predict(Gmod, type = "response", n = ord)
pred_GeDSgam <- predict(Gmodgam, type = "response", n = ord)
pred_GeDSboost <- predict(Gmodboost, type = "response", n = ord)

## ----fig.width=10, fig.height=6-----------------------------------------------
# Plot
plot(iris_subset$Sepal.Length, as.numeric(iris_subset$Species) - 1,
     xlab = "Sepal Length",
     ylab = "Probability of Versicolor",
     main = "Classification: setosa vs. versicolor",
     pch = 16, col = "gray40")
# GLM
pred <- data.frame(Sepal.Length = as.vector(iris_subset$Sepal.Length),
                   Predicted = pred_glm)
pred <- pred[order(pred$Sepal.Length), ]
lines(pred, col = "blue")
# GeDS
lines(pred$Sepal.Length, pred_GeDS, col = "red")
# GAM-GeDS
pred <- data.frame(Sepal.Length = Gmodboost$args$predictors$Sepal.Length,
                   pred = pred_GeDSgam)
pred <- pred[order(pred$Sepal.Length), ]
lines(pred, col = "green")
# FGB-GeDS
pred <- data.frame(Sepal.Length = Gmodboost$args$predictors$Sepal.Length,
                   ppred = pred_GeDSboost)
pred <- pred[order(pred$Sepal.Length), ]
lines(pred, col = "purple")

# Legend
legend("topleft", legend = c("GLM (logit)", "GeDS", "GAM-GeDS", "FGB-GeDS"),
       col = c("blue", "red", "green", "purple"),
       lwd = 2, bty = "n")

## -----------------------------------------------------------------------------
# Function to compute the confusion matrix and accuracy
evaluate_model <- function(predicted, actual) {
  # Ensure both are factors with the same levels
  predicted <- factor(predicted, levels = levels(actual))

  # Compute confusion matrix
  conf_matrix <- table(Predicted = predicted, Actual = actual)

  # Compute accuracy
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

  # Return results as a list
  return(list(
    Confusion_Matrix = conf_matrix,
    Accuracy = accuracy
  ))
}

## -----------------------------------------------------------------------------
compute_multinomial_probs <- function(mod_1, mod_2, newdata, n) {

  # Compute linear predictors
  if (missing(n)) {
    eta1 <- predict(mod_1, newdata = newdata, type = "link")
    eta2 <- predict(mod_2, newdata = newdata, type = "link")
  } else {
    # For GeDS models the order needs to be specified
    eta1 <- predict(mod_1, newdata = newdata, type = "link", n = n)
    eta2 <- predict(mod_2, newdata = newdata, type = "link", n = n)
  }

  # Normalize exponentials for numerical stability (Log-Sum-Exp Trick)
  max_eta <- pmax(0, eta1, eta2)

  # Compute stabilized exponentials
  exp_setosa     <- exp(0 - max_eta) # i.e., \approx 1
  exp_versicolor <- exp(eta1 - max_eta)
  exp_virginica  <- exp(eta2 - max_eta)

  # Calculate the denominator using the stabilized values
  denom <- exp_setosa + exp_versicolor + exp_virginica

  # Compute multinomial probabilities
  p_setosa     <- exp_setosa / denom
  p_versicolor <- exp_versicolor / denom
  p_virginica  <- exp_virginica / denom

  # Combine results in a data frame
  pred_probs <- data.frame(
    setosa = p_setosa,
    versicolor = p_versicolor,
    virginica = p_virginica
  )

  return(pred_probs)
}

## -----------------------------------------------------------------------------
multinomial_logit <- function(data, response, predictors, base_class,
                                    method = NGeDSboost, params = list()) {
  # Ensure response is a factor
  data[[response]] <- factor(data[[response]])

  # Identify the other classes
  classes <- levels(data[[response]])
  classes <- classes[classes != base_class]

  # Construct the formula dynamically with f() applied to each predictor
  predictor_formula <- paste(sprintf("f(%s)", predictors), collapse = " + ")
  formula_str <- paste(response, "~", predictor_formula)

  # Train binomial models
  models <- lapply(classes, function(class) {
    subset_data <- subset(data, data[[response]] %in% c(base_class, class))
    subset_data[[response]] <- factor(subset_data[[response]])
    suppressWarnings(do.call(method, c(list(as.formula(formula_str), data = subset_data), params)))
  })

  # Compute multinomial probabilities
  pred_probs <- suppressWarnings(compute_multinomial_probs(models[[1]], models[[2]], newdata = data))

  # Predict classes
  predicted_classes <- factor(
    apply(pred_probs, 1, function(x) colnames(pred_probs)[which.max(x)]),
    levels = colnames(pred_probs)
  )

  # Evaluate model
  evaluation_results <- evaluate_model(predicted_classes, data[[response]])

  return(list(probabilities = pred_probs, predicted_classes = predicted_classes, evaluation = evaluation_results))
}

## -----------------------------------------------------------------------------
# Example usage:
# NGeDSboost
results <- multinomial_logit(iris, response = "Species",
                             predictors = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                             base_class = "setosa",
                             method = NGeDSboost,
                             params = list(
                               family = mboost::Binomial(),
                               phi = 0.99,
                               q = 2
                               ))

# Access results:
print(head(results$probabilities))
print(head(results$predicted_classes))
print(results$evaluation)

