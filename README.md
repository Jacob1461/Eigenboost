
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eigenboost

<!-- badges: start -->

<!-- badges: end -->

Fit shallower decision trees in Adaboost by reflecting the feature space
to align the dominant direction of variance with a feature axis.

Algorithm was designed as part of work on the thesis [“Boosting in
Reflected Feature
Spaces”](https://ir.canterbury.ac.nz/server/api/core/bitstreams/855caa4c-e4ae-4aba-bbe2-ce9be126092d/content),
in partial requirement of the degree of Masters of Mathematical Sciences
(endorsed in Data Science).

## Installation

You can install the development version of eigenboost like so:

``` r
devtools::install_github("Jacob1461/Eigenboost")
```

## Example

In this example, the iris dataset is adapted to be a binary class
problem by removing the Setosa species. The remaining species are
encoded as -1 and 1. An eigenboost model is fit to the training data,
the model reflects the dataset using the dominant direction of the the
class which leads to the greatest reduction in training error. The CART
trees are by default left as stumps (i.e. depth 1 trees). The
reflection_type hyperparameter changes the behavior of the dataset
reflection, other options include: identity for regular Adaboost
(i.e. no reflections), eigen for dominant eigenvector based dataset
refections, random to reflect the dataset in a random direction, and
eigen-random for a combination of the eigenvector based and random based
dataset reflections.

``` r
library(eigenboost)

set.seed(123)

# Make iris into a binary class classification dataset
iris2 <- iris[iris$Species != "setosa", ]
rownames(iris2) <- NULL
iris2$Species <- ifelse(iris2$Species == "versicolor", 1, -1)

# Make training and testing splits
train_indx <- sample(nrow(iris2), 65)
X_train <- iris2[train_indx, 1:4]
y_train <- as.numeric(iris2$Species[train_indx])
X_test <- iris2[-train_indx, 1:4]
y_test <- as.numeric(iris2$Species[-train_indx])

# Fit model

model <- eigenboost(X = X_train, y = y_train, reflection_every = 3, reflections_type = "eigen", tree_depth = 1)


# Make prediction

preds <- predict.eigenboost(model, X_test)

# Evaluate

accuracy <- mean(preds == y_test)
accuracy
#> [1] 0.9714286
```
