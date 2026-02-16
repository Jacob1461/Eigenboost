iris_prediction <- function(){
  set.seed(123)
  iris2 <- iris[iris$Species != "setosa",]
  rownames(iris2) <- NULL
  iris2$Species <- ifelse(iris2$Species == "versicolor", 1, -1)

  model <- eigenboost(X = iris2[1:75, 1:4], y = as.numeric(iris2[1:75, 5]))

  preds <- predict.eigenboost(model, iris2[76:100,1:4])

  accuracy <- mean(preds == as.numeric(iris2[76:100, 5]))

  accuracy
}

testthat::test_that("Iris dataset test", {
  testthat::expect_equal(iris_prediction(), 0.88)
})
