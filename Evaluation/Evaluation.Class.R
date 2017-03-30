#' Evaluation on nominal class attribute
#'
#' @param validation whether evaluating on validation or test set
#' @param seed random generator seed
Evaluation.Class <- function(validation = FALSE, seed = 1) {
  set.seed(seed)

  cat("\n")
  cat("Evaluating on nominal class attribute (",if(validation) "validation" else "test"," set)\n", sep = "")

  results <- data.frame(actual = numeric(0), predicted = numeric(0))

  dataset.train.name <- paste("./DataProcess/Datasets/dataset.train.class",".RData",sep = "")
  dataset.val.name <- paste("./DataProcess/Datasets/dataset.val.class",".RData",sep = "")
  dataset.test.name <- paste("./DataProcess/Datasets/dataset.test.class",".RData",sep = "")

  load(dataset.train.name)
  load(dataset.val.name)
  load(dataset.test.name)

  if(validation) {
    dataset.test <- dataset.val
  }

  actual <- dataset.test$Class

  # model <- randomForest(Class ~ ., data = dataset.train, ntree = 400, mtry = 5)
  model <- svm(Class ~ ., data = dataset.train, kernel = "polynomial", coef0 = 0.5, gamma = 0.001)
  # model <- naiveBayes(Class ~ ., data = dataset.train)
  # model <- rpart(Class ~ ., data = dataset.train, method = "class", control = rpart.control(cp = 0.0005))

  dataset.test <- dataset.test[,-c(ncol(dataset.test))]

  predictions <- predict(model, newdata = dataset.test, type = "class")

  results <- rbind(results, data.frame(actual = as.numeric(actual), predictions = as.numeric(predictions)))

  cat(nrow(subset(results, actual == predictions))/nrow(results), "\n")
  cat(nrow(subset(results, actual == 1))/nrow(results), "\n")

  # cat(nrow(subset(results, actual <= predictions + 1 & actual >= predictions - 1))/nrow(results), "\n")
  # cat(nrow(subset(results, actual <= 3))/nrow(results), "\n")

  cm <- confusionMatrix(predictions, actual, positive = "2")

  print(cm$byClass)
  print(cm$table)
}
