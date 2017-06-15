#' Evaluation on nominal class attribute
#'
#' @param validation whether evaluating on validation or test set
#' @param seed random generator seed
Evaluation.Class <- function(intervals = c(1,5,30,200,1000), validation = FALSE, seed = 1, top.attr = 0, remove.nonsig = TRUE) {
  set.seed(seed)

  cat("\n")
  cat("Evaluating on nominal class attribute (",if(validation) "validation" else "test"," set)\n", sep = "")

  results <- data.frame(actual = numeric(0), predicted = numeric(0))

  dataset.train.name <- paste("./DataProcess/Datasets/dataset.train",".RData",sep = "")
  dataset.val.name <- paste("./DataProcess/Datasets/dataset.val",".RData",sep = "")
  dataset.test.name <- paste("./DataProcess/Datasets/dataset.test",".RData",sep = "")

  load(dataset.train.name)
  load(dataset.val.name)
  load(dataset.test.name)

  if(validation) {
    dataset.test <- dataset.val
  }

  predictions <- rep(which(as.vector(table(dataset.train$Class)) == max(as.numeric(as.character(as.vector(table(dataset.train$Class)))))), nrow(dataset.test))
  predictions <- factor(predictions, levels = levels(dataset.train$Class))

  cat("Original size of train:", nrow(dataset.train), "\n")

  # indices.repeated <- c()
  # for (i in 1:nrow(dataset.train)) {
  #   pl <- ceiling(dataset.train$Players[i])
  #   if(pl > 3) {
  #     pl <- pl - 2
  #   } else {
  #     pl <- 1
  #   }
  #   indices.repeated <- c(indices.repeated, rep(i,pl))
  # }
  # dataset.train <- dataset.train[indices.repeated,]


  # Players is continuous, Class is nominal based on given splits
  dataset.train$Class <- NA

  for (i in 1:nrow(dataset.train)) {
    intervals.tmp <- c(intervals, dataset.train$Players[i])
    intervals.tmp <- sort(intervals.tmp)
    dataset.train$Class[i] <- match(dataset.train$Players[i], intervals.tmp)
  }

  dataset.train$Class <- as.factor(dataset.train$Class)

  dataset.test$Class <- NA

  for (i in 1:nrow(dataset.test)) {
    intervals.tmp <- c(intervals, dataset.test$Players[i])
    intervals.tmp <- sort(intervals.tmp)
    dataset.test$Class[i] <- match(dataset.test$Players[i], intervals.tmp)
  }

  dataset.test$Class <- as.factor(dataset.test$Class)

  dataset.train <- dataset.train[,-which(colnames(dataset.train) == "Players")]
  dataset.test <- dataset.test[,-which(colnames(dataset.test) == "Players")]

  #baseline
  predictions <- rep(which(as.vector(table(dataset.train$Class)) == max(as.vector(table(dataset.train$Class)))), nrow(dataset.test))
  predictions <- factor(predictions, levels = levels(dataset.train$Class))


  # classes <- lapply(c(1:length(levels(dataset.train$Class))), function(x) dataset.train[dataset.train$Class == x,])
  # dataset.train.tmp <- classes[[1]][sample(nrow(classes[[1]]), nrow(classes[[1]]) / 2, replace = F),]
  #
  # for (i in 2:length(levels(dataset.train$Class))) {
  #   dataset.train.tmp <- rbind(dataset.train.tmp, classes[[i]][sample(nrow(classes[[i]]), nrow(classes[[i]]) * (i - 0), replace = T),])
  # }
  # dataset.train <- dataset.train.tmp



  cat("Final size of train:", nrow(dataset.train), "\n")
  cat("Size of test:", nrow(dataset.test), "\n")
  cat("Number of attributes:", ncol(dataset.test)-1, "\n\n")

  if(remove.nonsig) {
    score <- chi.squared(Class~., dataset.train)

    threshold <- 0.0

    dataset.train <- dataset.train[,score$attr_importance > threshold]
    dataset.test <- dataset.test[,score$attr_importance > threshold]
  }

  score <- chi.squared(Class~., dataset.train)

  if(top.attr > 0 && top.attr < ncol(dataset.train)) {

    players.train <- dataset.train$Class
    players.test <- dataset.test$Class

    dataset.train <- dataset.train[,order(score$attr_importance, decreasing = TRUE)[1:top.attr]]
    dataset.test <- dataset.test[,order(score$attr_importance, decreasing = TRUE)[1:top.attr]]

    dataset.train$Class <- players.train
    dataset.test$Class <- players.test
  }

  actual <- dataset.test$Class

  # model <- rpart(Class ~ ., data = dataset.train, method = "class", control = rpart.control(minsplit = 20 ,cp = 0.005))
  # model <- glm(Class ~ ., data = dataset.train, family = binomial(probit))
  # model <- multinom(Class ~ ., data = dataset.train)
  # model <- randomForest(Class ~ ., data = dataset.train, ntree = 500)
  # model <- svm(Class ~ ., data = dataset.train, kernel = "polynomial", coef0 = 0.5, gamma = 0.003)
  # model <- naiveBayes(Class ~ ., data = dataset.train)


  dataset.test <- dataset.test[,-c(ncol(dataset.test))]

  # predictions <- predict(model, newdata = dataset.test)

  # predictions <- predict(model, newdata = dataset.test, type = "class")

  # predictions <- round(predict(model, newdata = dataset.test, type = "response"), 0)+1
  # predictions <- factor(predictions, levels = levels(dataset.train$Class))

  # predictions.tmp <- numeric(length(predictions))
  # pred.thrs <- 0.5
  # predictions.tmp[predictions > pred.thrs] <- 1
  # predictions <- factor(predictions.tmp, levels = c("0", "1"))
  # levels(predictions) <- c("1", "2")



  results <- rbind(results, data.frame(actual = as.numeric(actual), predictions = as.numeric(predictions)))

  cat("Accuracy:", round(nrow(subset(results, actual == predictions))/nrow(results), 3)*100, "\n")
  cat("Baseline:", round(nrow(subset(results, actual == which(as.vector(table(results)) == max(as.vector(table(results))))))/nrow(results), 3)*100, "\n")

  cat(nrow(subset(results, actual <= predictions + 1 & actual >= predictions - 1))/nrow(results), "\n")
  cat(nrow(subset(results, actual <= 3))/nrow(results), "\n")

  if(length(levels(dataset.train$Class)) == 2) {
    cm <- confusionMatrix(predictions, actual, positive = "2")
  } else {
    cm <- confusionMatrix(predictions, actual)
  }

  print(cm$byClass)
  print(cm$table)
}
