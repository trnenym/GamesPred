#' Evaluation on continuous class attribute
#'
#' @param validation whether evaluating on validation or test set
#' @param seed random generator seed
Evaluation.Reg <- function(validation = FALSE, seed = 1, top.attr = 0, remove.nonsig = TRUE) {
  set.seed(seed)

  cat("\n")
  cat("Evaluating on continuous class attribute (",if(validation) "validation" else "test"," set)\n", sep = "")

  results <- data.frame(actual = numeric(0), predicted = numeric(0))

  dataset.train.name <- paste("./DataProcess/Datasets/dataset.train",".RData",sep = "")
  dataset.val.name <- paste("./DataProcess/Datasets/dataset.val",".RData",sep = "")
  dataset.test.name <- paste("./DataProcess/Datasets/dataset.test",".RData",sep = "")

  load(dataset.train.name)
  load(dataset.val.name)
  load(dataset.test.name)

  players.train <- dataset.train$Players
  players.test <- dataset.test$Players

  dataset.train <- na.roughfix(dataset.train[,-ncol(dataset.train)])
  dataset.test <- na.roughfix(dataset.test[,-ncol(dataset.test)])

  dataset.train$Players <- players.train
  dataset.test$Players <- players.test

  if(validation) {
    dataset.test <- dataset.val
  }

  cat("Original size of train:", nrow(dataset.train), "\n")
  cat("Original attributes:", ncol(dataset.train), "\n")

  # log adjustment to players
  dataset.train$Players <- log(dataset.train$Players + 1, base = 2)
  dataset.train$Players <- round(dataset.train$Players, 2)

  dataset.test$Players <- log(dataset.test$Players + 1, base = 2)
  dataset.test$Players <- round(dataset.test$Players, 2)

  indices.repeated <- c()
  for (i in 1:nrow(dataset.train)) {
    pl <- ceiling(dataset.train$Players[i])
    if(pl > 3) {
      pl <- pl - 2
    } else {
      pl <- 1
    }
    indices.repeated <- c(indices.repeated, rep(i,pl))
  }
  dataset.train <- dataset.train[indices.repeated,]



  if(remove.nonsig) {
    score <- chi.squared(Players~., dataset.train)

    threshold <- 0.0

    dataset.train <- dataset.train[,score$attr_importance > threshold]
    dataset.test <- dataset.test[,score$attr_importance > threshold]
  }

  score <- chi.squared(Players~., dataset.train)

  if(top.attr > 0 && top.attr < ncol(dataset.train)) {

    players.train <- dataset.train$Players
    players.test <- dataset.test$Players

    dataset.train <- dataset.train[,order(score$attr_importance, decreasing = TRUE)[1:top.attr]]
    dataset.test <- dataset.test[,order(score$attr_importance, decreasing = TRUE)[1:top.attr]]

    dataset.train$Players <- players.train
    dataset.test$Players <- players.test
  }

  attrs <- colnames(dataset.train)
  save(attrs, file = "./Prediction/attrs.RData")

  cat("Final size of train:", nrow(dataset.train), "\n")
  cat("Size of test:", nrow(dataset.test), "\n")
  cat("Number of attributes:", ncol(dataset.test)-1, "\n\n")

  actual <- dataset.test$Players

  # model.label <- "Linear Regression"
  # model <- lm(Players ~., data = dataset.train)
  # model.label <- "Decision Tree"
  # model <- rpart(Players ~ ., data = dataset.train, method = "anova", control = rpart.control(minsplit = 20 ,cp = 0.005))
  # model.label <- "Random Forest"
  # model <- randomForest(Players ~ ., data = dataset.train, ntree = 500)
  # model.label <- "Gaussian Process"
  # model <- gausspr(Players ~ ., data = dataset.train)
  model.label <- "SVM"
  model <- svm(Players ~ ., data = dataset.train, kernel = "polynomial", coef0 = 0.5, gamma = 0.003, na.action = na.omit)


  # names <- colnames(dataset.train)
  # formula <- as.formula(paste("Players ~", paste(names[!names %in% "Players"], collapse = " + ")))
  # model <- neuralnet(formula, data = dataset.train, hidden = c(32, 8), threshold = 0.3)

  save(model, file = "./Prediction/model.RData")

  dataset.test <- dataset.test[,-c(ncol(dataset.test))]

  predictions <- predict(model, newdata = dataset.test)

  # predictions <- compute(model, dataset.test)
  # predictions <- predictions$net.result[,1]

  predictions[predictions<0] <- 0

  results <- rbind(results, data.frame(actual, predictions))

  print(summary(results$predictions))

  cat("Results: \n\n")

  cat("Intervals by the most examples as baseline: \n")
  cat("+-1 from actual:", round(nrow(subset(results, actual <= predictions + 1 & actual >= predictions - 1))/nrow(results)*100, 1))
  cat(" with baseline:", round(nrow(subset(results, actual <= 2+min(actual)))/nrow(results)*100, 1), "\n")

  cat("+-2 from actual:", round(nrow(subset(results, actual <= predictions + 2 & actual >= predictions - 2))/nrow(results)*100, 1))
  cat(" with baseline:", round(nrow(subset(results, actual <= 4+min(actual)))/nrow(results)*100, 1), "\n")

  cat("+-3 from actual:", round(nrow(subset(results, actual <= predictions + 3 & actual >= predictions - 3))/nrow(results)*100, 1))
  cat(" with baseline:", round(nrow(subset(results, actual <= 6+min(actual)))/nrow(results)*100, 1), "\n\n")

  cat("Intervals from median as baseline: \n")
  cat("+-1 from actual:", round(nrow(subset(results, actual <= predictions + 1 & actual >= predictions - 1))/nrow(results)*100, 1))
  cat(" with baseline:", round(nrow(subset(results, abs(actual-median(actual)) <= 1))/nrow(results)*100, 1), "\n")

  cat("+-2 from actual:", round(nrow(subset(results, actual <= predictions + 2 & actual >= predictions - 2))/nrow(results)*100, 1))
  cat(" with baseline:", round(nrow(subset(results, abs(actual-median(actual)) <= 2))/nrow(results)*100, 1), "\n")

  cat("+-3 from actual:", round(nrow(subset(results, actual <= predictions + 3 & actual >= predictions - 3))/nrow(results)*100, 1))
  cat(" with baseline:", round(nrow(subset(results, abs(actual-median(actual)) <= 3))/nrow(results)*100, 1), "\n\n")

  cat("MAE:", round(mae(results$actual, results$predictions), 2), "\n")
  cat("RMSE:", round(rmse(results$actual, results$predictions), 2), "\n")
  cat("RRSE:", round(sqrt(sum((results$actual - results$predictions)**2)/sum((results$actual - mean(results$actual))**2))*100, 2), "\n")
  # cat("NRMSE:", rmse(results$actual, results$predictions) / sd(results$actual), "\n")
  cat("COR:", round(cor(results$actual, results$predictions, method = "pearson"), 2), "\n")

  results.ordered <- results[order(results$actual),]
  table.actual <- table(results$actual)

  old.par <- par(no.readonly=T)

  windows(width=8, height=8)
  par(mar=c(5, 5, 4, 2) + 0.1)

  plot(results$predictions, results$actual, xlim = c(0,16), ylim = c(0,16), col = "red", las = 1
       , main = model.label
       , xlab = "predicted", ylab = "actual", cex.lab=1.5, cex.main=1.7, cex.axis=1.5)
  abline(0,1)
  abline(2,1, col = "gray")
  abline(-2,1, col = "gray")

  par(old.par)

  # Save predictions
  dataset.test.predictions <- dataset.test
  dataset.test.predictions$ID <- rownames(dataset.test)
  dataset.test.predictions$ActualPlayers <- actual
  dataset.test.predictions$PredictedPlayers <- predictions
  dataset.test.predictions <- dataset.test.predictions[,-c(1:(ncol(dataset.test.predictions)-3))]

  save(dataset.test.predictions, file = "./DataProcess/Data/test.reg.predictions.RData")
}
