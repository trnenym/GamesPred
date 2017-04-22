#' Evaluation on continuous class attribute
#'
#' @param validation whether evaluating on validation or test set
#' @param seed random generator seed
Evaluation.Reg <- function(validation = FALSE, seed = 1, top.attr = 0, remove.nonsig = FALSE) {
  set.seed(seed)

  cat("\n")
  cat("Evaluating on continuous class attribute (",if(validation) "validation" else "test"," set)\n", sep = "")

  results <- data.frame(actual = numeric(0), predicted = numeric(0))

  dataset.train.name <- paste("./DataProcess/Datasets/dataset.train.reg",".RData",sep = "")
  dataset.val.name <- paste("./DataProcess/Datasets/dataset.val.reg",".RData",sep = "")
  dataset.test.name <- paste("./DataProcess/Datasets/dataset.test.reg",".RData",sep = "")

  load(dataset.train.name)
  load(dataset.val.name)
  load(dataset.test.name)

  if(validation) {
    dataset.test <- dataset.val
  }

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

  # dataset.train <- dataset.train[,-c(which(colnames(dataset.train) == "DevPrevGamesCount"):which(colnames(dataset.train) == "PubPrevGamesIneq"))]
  # dataset.train <- dataset.train[,-c(which(colnames(dataset.train) == "DevTop"):which(colnames(dataset.train) == "PubTop"))]

  # dataset.train <- dataset.train[,-c(which(colnames(dataset.train) == "SColorsFull"):which(colnames(dataset.train) == "SColorValAverage"))]
  # dataset.test <- dataset.test[,-c(which(colnames(dataset.test) == "SColorsFull"):which(colnames(dataset.test) == "SColorValAverage"))]

  # dataset.train <- dataset.train[,-c(which(colnames(dataset.train) == "TColorHue1"):which(colnames(dataset.train) == "TColorHue16"))]
  # dataset.test <- dataset.test[,-c(which(colnames(dataset.test) == "TColorHue1"):which(colnames(dataset.test) == "TColorHue16"))]
  # dataset.train <- dataset.train[,-c(which(colnames(dataset.train) == "SColorHue1"):which(colnames(dataset.train) == "SColorHue16"))]
  # dataset.test <- dataset.test[,-c(which(colnames(dataset.test) == "SColorHue1"):which(colnames(dataset.test) == "SColorHue16"))]

  # dataset.train <- dataset.train[,-c(which(colnames(dataset.train) == "SColorsFull"):which(colnames(dataset.train) == "SColorsReduced"))]
  # dataset.test <- dataset.test[,-c(which(colnames(dataset.test) == "SColorsFull"):which(colnames(dataset.test) == "SColorsReduced"))]
  # dataset.train <- dataset.train[,-c(which(colnames(dataset.train) == "SColorSatAverage"):which(colnames(dataset.train) == "SColorValAverage"))]
  # dataset.test <- dataset.test[,-c(which(colnames(dataset.test) == "SColorSatAverage"):which(colnames(dataset.test) == "SColorValAverage"))]
  # dataset.train <- dataset.train[,-c(which(colnames(dataset.train) == "TColorsFull"):which(colnames(dataset.train) == "TColorsReduced"))]
  # dataset.test <- dataset.test[,-c(which(colnames(dataset.test) == "TColorsFull"):which(colnames(dataset.test) == "TColorsReduced"))]
  # dataset.train <- dataset.train[,-c(which(colnames(dataset.train) == "TColorSatAverage"):which(colnames(dataset.train) == "TColorValAverage"))]
  # dataset.test <- dataset.test[,-c(which(colnames(dataset.test) == "TColorSatAverage"):which(colnames(dataset.test) == "TColorValAverage"))]

  actual <- dataset.test$Players

  model.label <- "SVM, polynomial kernel"
  # model <- randomForest(Players ~ ., data = dataset.train, ntree = 200)
  model <- svm(Players ~ ., data = dataset.train, kernel = "polynomial", coef0 = 0.5, gamma = 0.003)
  save(model, file = "./Prediction/model.RData")

  # names <- colnames(dataset.train)
  # formula <- as.formula(paste("Players ~", paste(names[!names %in% "Players"], collapse = " + ")))
  # model <- neuralnet(formula, data = dataset.train, hidden = c(32), threshold = 0.3)

  dataset.test <- dataset.test[,-c(ncol(dataset.test))]

  predictions <- predict(model, newdata = dataset.test)

  # predictions <- compute(model, dataset.test)
  # predictions <- predictions$net.result[,1]

  predictions[predictions<0] <- 0

  results <- rbind(results, data.frame(actual, predictions))

  cat("Results: \n\n")

  cat("+-1 from actual:", nrow(subset(results, actual <= predictions + 1 & actual >= predictions - 1))/nrow(results))
  cat(" with baseline:", nrow(subset(results, actual <= 2+min(actual)))/nrow(results), "\n")

  cat("+-2 from actual:", nrow(subset(results, actual <= predictions + 2 & actual >= predictions - 2))/nrow(results))
  cat(" with baseline:", nrow(subset(results, actual <= 4+min(actual)))/nrow(results), "\n")

  cat("+-3 from actual:", nrow(subset(results, actual <= predictions + 3 & actual >= predictions - 3))/nrow(results))
  cat(" with baseline:", nrow(subset(results, actual <= 6+min(actual)))/nrow(results), "\n\n")

  cat("MAE:", mae(results$actual, results$predictions), "\n")
  cat("RMSE:", rmse(results$actual, results$predictions), "\n")
  cat("NRMSE:", rmse(results$actual, results$predictions) / sd(results$actual), "\n")
  cat("COR:", cor(results$actual, results$predictions, method = "pearson"), "\n")

  results.ordered <- results[order(results$actual),]
  table.actual <- table(results$actual)

  old.par <- par(no.readonly=T)

  windows(width=8, height=8)
  par(mar=c(5, 5, 4, 2) + 0.1)

  plot(results$predictions, results$actual, xlim = c(0,16), ylim = c(0,16), col = "red", las = 1
       , main = paste0("Regression (", model.label, ")", "\n", if(validation) "validation" else "test", " set")
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
