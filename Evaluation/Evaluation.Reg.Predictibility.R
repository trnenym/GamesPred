# setwd("D:/Dropbox/MUNI/4. semestr/Steam")

# Evaluation.Reg <- function() {
  suppressMessages(library(randomForest))
  # library(RWeka)
  suppressMessages(library(caret))
  library(Metrics)
  library(e1071)
  library(neuralnet)
  set.seed(61)
  
  
  
  results1 <- data.frame(actual = numeric(0), predicted = numeric(0))
  
  dataset.train.name <- paste("./DataProcess/Datasets/dataset.train.reg",".RData",sep = "")
  dataset.val.name <- paste("./DataProcess/Datasets/dataset.val.reg",".RData",sep = "")
  dataset.test.name <- paste("./DataProcess/Datasets/dataset.test.reg",".RData",sep = "")
  
  load(dataset.train.name)
  load(dataset.val.name)
  load(dataset.test.name)
  
  # score <- chi.squared(Players~., dataset.train)
  # threshold <- 0.0
  # 
  # dataset.train <- dataset.train[,score$attr_importance >= threshold]
  # dataset.val <- dataset.val[,score$attr_importance >= threshold]
  # dataset.test <- dataset.test[,score$attr_importance >= threshold]
  
  # dataset.test <- dataset.val
  
  dataset.train1 <- dataset.train
  # actual1 <- dataset.train1$Players
  actual1 <- dataset.val$Players
  
  # model <- randomForest(Players ~ ., data = dataset.train, ntree = 200)
  model1 <- svm(Players ~ ., data = dataset.train1, kernel = "polynomial", coef0 = 0.5, gamma = 0.003)
  # save(model, file = "./Prediction/model.RData")
  # model <- svm(Players ~ ., data = dataset.train, kernel = "radial", gamma = 0.002)
  # model <- svm(Players ~ ., data = dataset.train, kernel = "sigmoid", coef0 = 0.01, gamma = 0.005)
  
  # names <- colnames(dataset.train)
  # formula <- as.formula(paste("Players ~", paste(names[!names %in% "Players"], collapse = " + ")))
  # model <- neuralnet(formula, data = dataset.train, hidden = c(16,8), threshold = 0.3)
  
  # dataset.test1 <- dataset.train[,-c(ncol(dataset.train))]
  dataset.test1 <- dataset.val[,-c(ncol(dataset.val))]
  
  predictions1 <- predict(model1, newdata = dataset.test1)
  
  # predictions <- compute(model, dataset.test)
  # predictions <- predictions$net.result[,1]
  
  results1 <- rbind(results1, data.frame(actual1, predictions1))
  
  cat("Results: \n\n")
  
  # cat("+-1 from actual:", nrow(subset(results, actual <= predictions + 1 & actual >= predictions - 1))/nrow(results))
  # cat(" with baseline:", nrow(subset(results, actual <= 2+min(actual)))/nrow(results), "\n")
  # 
  # cat("+-2 from actual:", nrow(subset(results, actual <= predictions + 2 & actual >= predictions - 2))/nrow(results))
  # cat(" with baseline:", nrow(subset(results, actual <= 4+min(actual)))/nrow(results), "\n")
  # 
  # cat("+-3 from actual:", nrow(subset(results, actual <= predictions + 3 & actual >= predictions - 3))/nrow(results))
  # cat(" with baseline:", nrow(subset(results, actual <= 6+min(actual)))/nrow(results), "\n\n")
  
  # cat("MAE:", mae(results1$actual, results1$predictions), "\n")
  # cat("RMSE:", rmse(results1$actual, results1$predictions), "\n")
  # cat("NRMSE:", rmse(results1$actual, results1$predictions) / sd(results1$actual), "\n")
  # cat("COR:", cor(results1$actual, results1$predictions, method = "pearson"), "\n")
  # 
  # # results.ordered <- results[order(results$actual),]
  # # table.actual <- table(results$actual)
  # # 
  # old.par <- par(no.readonly=T)
  # par(mar=c(5, 5, 4, 2) + 0.1)
  # 
  # plot(results1$predictions, results1$actual, xlim = c(0,16), ylim = c(0,16), col = "red", las = 1, main = "Regression (SVM, polynomial kernel)", xlab = "predicted"
  #      , ylab = "actual", cex.lab=1.5, cex.main=1.7, cex.axis=1.5)
  # abline(0,1)
  # abline(2,1, col = "gray")
  # abline(-2,1, col = "gray")
  # 
  # par(old.par)
  
  difs1 <- abs(predictions1 - actual1)
  
  # dataset.train2 <- dataset.train
  dataset.train2 <- dataset.val
  
  # dataset.train2$Players <- 0
  # dataset.train2$Players[difs1 > 1] <- 1
  # dataset.train2$Players <- factor(dataset.train2$Players)
  dataset.train2$Players <- difs1
  
  # indices.repeated <- c()
  # for (i in 1:nrow(dataset.train2)) {
  #   pl <- ceiling(dataset.train2$Players[i])
  #   if(pl > 1) {
  #     pl <- pl * 5
  #   } else {
  #     pl <- 1
  #   }
  #   # pl <- pl*10
  #   indices.repeated <- c(indices.repeated, rep(i,pl))
  # }
  # dataset.train2 <- dataset.train2[indices.repeated,]
  
  dataset.test2 <- dataset.test
  actual2 <- dataset.test2$Players
  
  dataset.train2 <- dataset.train2[,-which(colnames(dataset.train2) == "DescTFallnew")]
  
  model2 <- svm(Players ~ ., data = dataset.train2, kernel = "polynomial", coef0 = 0.5, gamma = 0.003)
  
  dataset.test2 <- dataset.test2[,-c(ncol(dataset.test2))]
  dataset.test2 <- dataset.test2[,-which(colnames(dataset.test2) == "DescTFallnew")]
  
  predictions2 <- predict(model2, newdata = dataset.test2)
  
  
  
  actual <- dataset.test$Players
  
  # model <- randomForest(Players ~ ., data = dataset.train, ntree = 200)
  model <- svm(Players ~ ., data = dataset.train, kernel = "polynomial", coef0 = 0.5, gamma = 0.003)
  # model <- svm(Players ~ ., data = dataset.train, kernel = "radial", gamma = 0.002)
  # model <- svm(Players ~ ., data = dataset.train, kernel = "sigmoid", coef0 = 0.01, gamma = 0.005)
  
  # names <- colnames(dataset.train)
  # formula <- as.formula(paste("Players ~", paste(names[!names %in% "Players"], collapse = " + ")))
  # model <- neuralnet(formula, data = dataset.train, hidden = c(16,8), threshold = 0.3)
  
  dataset.test <- dataset.test[,-c(ncol(dataset.test))]
  
  predictions <- predict(model, newdata = dataset.test)
  
  difs2 <- abs(predictions - actual)
  # difs2.factor <- 0
  # difs2.factor[difs2 > 1] <- 1
  # difs2.factor <- factor(difs2.factor, levels = levels(dataset.train2$Players))
  difs2.factor <- difs2
  
  results2 <- data.frame(actual = numeric(0), predicted = numeric(0))
  results2 <- rbind(results2, data.frame(actual = difs2.factor, predicted = predictions2))
  
  cat("MAE:", mae(results2$actual, results2$predicted), "\n")
  cat("RMSE:", rmse(results2$actual, results2$predicted), "\n")
  cat("NRMSE:", rmse(results2$actual, results2$predicted) / sd(results2$actual), "\n")
  cat("COR:", cor(results2$actual, results2$predicted, method = "pearson"), "\n")
  
  results.ordered <- results2[order(results2$actual),]
  table.actual <- table(results2$actual)

  old.par <- par(no.readonly=T)
  par(mar=c(5, 5, 4, 2) + 0.1)

  plot(results2$predicted, results2$actual, xlim = c(0,16), ylim = c(0,16), col = "red", las = 1, main = "Regression (SVM, polynomial kernel)", xlab = "predicted"
       , ylab = "actual", cex.lab=1.5, cex.main=1.7, cex.axis=1.5)
  abline(0,1)
  abline(2,1, col = "gray")
  abline(-2,1, col = "gray")

  par(old.par)
  
  # confusionMatrix(results2$actual, results2$predicted)
  
  # }
