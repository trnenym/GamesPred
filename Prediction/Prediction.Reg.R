# Predict average players for a new game

Prediction.Reg <- function(dataset.new, new.game.data) {
  cat("\n")
  cat("Making prediction\n")
  set.seed(61)

  results <- data.frame(actual = numeric(0), predicted = numeric(0))

  dataset.train.name <- paste("./DataProcess/Datasets/dataset.train",".RData",sep = "")
  load(dataset.train.name)
  dataset.test <- dataset.new

  actual <- dataset.test$Players

  if(file.exists("./Prediction/model.RData") && file.exists("./Prediction/attrs.RData")) {
    load("./Prediction/model.RData")
    load("./Prediction/attrs.RData")
  } else {
    stop("You need to run evaluation first to train a model!")
  }

  # names <- colnames(dataset.train)
  # formula <- as.formula(paste("Players ~", paste(names[!names %in% "Players"], collapse = " + ")))
  # model <- neuralnet(formula, data = dataset.train, hidden = c(16,8), threshold = 0.3)

  dataset.test <- subset(dataset.test, select = attrs)
  dataset.test <- dataset.test[,-c(ncol(dataset.test))]

  predictions <- predict(model, newdata = dataset.test)

  # predictions <- compute(model, dataset.test)
  # predictions <- predictions$net.result[,1]

  predictions[predictions<0] <- 0

  prediction.low <- predictions - 2
  if(prediction.low < 0) {
    prediction.low <- 0
  }
  prediction.high <- predictions + 2

  prediction.low <- round(2**prediction.low-1, 0)
  prediction.high <- round(2**prediction.high-1, 0)

  # Present similar games based on predictions
  load("./DataProcess/Data/test.reg.predictions.RData")
  load("./DataProcess/Datasets/dataset.RData")

  dataset.test.predictions$Diff <- abs(predictions - dataset.test.predictions$PredictedPlayers)
  dataset.test.predictions$ActualPlayers <- round(2**dataset.test.predictions$ActualPlayers-1, 0)
  dataset.test.predictions$PredictedPlayers <- paste(round(2**(pmax(dataset.test.predictions$PredictedPlayers-2, 0))-1, 0), " - "
                                                     , round(2**(dataset.test.predictions$PredictedPlayers+2)-1, 0), sep = "")

  dataset.test.predictions.ordered <- dataset.test.predictions[order(dataset.test.predictions$Diff),]
  dataset.test.predictions.ordered <- cbind(Name = NA, dataset.test.predictions.ordered[c(1:10), c(1:3)])

  names <- sapply(dataset.test.predictions.ordered$ID, function(x) dataset$Name[which(x == dataset$ID)])
  dataset.test.predictions.ordered$Name <- names
  dataset.test.predictions.ordered$Name <- paste0("<a href='","http://store.steampowered.com/app/", dataset.test.predictions.ordered$ID
                                                  ,"' target='_blank'>",dataset.test.predictions.ordered$Name,"</a>")

  for(i in 1:nrow(dataset.test.predictions.ordered)) {
    pair <- rbind(new.game.data, dataset[which(dataset.test.predictions.ordered$ID[i] == dataset$ID),])
    pair <- pair[,-ncol(pair)]
    columns.same <- sapply(seq_len(ncol(pair)), function(x) {
      if(length(unique(pair[,x])) == 1 && !is.logical(pair[,x])) return(attr.to.string(names(pair[x])))
      if(length(unique(pair[,x])) == 1 && is.logical(pair[,x]) && pair[1,x]) return(attr.to.string(names(pair[x])))
      return(NA)
    })

    dataset.test.predictions.ordered$CommonAttributes[i] <- paste0(na.omit(columns.same), collapse = ", ")
  }

  dataset.test.predictions.ordered <- dataset.test.predictions.ordered[,-which(colnames(dataset.test.predictions.ordered) == "ID")]

  colnames(dataset.test.predictions.ordered) <- c("Name", "Actual number of players", "Prediction", "Features that these games share with your game")

  final.result <- list(message = paste0("Predicted average number of concurrent players in the first 2 months after release: <b>"
                                        , prediction.low, " - ", prediction.high, "</b> (confidence 82 %)"
                                        , "<br/><br/> Games which got a similar prediction in the past: <br/>", collapse = "")
                       , similar = dataset.test.predictions.ordered)

  return(final.result)
}
