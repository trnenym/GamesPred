#' Make a prediction for a new game
#' Input is passed from app.R as a list of features
#'
#' @param new.game new game data passed from app.R
Predict.game <- function(new.game) {
  # Attempt to process data from app.R
  source("./DataProcess/DataProcess01.New.R")
  new.game.info <- DataProcess01.New(new.game)

  # Check whether the game can be further processed
  if(!is.null(new.game.info$message)) {
    return(list(message = new.game.info$message))
  } else {
    if(!(new.game.info$is.dev.exp || new.game.info$is.pub.exp)) {
      return(list(message = "unknown"))
    } else {
      # Infer new attributes
      source("./DataProcess/DataProcess02.R")
      new.game.data <- DataProcess02(game.predicted = new.game.info$new.game.processed)

      # Use previously created train set to further infer new attributes
      source("./DataProcess/DataProcess03.Split.R")
      new.game.processed <- DataProcess03.Split(new.game = new.game.data, thumbnail = new.game.info$thumbnail)

      # Make a prediction
      source("./Prediction/Prediction.Reg.R")
      prediction <-  Prediction.Reg(new.game.processed, new.game.data)

      return(prediction)
    }
  }
}
