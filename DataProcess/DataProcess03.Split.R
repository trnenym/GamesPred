#' Dataset is split into training, validation, and test set and attributes dependent on this split are added
#'
#' @param split how dataset should be split into train, validation, and test set
#' @param intervals players are split into classes which are then used for classification and are also balanced
#' @param evaluation whether evaluating on validation/test data or predicting a single game
#' @param new.game new game data in the same format as the rest of the dataset
#' @param thumbnail path or url to a .jpg
#' @param min.devpub only games by a developer/publisher with this many games on their account are processed
#' @param seed random generator seed
#' @param top.terms how many top terms (based on information gain) from a description's document-term matrix should be selected; all if top.terms = 0
#' @param pca.ncomp if > 0, top.terms will be tranformed to this number of principal components
DataProcess03.Split <- function(split = c(0.60, 0.20, 0.20), intervals = c(2,10,50,250,1000), mode = "reg", evaluation = FALSE
                                , new.game = NULL, thumbnail = NULL, min.devpub = 2, seed = 1, top.terms = 50, pca.ncomp = 0, train.cache = FALSE) {
  source("./DataProcess/DataProcess04.Final.R")

  if(evaluation && (length(split) != 3 || !all.equal(sum(split), 1))) {
    stop("Incorrect split coeficients")
  }

  cat("\n")
  cat("Preparing for final processing...", "\n")
  options(java.parameters = "-Xmx8g")
  set.seed(seed)

  load("./DataProcess/Datasets/dataset.RData")
  if(!evaluation) {
    dataset <- rbind(dataset, new.game)
  }

  # Data about images is added
  load("./DataProcess/Data/screenshots.colors.RData")
  if(!evaluation) {
    new.colors <- c(dataset$ID[nrow(dataset)])

    if(is.null(thumbnail)) {
      color.means <- colMeans(screenshots.colors)[-1]
      new.colors <- c(new.colors, color.means)
    } else {
      source("./DataProcess/lib/ScreenshotsProcess.lib.R")
      new.colors.thumbnail <- get.colors(thumbnail)
      new.colors <- c(new.colors, new.colors.thumbnail, new.colors.thumbnail)
    }

    screenshots.colors <- rbind(screenshots.colors, unname(new.colors))
  }

  players <- dataset$Players

  dataset <- dataset[,-ncol(dataset)]

  if(all(dataset$ID == screenshots.colors$ID)) {
    dataset <- cbind(dataset, screenshots.colors[,-1])
  } else {
    stop("IDs do not match")
  }

  dataset$Players <- players

  # Players is continuous, Class is nominal based on given splits
  dataset$Class <- NA

  for (i in 1:nrow(dataset)) {
    intervals.tmp <- c(intervals, dataset$Players[i])
    intervals.tmp <- sort(intervals.tmp)
    dataset$Class[i] <- match(dataset$Players[i], intervals.tmp)
  }

  dataset$Class <- as.factor(dataset$Class)

  # This is used for determining significant terms in descriptions
  intervals.helper <- c(quantile(dataset$Players, probs = (0.75)))
  dataset$ClassHelper <- NA

  for (i in 1:nrow(dataset)) {
    intervals.helper.tmp <- c(intervals.helper, dataset$Players[i])
    intervals.helper.tmp <- sort(intervals.helper.tmp)
    dataset$ClassHelper[i] <- match(dataset$Players[i], intervals.helper.tmp)
  }

  dataset$ClassHelper <- as.factor(dataset$ClassHelper)

  # log adjustment to players
  dataset$Players <- log(dataset$Players + 1, base = 2)
  dataset$Players <- round(dataset$Players, 2)

  # The way games are allowed on Steam changed at the end of August, 2013
  dataset <- subset(dataset, Year > 2013 | (Year == 2013 & Month > 8))

  # Split into train/validation/test sets
  if(!evaluation) {
    dataset.new <- dataset[nrow(dataset),]
    dataset <- dataset[-nrow(dataset),]
  }

  dataset.nrow <- nrow(dataset)

  dataset.train <- dataset[1:(round(split[1] * dataset.nrow)),]

  # Processing individual splits
  if(evaluation) {
    dataset.val <- dataset[(round(split[1] * dataset.nrow) + 1):(round((split[1] + split[2]) * dataset.nrow)),]
    dataset.test <- dataset[(round((1 - split[3]) * dataset.nrow) + 1):dataset.nrow,]

    cat("\n")
    cat("Processing train data:", "\n")
    process.final(dataset.train, type = "train", evaluation = evaluation, min.devpub, seed = seed, top.terms = top.terms
                  , pca.ncomp = pca.ncomp, train.cache)
    cat("\n")
    cat("Processing validation data:", "\n")
    process.final(dataset.val, type = "val", evaluation = evaluation, min.devpub, seed = seed)
    cat("\n")
    cat("Processing test data:", "\n")
    process.final(dataset.test, type = "test", evaluation = evaluation, min.devpub, seed = seed)
  } else {
    dataset.new.processed <- process.final(dataset.new, type = "test", evaluation = evaluation, min.devpub, seed = seed)
    return(dataset.new.processed)
  }
}
