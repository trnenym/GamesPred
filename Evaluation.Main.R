#' Download and process data
#' Create train-validation-test splits and perform evaluation
#' Classification algorithms must be configured in Evaluation.Reg.R and Evaluation.Class.R
#'
#' @param update whether data should be updated/downloaded
#' @param split how dataset should be split into train, validation, and test set
#' @param intervals players are split into classes which are then used for classification and are also balanced
#' @param seed random generator seed
#' @param top.terms how many top terms (based on information gain) from a description's document-term matrix should be selected; all if top.terms = 0
#' @param pca.ncomp if > 0, top.terms will be tranformed to this number of principal components
Evaluate <- function(update = TRUE, split = c(0.60, 0.20, 0.20), intervals = c(2,10,50,250,1000), mode = "reg", seed = 61, top.terms = 50, pca.ncomp = 0) {
  # Load/install required packages and load required functions
  source("./Init.R")
  Init(TRUE)

  if(update) {
    # # Download raw data into individual files
    # source("./DataDownload/DataDownload.R")
    # DataDownload()

    # Create a table
    source("./DataProcess/DataProcess01.All.R")
    DataProcess01.Raw()

    # Infer new attributes
    source("./DataProcess/DataProcess02.R")
    DataProcess02(evaluation = TRUE)

    # Process images
    source("./DataProcess/DataProcess.Screenshots.R")
    DataProcess.Screenshots()

    # Dataset is split into training, validation, and test set and attributes dependent on this split are added
    source("./DataProcess/DataProcess03.Split.R")
    DataProcess03.Split(split = split, evaluation = TRUE, min.devpub = 2, seed = seed
                        , top.terms = top.terms, pca.ncomp = pca.ncomp)
  }

  if(mode == "reg") {
    # Perform validation and testing
    source("./Evaluation/Evaluation.Reg.R")
    Evaluation.Reg(validation = TRUE, seed = seed, top.attr = 0, remove.nonsig = TRUE)
    Evaluation.Reg(validation = FALSE, seed = seed, top.attr = 0, remove.nonsig = TRUE)
  } else {
    if(mode == "class") {
      # Perform validation and testing
      source("./Evaluation/Evaluation.Class.R")
      Evaluation.Class(intervals = intervals, validation = TRUE, seed = seed)
      Evaluation.Class(intervals = intervals, validation = FALSE, seed = seed)
    } else {
      stop(paste('Unrecognized mode "', mode, '"', sep = ""))
    }
  }
}
