# Load/install required packages and load required functions

load.package <- function(package) {
  if(!suppressMessages(require(package, character.only = TRUE))) {
    suppressMessages(install.packages(package, repos='http://cran.rstudio.com/'))
    suppressMessages(require(package, character.only = TRUE))
  }
}

# evaluation: whether evaluating on validation/test data or predicting a single game
Init <- function(evaluation = FALSE) {
  source("./DataProcess/lib/DataProcess.lib.R")
  
  packages.essential <- c("shiny", "ineq", "utils", "NLP", "tm", "grDevices", "jpeg", "graphics", "iterators", "itertools", "foreach"
                          , "randomForest", "missForest", "e1071")
  
  lapply(packages.essential, load.package)
  
  if(evaluation) {
    packages.eval <- c("jsonlite", "curl", "bitops", "RCurl", "XML", "lubridate", "stats", "FSelector", "RWeka"
                       , "ggplot2", "caret", "neuralnet", "rpart", "tree", "Metrics")
    
    lapply(packages.eval, load.package)
  }
}


# Shinyapps "hack" to make it install required packages
if(FALSE) {
  library(ineq)
  library(utils)
  library(tm)
  library(grDevices)
  library(jpeg)
  library(graphics)
  library(missForest)
  library(e1071)
}
