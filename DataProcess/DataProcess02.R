#' Adjustments and inferring attributes not effected by train-test split
#' 
#' @param evaluation whether evaluating on validation/test data or predicting a single game
#' @param game.predicted new game data passed from DataProcess01.New.R
DataProcess02 <- function(evaluation = FALSE, game.predicted = NULL) {
  cat("\n")
  cat("Inferring new attributes: \n")
  source("./DataProcess/lib/DataProcess.Usertags.lib.R")
  source("./DataProcess/lib/DataProcess.DevPub.lib.R")
  load("./DataProcess/Datasets/dataset.all.RData")
  
  dataset.all <- dataset.all[,-which(colnames(dataset.all) == "EarlyAccessNow")]
  dataset.all <- dataset.all[,-which(colnames(dataset.all) == "EarlyAccessPast")]
  
  # Default values for HW minimum requirements (when missing)
  hw.cpu.value.default <- 1024
  hw.gpu.score.default <- 143
  hw.ram.value.default <- 1024
  hw.hdd.value.default <- 200
  hw.dx.version.default <- 9.0
  
  if(evaluation) {
    # Only applicable records are further processed
    dataset <- dataset.all[dataset.all$Relevant,][,-2]
    dataset.all <- dataset.all[,-which(colnames(dataset.all) == "Relevant")]
  } else {
    dataset.all <- dataset.all[,-which(colnames(dataset.all) == "Relevant")]
    dataset <- data.frame(game.predicted)
    dataset.all <- rbind(dataset.all, game.predicted)
  }
  
  # Months are counted since a specific time point to present a games's age as one attribute
  dataset$MonthsSinceBeginning <- (dataset$Year - 2012) * 12 + dataset$Month
  
  # Sequels
  cat("Detecting sequels...\n")
  sequel.tags <- c("sequel", "series", "franchise", "continuation")
  intersects.sequel <- is.token.intersect(dataset$Description, sequel.tags, ignore.case = TRUE)
  
  dataset$IsSequel <- intersects.sequel
  
  dataset$IsSequel <- dataset$IsSequel || is.sequel(dataset$Name)
  
  # Customization options
  cat("Detecting customization features...\n")
  custom.tags <- c("custom", "customize", "customizable")
  intersects.custom <- is.token.intersect(dataset$Description, custom.tags, ignore.case = TRUE)
  
  dataset$IsCustomizable <- as.logical(intersects.custom)
  
  # Each game has a record of games previously released by its developer/publisher 
  cat("Processing developers and publishers...\n")
  devpub <- get.devpub(dataset, dataset.all)
  dataset <- cbind(dataset, devpub[,-1])
  
  # User tags as separate attributes
  usertags.names <- get.usertags.names(prefix = "Tag")
  usertags.table <- data.frame(matrix(FALSE, ncol = length(usertags.names), nrow = nrow(dataset)))
  colnames(usertags.table) <- usertags.names
  dataset <- cbind(dataset, usertags.table)
  
  age.requirements <- dataset$AgeRequirements
  dataset$AgeRequirements <- 0
  
  for (i in 1:nrow(dataset)) {
    # Using default values for missing HW info
    if(is.na(dataset$HWCPU[i])) {
      dataset$HWCPU[i] <- hw.cpu.value.default
    }
    if(is.na(dataset$HWGPU[i])) {
      dataset$HWGPU[i] <- hw.gpu.score.default
    }
    if(is.na(dataset$HWRAM[i])) {
      dataset$HWRAM[i] <- hw.ram.value.default
    }
    if(is.na(dataset$HWHDD[i])) {
      dataset$HWHDD[i] <- hw.hdd.value.default
    }
    if(is.na(dataset$HWDx[i])) {
      dataset$HWDx[i] <- hw.dx.version.default
    }
    
    # Age requirements as actual age
    if(age.requirements[i] == "e") {
      dataset$AgeRequirements[i] <- 0
    }
    if(age.requirements[i] == "rp") {
      dataset$AgeRequirements[i] <- 0
    }
    if(age.requirements[i] == "ec") {
      dataset$AgeRequirements[i] <- 3
    }
    if(age.requirements[i] == "t") {
      dataset$AgeRequirements[i] <- 13
    }
    if(age.requirements[i] == "m") {
      dataset$AgeRequirements[i] <- 17
    }
    if(age.requirements[i] == "ao") {
      dataset$AgeRequirements[i] <- 18
    }
    
    # Weekday
    date.str <- paste(dataset$Year[i],dataset$Month[i],dataset$Day[i], sep = "-")
    weekday <- weekdays(as.Date(date.str, "%Y-%m-%d"))
    dataset$Weekday[i] <- match(weekday, weekdays.names)
    
    # Languages
    languages <- unlist(strsplit(as.character(dataset$Languages[i]), split = ";"))
    dataset$English[i] <- "English" %in% languages
    dataset$French[i] <- "French" %in% languages
    dataset$German[i] <- "German" %in% languages
    dataset$Italian[i] <- "Italian" %in% languages
    dataset$Japanese[i] <- "Japanese" %in% languages
    dataset$Polish[i] <- "Polish" %in% languages
    dataset$Portuguese[i] <- "Portuguese" %in% languages
    dataset$PortugueseBrazil[i] <- "Portuguese-Brazil" %in% languages
    dataset$Russian[i] <- "Russian" %in% languages
    dataset$Spanish[i] <- "Spanish" %in% languages
    
    dataset$LanguagesNum[i] <- length(languages)
    
    # Genres
    genres <- unlist(strsplit(as.character(dataset$Genres[i]), split = ";"))
    dataset$RPG[i] <- "RPG" %in% genres
    dataset$Strategy[i] <- "Strategy" %in% genres
    dataset$Adventure[i] <- "Adventure" %in% genres
    dataset$Action[i] <- "Action" %in% genres
    dataset$Simulation[i] <- "Simulation" %in% genres
    dataset$Racing[i] <- "Racing" %in% genres
    dataset$Casual[i] <- "Casual" %in% genres
    dataset$Sports[i] <- "Sports" %in% genres
    dataset$MassivelyMultiplayer[i] <- "Massively Multiplayer" %in% genres
    dataset$Education[i] <- "Education" %in% genres
    dataset$Indie[i] <- "Indie" %in% genres
    
    # Description length
    dataset$ShortDescriptionLength[i] <- nchar(as.character(dataset$ShortDescription[i]))
    dataset$DescriptionLength[i] <- nchar(as.character(dataset$Description[i]))
    
    # Name length
    dataset$NameLength[i] <- nchar(as.character(dataset$Name[i]))
    
    # User tags as separate attributes
    usertags <- get.usertags(dataset$UserTags[i])
    dataset[i,c(which(colnames(dataset) == "Tag2D"):which(colnames(dataset) == "TagZombies"))] <- usertags
  }
  
  cat("Finishing...\n")
  
  # Age requirements as numeric
  dataset$AgeRequirements <- as.numeric(as.character(dataset$AgeRequirements))
  
  # Remove redundant attributes
  dataset <- dataset[,-which(colnames(dataset) == "UserTags")]
  dataset <- dataset[,-which(colnames(dataset) == "Languages")]
  dataset <- dataset[,-which(colnames(dataset) == "Genres")]
  
  # Move class attribute to the end
  players <- dataset$Players
  dataset <- dataset[,-which(colnames(dataset) == "Players")]
  dataset$Players <- players
  
  if(evaluation) {
    save(dataset, file = "./DataProcess/Datasets/dataset.RData")
    return(NULL)
  } else {
    new.game.processed <- dataset
    return(new.game.processed)
  }
  
  cat("Done!\n\n")
}
