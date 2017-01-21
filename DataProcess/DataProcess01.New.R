#' Process input from app.R so it follows the same format as the rest of the data
#'
#' @param data.raw new game data passed from app.R
DataProcess01.New <- function(data.raw) {
  cat("Preparing new game...\n")
  
  game.predicted <- list()
  message <- NULL
  
  game.predicted$ID <- 9999999
  
  game.predicted$Name <- data.raw$Name
  
  if(is.null(message) && nchar(data.raw$Developer) < 1) {
    message <- "Please provide a developer"
  }
  game.predicted$Developer <- data.raw$Developer
  
  if(is.null(message) && nchar(data.raw$Publisher) < 1) {
    message <- "Please provide a publisher"
  }
  game.predicted$Publisher <- data.raw$Publisher
  
  load("./DataProcess/Data/cache.RData")
  
  is.dev.exp <- game.predicted$Developer %in% cache$devs.exp
  is.pub.exp <- game.predicted$Publisher %in% cache$pubs.exp
  
  game.predicted$AgeRequirements <- data.raw$AgeRequirements
  
  tryCatch({
    if(is.null(message) 
       && (is.na(as.Date(as.character(data.raw$ReleaseDate),format="%Y-%m-%d")) || length(as.Date(as.character(data.raw$ReleaseDate),format="%Y-%m-%d")) < 1)) {
      message <- "Please provide a valid date"
    }
  },
  error=function(cond) {
    message <- "Please provide a valid date"
  },
  warning=function(cond) {
    message <- "Please provide a valid date"
  })
  
  game.predicted$Year <- as.numeric(format(data.raw$ReleaseDate,'%Y'))
  game.predicted$Month <- as.numeric(format(data.raw$ReleaseDate,'%m'))
  game.predicted$Day <- as.numeric(format(data.raw$ReleaseDate,'%d'))
  game.predicted$Price <- round(data.raw$Price, 0)
  
  game.predicted$ShortDescription <- cleanHTML(data.raw$ShortDescription)
  
  game.predicted$Description <- cleanHTML(data.raw$Description)
  
  platforms <- c("Windows", "Mac", "Linux")
  game.predicted$Windows <- "Windows" %in%  data.raw$Platforms
  game.predicted$Mac <- "Mac" %in%  data.raw$Platforms
  game.predicted$Linux <- "Linux" %in%  data.raw$Platforms
  
  game.predicted$SinglePlayer <- "sp" %in% data.raw$Features
  game.predicted$MultiPlayer <- "mp" %in% data.raw$Features
  game.predicted$Coop <- "co" %in% data.raw$Features
  game.predicted$LocalCoop <- "lco" %in% data.raw$Features
  game.predicted$SteamAchievements <- "ach" %in% data.raw$Features
  game.predicted$SteamTradingCards <- "cards" %in% data.raw$Features
  game.predicted$SteamWorkshop <- "wshop" %in% data.raw$Features
  game.predicted$VRSupport <- "vr" %in% data.raw$Features
  game.predicted$ControllerSupport <- "con" %in% data.raw$Features
  
  game.predicted$HWCPU <- data.raw$HWCPU
  
  hw.gpu.score <- 143
  load("./DataProcess/Data/gpu.chart.RData")
  hw.gpu.string <- data.raw$HWGPU
  hw.gpu.matches <- lapply(hw.gpu.table$Name, function(x) grep(x, hw.gpu.string, ignore.case = TRUE))
  hw.gpu.matches <- hw.gpu.table[which(hw.gpu.matches > 0),]
  if(nrow(hw.gpu.matches) > 0) {
    hw.gpu.score <- hw.gpu.matches$Score[1]
  }
  game.predicted$HWGPU <- hw.gpu.score
  
  game.predicted$HWRAM <- data.raw$HWRAM
  game.predicted$HWHDD <- data.raw$HWHDD
  game.predicted$HWDx <- data.raw$HWDx
  
  game.predicted$Languages <- gsub(' *, *', ';', data.raw$Languages)
  
  game.predicted$Genres <- paste(data.raw$Genres, collapse = ";")
  
  game.predicted$Screenshots <- data.raw$Screenshots
  game.predicted$Trailers <- data.raw$Trailers
  
  game.predicted$UserTags <- gsub(' *, *', ';', data.raw$UserTags)
  
  game.predicted$DRMNotice <- "DRMNotice" %in% data.raw$DRM
  game.predicted$DRMEula <- "DRMEula" %in% data.raw$DRM
  game.predicted$DRMAccount <- "DRMAccount" %in% data.raw$DRM
  game.predicted$DRMSecuRom <- FALSE
  
  game.predicted$Players <- 0
  
  if(is.null(data.raw$Thumbnail) || nchar(data.raw$Thumbnail) < 5) {
    thumbnail = NULL
  } else {
    thumbnail = data.raw$Thumbnail
  }
  return(list(new.game.processed = game.predicted, is.dev.exp = is.dev.exp, is.pub.exp = is.pub.exp, thumbnail = thumbnail, message = message))
}