#' Processes thumbnails and the first screenshot of games in dataset
DataProcess.Screenshots <- function() {
  source("./DataProcess/lib/ScreenshotsProcess.lib.R")

  cat("\n")
  cat("Processing images...\n")

  load("./DataProcess/Datasets/dataset.RData")

  # Images are stored in ./DataDownload/DataRaw
  steam.ids <- dataset$ID
  dirs.processed <- list.files("./DataDownload/DataRaw", recursive = TRUE, pattern = "screenshots\\.RData$")
  steam.ids.processed <- as.integer(sapply(dirs.processed, function(x) gsub('/.*$','',x)))
  steam.ids.toprocess <- setdiff(steam.ids, steam.ids.processed)

  # It is possible to interrupt and resume the computation
  id.current <- suppressWarnings(readLines(con = "./DataDownload/current.screenshot.ini"))
  if(length(id.current > 0)) {
    unlink(paste("./DataDownload/DataRaw/",id.current[[1]],"/screenshots.RData", sep = ""))
    steam.ids.toprocess <- c(id.current, steam.ids.toprocess)
  } else {
    cat("", file = "./DataDownload/current.screenshot.ini")
  }

  # The result is stored in this data frame
  screenshots.colors <- data.frame(ID = numeric(nrow(dataset))
                                   , TColorsFull = numeric(nrow(dataset)), TColorsReduced = numeric(nrow(dataset))
                                   , TColorHue1 = numeric(nrow(dataset)), TColorHue2 = numeric(nrow(dataset))
                                   , TColorHue3 = numeric(nrow(dataset)), TColorHue4 = numeric(nrow(dataset))
                                   , TColorHue5 = numeric(nrow(dataset)), TColorHue6 = numeric(nrow(dataset))
                                   , TColorHue7 = numeric(nrow(dataset)), TColorHue8 = numeric(nrow(dataset))
                                   , TColorHue9 = numeric(nrow(dataset)), TColorHue10 = numeric(nrow(dataset))
                                   , TColorHue11 = numeric(nrow(dataset)), TColorHue12 = numeric(nrow(dataset))
                                   , TColorHue13 = numeric(nrow(dataset)), TColorHue14 = numeric(nrow(dataset))
                                   , TColorHue15 = numeric(nrow(dataset)), TColorHue16 = numeric(nrow(dataset))
                                   , TColorSatAverage = numeric(nrow(dataset)), TColorValAverage = numeric(nrow(dataset))
                                   , SColorsFull = numeric(nrow(dataset)), SColorsReduced = numeric(nrow(dataset))
                                   , SColorHue1 = numeric(nrow(dataset)), SColorHue2 = numeric(nrow(dataset))
                                   , SColorHue3 = numeric(nrow(dataset)), SColorHue4 = numeric(nrow(dataset))
                                   , SColorHue5 = numeric(nrow(dataset)), SColorHue6 = numeric(nrow(dataset))
                                   , SColorHue7 = numeric(nrow(dataset)), SColorHue8 = numeric(nrow(dataset))
                                   , SColorHue9 = numeric(nrow(dataset)), SColorHue10 = numeric(nrow(dataset))
                                   , SColorHue11 = numeric(nrow(dataset)), SColorHue12 = numeric(nrow(dataset))
                                   , SColorHue13 = numeric(nrow(dataset)), SColorHue14 = numeric(nrow(dataset))
                                   , SColorHue15 = numeric(nrow(dataset)), SColorHue16 = numeric(nrow(dataset))
                                   , SColorSatAverage = numeric(nrow(dataset)), SColorValAverage = numeric(nrow(dataset))
  )

  time.diff <- c()

  # Process individual images and save the info in ./DataDownload/DataRaw/#ID#/screenshots.RData"
  if(length(steam.ids.toprocess) > 0) {
    for (i in 1:length(steam.ids.toprocess)) {
      time.start <- Sys.time()

      id <- steam.ids.toprocess[i]
      cat(id, file = "./DataDownload/current.screenshot.ini")

      if(i > 1) {
        time.estimated <- as.character(seconds_to_period(as.integer(mean(time.diff) * (length(steam.ids.toprocess) - i + 1))))
      } else {
        time.estimated <- "NA"
      }
      console.rewrite(c('Processing ', i, ' of ', length(steam.ids.toprocess), '. Estimated time remaining: ', time.estimated))

      screenshots.data <- list(ID = id)
      screenshots.path <- paste("DataDownload/DataRaw/",id, sep = "")
      screenshots <- list.files(screenshots.path, pattern = "\\.jpg$")

      thumbnail.path <- paste("DataDownload/DataRaw/",id,"/thumbnail.jpg", sep = "")

      if(file.exists(thumbnail.path)) {
        Tcolors.list <- get.colors(thumbnail.path)

        screenshots.data <- c(screenshots.data, Tcolors.list)
      } else {
        screenshots.data <- c(screenshots.data, rep(NA, 20))
      }

      if(length(screenshots) > 0) {
        screenshot.path <- paste("DataDownload/DataRaw/",id,"/","screenshot1.jpg", sep = "")

        Scolors.list <- get.colors(screenshot.path)

        screenshots.data <- c(screenshots.data, Scolors.list)
      } else {
        screenshots.data <- c(screenshots.data, rep(NA, 20))
      }

      save(screenshots.data, file = paste("./DataDownload/DataRaw/",id,"/screenshots.RData", sep = ""))

      cat("", file = "./DataDownload/current.screenshot.ini")
      time.end <- Sys.time()
      time.diff <- c(time.diff, as.integer(time.end - time.start))
    }
  }

  console.rewrite('Finishing...')

  # Compose the final table
  for (i in 1:nrow(dataset)) {
    id <- dataset$ID[i]
    load(paste("./DataDownload/DataRaw/",id,"/screenshots.RData", sep = ""))
    screenshots.colors[i,] <- screenshots.data
  }

  save(screenshots.colors, file = "./DataProcess/Data/screenshots.colors.RData")

  console.rewrite('Done!')
  cat("\n")
}
