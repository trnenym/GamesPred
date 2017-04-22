# Functions for processing JPG images

# Creates an HSV vector if the image is in greyscale and optionally reduces each of the 3 channels to the specified number of bins
# (the resulting number of colors is then bins^3)
color.transform <- function(x, reduce = FALSE, bins = 4) {
  bins = bins-1
  if(length(x[[1]]) == 1) {
    x <- c(x,x,x)
  }
  if(reduce) {
    r <- round(x[1] * bins) / bins
    g <- round(x[2] * bins) / bins
    b <- round(x[3] * bins) / bins
    return(rgb(r,g,b))
  } else {
    return(rgb(x[1],x[2],x[3]))
  }
}

# Returns a vector of hue values (divided into 16 bins), and the average saturation and value of an image
get.colors <- function(screenshot.path) {
  save.orig <- "./DataProcess/Data/processed.jpg"
  if(grepl('^http', screenshot.path, perl = TRUE) || grepl('^www', screenshot.path, perl = TRUE)) {
    download.file(screenshot.path, save.orig, method = "internal", mode = "wb", quiet = TRUE)
    screenshot.path <- save.orig
  }
  screenshot.raw <- readJPEG(screenshot.path)

  screenshot.data <- list()
  bw <- FALSE

  if(length(dim(screenshot.raw)) == 2) {
    bw <- TRUE
    tmpImage <- array(NA, dim = c(nrow(screenshot.raw), ncol(screenshot.raw), 3))
    for (i in 1:nrow(screenshot.raw)) {
      for (j in 1:ncol(screenshot.raw)) {
        tmpImage[i,j,] <- c(screenshot.raw[i,j], screenshot.raw[i,j], screenshot.raw[i,j])
      }
    }
    screenshot.raw <- tmpImage
  }

  screenshot.fullcol <- apply(screenshot.raw, c(1,2), function(x) color.transform(x))
  colors.full <- length(unique(c(screenshot.fullcol)))
  screenshot.data <- c(screenshot.data, ColorsFull = colors.full)

  screenshot.reduced <- apply(screenshot.raw, c(1,2), function(x) color.transform(x, TRUE))
  screenshot.reduced.table <- as.data.frame(table(screenshot.reduced))
  screenshot.reduced.table <- screenshot.reduced.table[order(screenshot.reduced.table$Freq, decreasing = TRUE),]
  screenshot.data <- c(screenshot.data, ColorsReduced = nrow(screenshot.reduced.table))

  screenshot.hue <- apply(screenshot.raw, c(1,2), function(x) rgb2hsv(x[1], x[2], x[3])[1])
  hist.hue <- hist(screenshot.hue, breaks = seq(0,1,1/16), plot = FALSE)
  hue.vector <- round((hist.hue$counts / (dim(screenshot.hue)[1] * dim(screenshot.hue)[2])) * 100, 1)

  screenshot.sat <- apply(screenshot.raw, c(1,2), function(x) rgb2hsv(x[1], x[2], x[3])[2])

  screenshot.val <- apply(screenshot.raw, c(1,2), function(x) rgb2hsv(x[1], x[2], x[3])[3])

  screenshot.data <- c(screenshot.data, hue.vector, mean(screenshot.sat), mean(screenshot.val))

  unlink(save.orig)

  return(screenshot.data)
}
