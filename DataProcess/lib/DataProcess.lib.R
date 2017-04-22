# Functions used in various scripts

weekdays.names <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

# Add or subtract offset months from a year and a month
month.add <- function(month, year, offset) {
  new.year <- year
  if((month + offset) > 12 | (month + offset) < 1) {
    new.year <- new.year + ((month + offset) %/% 12)
    if((month + offset) %% 12 == 0) {
      new.year <- new.year - 1
    }
  }
  new.month <- (month + offset) %% 12
  if(new.month == 0) {
    new.month <- 12
  }
  return(c(new.month, new.year))
}

# Check connection to google.com
connection.check <- function(wait = FALSE) {
  if(wait) {
    while(!(url.exists("216.58.209.206"))) {
      Sys.sleep(5)
    }
    return(TRUE)
  } else {
    return(url.exists("216.58.209.206"))
  }
}

# Replace the last line
console.rewrite <- function(output) {
  cat('\r', rep(' ', 128, sep = ''))
  cat('\r', output, sep = '')
  flush.console()
}

# Remove HTML tags from a string
cleanHTML <- function(htmlString) {
  htmlString <- gsub("\\s+", " ", htmlString)
  return(gsub("<[^>]*?>", "", htmlString))
}

# Returns a 2-month average of concurrent players from a table containing records of year, month, and avg for a game + release date
steam.charts <- function(sc.table, year, month, day) {
  reduced <- FALSE
  days.in.month <- as.numeric(days_in_month(as.Date(paste(year, month, day, sep = "-"), "%Y-%m-%d")))

  if(nrow(sc.table) < 2) {
    return(NA)
  }

  release.index <- which(sc.table$year == year & sc.table$month == month, arr.ind = FALSE)
  day.adj <- day

  if(length(release.index) > 0 && nrow(sc.table[1:release.index,]) < 3) {
    return(NA)
  }

  if(length(release.index) == 0 && day >= days.in.month - 3) {
    if((sc.table[nrow(sc.table),1] == year) && (sc.table[nrow(sc.table),2] == month + 1)) {
      release.index <- nrow(sc.table)
      day.adj <- 1
      reduced <- TRUE
    }

    if((sc.table[nrow(sc.table),1] == year + 1) && (sc.table[nrow(sc.table),2] == 1) && (month == 12)) {
      release.index <- nrow(sc.table)
      day.adj <- 1
      reduced <- TRUE
    }
  }

  if(length(release.index) == 0) {
    return(NA)
  }

  if(nrow(sc.table) > release.index && nrow(sc.table[1:release.index,]) < 3) {
    return(NA)
  }

  if(nrow(sc.table) > release.index) {
    day.adj <- 1
  }

  players <- NA

  if(reduced) {
    sc.table.selection <- sc.table[(release.index-1):release.index,]

    weight1 <- as.numeric(days_in_month(ymd(paste(year, month, day, sep = "-")) %m+% months(1)))
    weight2 <- as.numeric(days_in_month(ymd(paste(year, month, day, sep = "-")) %m+% months(2)))

    players <- (sc.table.selection[2,3]*weight1 + sc.table.selection[1,3]*weight2) / (weight1+weight2)
  } else {
    sc.table.selection <- sc.table[(release.index-2):release.index,]

    weight1 <- as.numeric(days_in_month(ymd(paste(year, month, day, sep = "-")))) + 1 - day.adj
    weight2 <- as.numeric(days_in_month(ymd(paste(year, month, day, sep = "-")) %m+% months(1)))
    weight3 <- min(day, as.numeric(days_in_month(ymd(paste(year, month, day, sep = "-")) %m+% months(2))))

    players <- (sc.table.selection[3,3]*weight1 + sc.table.selection[2,3]*weight2 + sc.table.selection[1,3]*weight3) / (weight1+weight2+weight3)
  }

  return(players)
}

# Creates a table of GPUs and their benchmark scores
gpu.chart <- function() {
  gpu.table.high <- readHTMLTable("http://www.videocardbenchmark.net/high_end_gpus.html")[[4]][,c(1,2)]
  gpu.table.mid <- readHTMLTable("http://www.videocardbenchmark.net/mid_range_gpus.html")[[4]][,c(1,2)]
  gpu.table.midlow <- readHTMLTable("http://www.videocardbenchmark.net/midlow_range_gpus.html")[[4]][,c(1,2)]
  gpu.data <- rbind(gpu.table.high[-nrow(gpu.table.high),], gpu.table.mid[-nrow(gpu.table.mid),], gpu.table.midlow[-nrow(gpu.table.midlow),])
  colnames(gpu.data) <- c("Name", "Score")
  gpu.data$Name <- as.character(gpu.data$Name)
  gpu.data$Score <- gsub(',', '', gpu.data$Score)
  gpu.data$Score <- suppressWarnings(as.integer(gpu.data$Score))
  gpu.data$Name <- gsub(' \\+ .*$', '', gpu.data$Name)
  gpu.data$Name <- gsub(', .*$', '', gpu.data$Name)
  gpu.data$Name <- gsub(' ?/ ?.*$', '', gpu.data$Name)
  return(gpu.data)
}

# Determines whether a character vector y intersects a coherent text x
is.token.intersect <- function(x, y, ignore.case = FALSE) {
  if(ignore.case) {
    x <- tolower(x)
    y <- tolower(y)
  }
  x.tokenized <- lapply(x, function(a) MC_tokenizer(a))
  x.tokenized <- lapply(x.tokenized, function(a) setdiff(a, ""))
  intersect <- lapply(x.tokenized, function(a) intersect(a, y))
  return(as.logical(sapply(intersect, length)))
}

# Determines whether a game's name suggests it is a sequel
is.sequel <- function(text) {
  result <- lapply(text, function(x) grepl("( [1-9]?[2-9]$)|( [1-9]?[2-9]: )|( [1-9]?[2-9] (-|–) )|( [IVX]{1,5}$)|( [IVX]{1,5}: )|( [IVX]{1,5} (-|–) )", x))
  return(unlist(result))
}

# Creates a document-term matrix for training or test data (requires a dictionary)
create.tdm <- function(texts, lbound = 1, dictionary = NULL) {
  vs <- VectorSource(texts)
  corpus <- VCorpus(vs)

  if(is.null(dictionary)) {
    tdm <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE, stopwords = T, bounds = list(global = c(lbound,Inf))
                                                     , removeNumbers = T))
    dictionary <- tdm
  } else {
    tdm <- TermDocumentMatrix(corpus, control = list(dictionary=Terms(dictionary)))
  }
  tdm <- as.data.frame(as.matrix(t(tdm)))

  return(list(tdm = tdm, dictionary = dictionary))
}

#Creates PCA, possible to only supply loadings
create.pca <- function(data_, loadings = NULL, ncomp = 3, plot = FALSE, name.prefix = "pca") {
  if(is.null(loadings)) {
    pca <- princomp(as.matrix(data_), cor = TRUE, scores = TRUE)
    loadings <- pca$loadings[,1:ncomp]
  }

  if(plot) {
    screeplot(pca, npcs = ncomp, las = 3, main = "Scree plot")
    plot(1:ncol(data_), cumsum(pca$sdev**2) / ncol(data_), type = "b", ylim = c(0.0, 1.0), xlab = "r", ylab = "Variability fraction")
    abline(h = 0.8)
  }

  col.names <- sapply(1:ncomp, function(x) paste(name.prefix,x, sep = ""))

  result <- as.data.frame(as.matrix(data_) %*% loadings)
  colnames(result) <- col.names

  return(list(pca = result, loadings = loadings))
}

# Return a human-friendly name for a given attribute
attr.to.string <- function(attr.name, prefix = "") {
  name <- attr.name

  switch(attr.name
         , "AgeRequirements" = name <- "Age Requirements"
         , "Year" = name <- "Year of release"
         , "Month" = name <- "Month of release"
         , "Day" = name <- "Day of release"
         , "SinglePlayer" = name <- "Single-Player"
         , "MultiPlayer" = name <- "Multi-Player"
         , "Coop" = name <- "Co-op"
         , "LocalCoop" = name <- "Local Co-op"
         , "SteamAchievements" = name <- "Steam Achievements"
         , "SteamTradingCards" = name <- "Steam Trading Cards"
         , "SteamWorkshop" = name <- "Steam Workshop"
         , "ControllerSupport" = name <- "Controller Support"
         , "HWCPU" = name <- "Required CPU"
         , "HWGPU" = name <- "Required GPU"
         , "HWRAM" = name <- "Required RAM"
         , "HWHDD" = name <- "Required Disk Space"
         , "HWDx" = name <- "Required DirectX version"
         , "Screenshots" = name <- "Number of Screenshots"
         , "Trailers" = name <- "Number of Trailers"
         , "DRMNotice" = name <- "Third-party DRM"
         , "DRMEula" = name <- "Third-party Eula"
         , "DRMAccount" = name <- "Third-party ccount"
         , "MonthsSinceBeginning" = name <- "Release date"
         , "IsSequel" = name <- "Is a Sequel"
         , "IsCustomizable" = name <- "Allows customization"
         , "DevPrevGamesCount" = name <- "Number of games previously made by the developer"
         , "DevPrevGamesMax" = name <- "Number of players in most successful game made by the developer"
         , "DevPrevGamesMin" = name <- "Number of players in least successful game made by the developer"
         , "DevPrevGamesIneq" = name <- "Gini index of number of players in games made by the developer"
         , "PubPrevGamesCount" = name <- "Number of games previously published by the publisher"
         , "PubPrevGamesMax" = name <- "Number of players in most successful game published by the publisher"
         , "PubPrevGamesMin" = name <- "Number of players in least successful game published by the publisher"
         , "PubPrevGamesIneq" = name <- "Gini index of number of players in games published by the publisher"
         , "Weekday" = name <- "Weekday of release"
         , "LanguagesNum" = name <- "Number of supported languages"
         , "MassivelyMultiplayer" = name <- "Massively Multiplayer"
         , "ShortDescriptionLength" = name <- "Short Description Length"
         , "DescriptionLength" = name <- "Description Length"
         , "NameLength" = name <- "Name Length"
  )

  return(paste0(prefix, name))
}
