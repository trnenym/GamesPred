#Script processes previously downloaded data

DataProcess01.Raw <- function() {
  options(warn=2)
  to.delete <- c()

  cat("Processing downloads:\n")
  console.rewrite(c("Initializing..."))

  # Individually process games in ./DataDownload/DataRaw
  dirs <- list.dirs("./DataDownload/DataRaw", full.names = FALSE)[-1]
  dataset.all <- data.frame(ID = numeric(0))
  dirs.length <- length(dirs)

  if(dirs.length == 0) {
    stop("You need to download some game data first!")
  }

  hw.gpu.table <- gpu.chart()
  save(hw.gpu.table, file = "./DataProcess/Data/gpu.chart.RData")

  #Default values for HW minimum requirements (when missing)
  hw.cpu.value.default <- NA
  hw.gpu.score.default <- NA
  hw.ram.value.default <- NA
  hw.hdd.value.default <- NA
  hw.dx.version.default <- NA

  for(i in 1:dirs.length) {
    console.rewrite(c("Processing ", i, " of ", dirs.length))

    path <- paste("DataDownload/DataRaw/",dirs[i],"/",dirs[i],".RData", sep = "")
    load(path)

    #Steam ID
    dataset.all[i,1] <- gamedata[[1]]$steam$steam_appid

    #Qualifies for further processing
    relevant <- TRUE
    dataset.all$Relevant[i] <- TRUE

    #Name
    name <- gamedata[[1]]$steam$name
    name <- cleanHTML(name)
    name <- iconv(name, to="ASCII", sub = "")
    dataset.all$Name[i] <- name

    #Developer
    developer <- gamedata[[1]]$steam$developers[1]

    if(is.null(developer) || is.na(developer) || nchar(developer) <= 1) {
      developer <- paste('unknown',i, sep = "")
    }

    dataset.all$Developer[i] <- cleanHTML(developer)

    #Publisher
    publisher <- gamedata[[1]]$steam$publishers[1]

    if(is.null(publisher) || is.na(publisher) || nchar(publisher) <= 1) {
      publisher <- paste('unknown',i, sep = "")
    }

    dataset.all$Publisher[i] <- cleanHTML(publisher)

    #Age requirements
    dataset.all$AgeRequirements[i] <- gamedata[[1]]$esrb

    #Release date
    year <- gamedata[[1]]$release.date$year
    month <- gamedata[[1]]$release.date$month
    day <- gamedata[[1]]$release.date$day

    dataset.all$Year[i] <- year
    dataset.all$Month[i] <- month
    dataset.all$Day[i] <- day

    month.current <- 1 + as.POSIXlt(Sys.Date())$mon
    year.current <- 1900 + as.POSIXlt(Sys.Date())$year
    month.max <- month.add(month.current, year.current, -1)[1]
    year.max <- month.add(month.current, year.current, -1)[2]

    if(((year == 2012 && month < 7) || year < 2012) || (year > year.max || (year == year.max && month > month.max))) {
      relevant <- FALSE
    }

    #Launch price (the oldest record available)
    if(length(gamedata[[1]]$launch.price) == 0 || is.na(gamedata[[1]]$launch.price)) {
      dataset.all$Price[i] <- NA
      relevant <- FALSE
    } else {
      dataset.all$Price[i] <- round(gamedata[[1]]$launch.price, 0)
    }

    #Short description
    if(is.na(gamedata[[1]]$description.short) || length(gamedata[[1]]$description.short) == 0) {
      dataset.all$ShortDescription[i] <- "null"
    } else {
      dataset.all$ShortDescription[i] <- cleanHTML(gamedata[[1]]$description.short)
    }

    #Long description
    desc <- gamedata[[1]]$steam$about_the_game
    desc <- cleanHTML(desc)
    desc <- gsub("\"", "", desc)
    desc <- gsub("&quot;", "", desc)
    dataset.all$Description[i] <- desc

    #Platforms
    dataset.all$Windows[i] <- gamedata[[1]]$steam$platforms$windows
    dataset.all$Mac[i] <- gamedata[[1]]$steam$platforms$mac
    dataset.all$Linux[i] <- gamedata[[1]]$steam$platforms$linux

    #Categories
    categories <- gamedata[[1]]$steam$categories$description

    dataset.all$SinglePlayer[i] <- "Single-player" %in% categories
    dataset.all$MultiPlayer[i] = "Multi-player" %in% categories || "Online Multi-Player" %in% categories
    dataset.all$CrossPlatformMP[i] <- "Cross-Platform Multiplayer" %in% categories
    dataset.all$LocalMP[i] = "Local Co-op" %in% categories || "Local Multi-Player" %in% categories
    dataset.all$Coop[i] = "Co-op" %in% categories
    dataset.all$OnlineCoop[i] = "Online Co-op" %in% categories
    dataset.all$SplitScreen[i] = "Shared/Split Screen" %in% categories
    dataset.all$SteamAchievements[i] = "Steam Achievements" %in% categories
    dataset.all$SteamLeaderboards[i] = "Steam Leaderboards" %in% categories
    dataset.all$SteamTradingCards[i] = "Steam Trading Cards" %in% categories
    dataset.all$SteamWorkshop[i] = "Steam Workshop" %in% categories
    dataset.all$LevelEditor[i] = "Includes level editor" %in% categories
    dataset.all$PartialControllerSupport[i] = "Partial Controller Support" %in% categories
    dataset.all$FullControllerSupport[i] = "Full controller support" %in% categories
    dataset.all$VRSupport[i] = "VR Support" %in% categories
    dataset.all$SteamCloud[i] = "Steam Cloud" %in% categories
    dataset.all$ValveAntiCheat[i] = "Valve Anti-Cheat enabled" %in% categories
    dataset.all$CaptionsAvailable[i] = "Captions available" %in% categories
    dataset.all$CommentaryAvailable[i] = "Commentary available" %in% categories
    dataset.all$InAppPurchases[i] = "In-App Purchases" %in% categories


    # if(dataset.all$VRSupport[i]) {
    #   relevant <- FALSE
    # }

    #Early Access
    genres <- gamedata[[1]]$steam$genres$description

    dataset.all$EarlyAccessNow[i] <- FALSE

    if("Early Access" %in% genres) {
      dataset.all$EarlyAccessNow[i] <- TRUE
      relevant <- FALSE
    }

    dataset.all$EarlyAccessPast[i] <- FALSE

    if(gamedata[[1]]$early.access.reviews) {
      dataset.all$EarlyAccessPast[i] <- TRUE
      relevant <- FALSE
    }

    # Controller
    # controller <- gamedata[[1]]$steam$controller_support
    #
    # if(is.null(controller) || controller == "none") {
    #   controller <- FALSE
    # } else {
    #   controller <- TRUE
    # }
    #
    # dataset.all$ControllerSupport[i] <- controller

    #HW requirements
    hw.string <- gamedata[[1]]$steam$pc_requirements$minimum

    if(!is.null(hw.string)) {
      hw.cpu.string <- regmatches(hw.string, gregexpr ("<strong>Processor: ?</strong> ?\\K[^<]*(?=\r(<br>)?)", hw.string, perl = TRUE))[[1]]
      hw.cpu.string <- iconv(paste(hw.cpu.string, collapse = ""), to = "ASCII", sub = "")
      hw.gpu.string <- regmatches(hw.string, gregexpr ("<strong>(Graphics|Video Card): ?</strong> ?\\K[^<]*(?=\r(<br>)?)", hw.string, perl = TRUE))[[1]]
      hw.gpu.string <- iconv(paste(hw.gpu.string, collapse = ""), to = "ASCII", sub = "")
      hw.ram.string <- regmatches(hw.string, gregexpr ("<strong>Memory: ?</strong> ?\\K[^<]*(?=\r(<br>)?)", hw.string, perl = TRUE))[[1]]
      hw.ram.string <- iconv(paste(hw.ram.string, collapse = ""), to = "ASCII", sub = "")
      hw.hdd.string <- regmatches(hw.string, gregexpr ("<strong>(Storage|Hard Disk Space|Hard Drive): ?</strong> ?\\K[^<]*(?=\r(<br>)?)"
                                                       , hw.string, perl = TRUE))[[1]]
      hw.hdd.string <- iconv(paste(hw.hdd.string, collapse = ""), to = "ASCII", sub = "")
      hw.dx.string <- regmatches(hw.string, gregexpr ("<strong>DirectX.?: ?</strong> ?\\K[^<]*(?=\r(<br>)?)", hw.string, perl = TRUE))[[1]]
      hw.dx.string <- iconv(paste(hw.dx.string, collapse = ""), to = "ASCII", sub = "")

      hw.cpu.value <- hw.cpu.value.default

      if(length(grep("(^| )[0-9].?GHz|Gigahertz", hw.cpu.string, ignore.case = TRUE)) > 0) {
        if(length(grep("Pentium\\.*III", hw.cpu.string, ignore.case = TRUE)) > 0) {
          hw.cpu.value <- 1024
        } else {
          hw.cpu.value <- unlist(regmatches(hw.cpu.string,gregexpr("(^| )\\K[[:digit:]]+(\\.|\\,)*[[:digit:]]*(?=.?(GHz|Gigahertz))"
                                                                   ,hw.cpu.string, perl = TRUE, ignore.case = TRUE)))[[1]]
          hw.cpu.value <- as.numeric(gsub(',', '.', hw.cpu.value)) * 2**10
        }
      } else {
        if(length(grep("(^| )[0-9].?MHz|Megahertz", hw.cpu.string, ignore.case = TRUE)) > 0) {
          hw.cpu.value <- unlist(regmatches(hw.cpu.string,gregexpr("(^| )\\K[[:digit:]]+(\\.|,)*[[:digit:]]*(?=.?(MHz|Megahertz))"
                                                                   ,hw.cpu.string, perl = TRUE, ignore.case = TRUE)))[[1]]
          hw.cpu.value <- as.numeric(gsub(',', '.', hw.cpu.value))
        } else {
          if(length(grep("(^| )[0-9].?KHz|Kilohertz", hw.cpu.string, ignore.case = TRUE)) > 0) {
            hw.cpu.value <- unlist(regmatches(hw.cpu.string,gregexpr("(^| )\\K[[:digit:]]+(\\.|,)*[[:digit:]]*(?=.?(KHz|Kilohertz))"
                                                                     ,hw.cpu.string, perl = TRUE, ignore.case = TRUE)))[[1]]
            hw.cpu.value <- as.numeric(gsub(',', '.', hw.cpu.value)) * 2**(-10)
          }
        }
      }

      hw.gpu.score <- hw.gpu.score.default

      hw.gpu.matches <- lapply(hw.gpu.table$Name, function(x) grep(x, hw.gpu.string, ignore.case = TRUE))
      hw.gpu.matches <- hw.gpu.table[which(hw.gpu.matches > 0),]
      if(nrow(hw.gpu.matches) > 0) {
        hw.gpu.score <- hw.gpu.matches$Score[1]
      }

      hw.ram.value <- hw.ram.value.default

      if(length(grep("[0-9].?GB", hw.ram.string, ignore.case = TRUE)) > 0) {
        hw.ram.value <- regmatches(hw.ram.string, gregexpr ("[[:digit:]]+\\.*[[:digit:]]*(?=.?GB)", hw.ram.string, perl = TRUE, ignore.case = TRUE))
        hw.ram.value <- as.numeric(unlist(hw.ram.value))[[1]]
        if(hw.ram.value < 32) {
          hw.ram.value <- hw.ram.value * 2**10
        }
      } else {
        if(length(grep("[0-9].?MB", hw.ram.string, ignore.case = TRUE)) > 0) {
          hw.ram.value <- regmatches(hw.ram.string, gregexpr ("[[:digit:]]+\\.*[[:digit:]]*(?=.?MB)", hw.ram.string, perl = TRUE, ignore.case = TRUE))
          hw.ram.value <- as.numeric(unlist(hw.ram.value))[[1]]
        }
      }

      hw.hdd.value <- hw.hdd.value.default

      if(length(grep("[0-9].?GB", hw.hdd.string, ignore.case = TRUE)) > 0) {
        hw.hdd.value <- as.numeric(unlist(regmatches(hw.hdd.string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*(?=.?GB)",hw.hdd.string, perl = TRUE
                                                                            , ignore.case = TRUE))))[[1]]
        if(hw.hdd.value < 200) {
          hw.hdd.value <- hw.hdd.value * 2**10
        }
      } else {
        if(length(grep("[0-9].?MB", hw.hdd.string, ignore.case = TRUE)) > 0) {
          hw.hdd.value <- as.numeric(unlist(regmatches(hw.hdd.string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*(?=.?MB)",hw.hdd.string, perl = TRUE
                                                                              , ignore.case = TRUE))))[[1]]
        } else {
          if(length(grep("[0-9].?KB", hw.hdd.string, ignore.case = TRUE)) > 0) {
            hw.hdd.value <- as.numeric(unlist(regmatches(hw.hdd.string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*(?=.?KB)",hw.hdd.string, perl = TRUE
                                                                                , ignore.case = TRUE))))[[1]] * 2**(-10)
          }
        }
      }

      hw.dx.version <- hw.dx.version.default

      if(length(grep("[0-9]", hw.dx.string, ignore.case = TRUE)) > 0) {
        hw.dx.version <- regmatches(hw.dx.string, gregexpr ("[0-9][0-9]?\\.?[0-9]?", hw.dx.string, perl = TRUE))[[1]]
        hw.dx.version <- as.numeric(hw.dx.version[1])

        if(hw.dx.version > 50) {
          hw.dx.version <- hw.dx.version / 10
        }
      }

      if(!is.na(hw.ram.value)) {
        if(hw.ram.value < 5 & ((is.na(hw.gpu.score) || hw.gpu.score > 143) | (is.na(hw.hdd.value) || hw.hdd.value > 200))) {
          hw.ram.value <- hw.ram.value * 2**10
        }
      }

      dataset.all$HWCPU[i] <- hw.cpu.value
      dataset.all$HWGPU[i] <- hw.gpu.score
      dataset.all$HWRAM[i] <- hw.ram.value
      dataset.all$HWHDD[i] <- hw.hdd.value
      dataset.all$HWDx[i] <- hw.dx.version
    } else {
      dataset.all$HWCPU[i] <- hw.cpu.value.default
      dataset.all$HWGPU[i] <- hw.gpu.score.default
      dataset.all$HWRAM[i] <- hw.ram.value.default
      dataset.all$HWHDD[i] <- hw.hdd.value.default
      dataset.all$HWDx[i] <- hw.dx.version.default

    }


    #Languages
    languages <- gamedata[[1]]$steam$supported_languages
    languages <- cleanHTML(languages)
    languages <- gsub("\\*languages with full audio support", "", languages)
    languages <- gsub("\\*", "", languages)
    languages <- strsplit(languages, ", ")[[1]]

    dataset.all$Languages[i] <- paste(languages, collapse = ";")


    #Unwanted genres
    if("Animation & Modeling" %in% genres || "Photo Editing" %in% genres || "Utilities" %in% genres ||
       "Design & Illustration" %in% genres || "Web Publishing" %in% genres || "Audio Production" %in% genres || "Software Training" %in% genres ||
       "Video Production" %in% genres || "Accounting" %in% genres) {

      to.delete <- c(to.delete, i)
    }

    if("Free to Play" %in% genres) {
      relevant <- FALSE
    }

    #Genres
    dataset.all$Genres[i] <- paste(genres, collapse = ";")

    #Number of screenshots
    screenshots <- nrow(gamedata[[1]]$steam$screenshots)

    if(is.null(screenshots)) {
      screenshots <- 0
    }

    dataset.all$Screenshots[i] <- screenshots

    #Number of trailers
    trailers <- nrow(gamedata[[1]]$steam$movies)

    if(is.null(trailers)) {
      trailers <- 0
    }

    dataset.all$Trailers[i] <- trailers

    #User tags
    tags <- paste(gamedata[[1]]$tags, collapse = ";")
    dataset.all$UserTags[i] <- tags

    #DRM notices
    dataset.all$DRMNotice[i] <- gamedata[[1]]$drm.notice
    dataset.all$DRMEula[i] <- gamedata[[1]]$drm.eula
    dataset.all$DRMAccount[i] <- gamedata[[1]]$drm.account
    dataset.all$DRMSecuRom[i] <- gamedata[[1]]$drm.securom

    #Steam Charts
    sc.table.raw <- gamedata[[1]]$steamCharts.data[-1,]
    sc.table <- data.frame(year = as.numeric(as.character(gsub(".+(?= [0-9]{4})", "", sc.table.raw$Month, perl = TRUE))),
                           month = match(gsub(" [0-9]{4}", "", sc.table.raw$Month, perl = TRUE), month.name),
                           avg = as.numeric(as.character(sc.table.raw$`Avg. Players`)),
                           peak = as.numeric(as.character(sc.table.raw$`Peak Players`)))

    players <- steam.charts(sc.table, year, month, day)

    if(is.na(players)) {
      relevant <- FALSE
    }

    dataset.all$Players[i] <- round(players, 1)

    dataset.all$Relevant[i] <- relevant
  }

  console.rewrite(c("Finishing..."))

  if(length(to.delete) > 0) {
    dataset.all <- dataset.all[-to.delete,]
  }

  rownames(dataset.all) <- dataset.all$ID

  dataset.all <- dataset.all[order(dataset.all$Year, dataset.all$Month, dataset.all$Day),]

  save(dataset.all, file = "DataProcess/Datasets/dataset.all.RData")

  console.rewrite(c("Done!\n"))

  options(warn=0)
}
