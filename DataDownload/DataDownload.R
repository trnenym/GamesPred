# Script downloads data about Steam games and stores them in separate directories

DataDownload <- function() {
  cat("Downloading games:\n")
  
  #SteamSpy API has a list of all games, unlike Steam API that lists also DLCs
  #Movies, other software etc. are still included and have to removed later
  steam.ids.json <- fromJSON("http://steamspy.com/api.php?request=all")
  steam.ids <- unname(sapply(steam.ids.json, function(x) x$appid))
  
  #Script can be interupted at any moment, it attempts to download only games that have no directory in the download folder
  dirs.processed <- list.files("./DataDownload/DataRaw", recursive = TRUE, pattern = "[0-9]+\\.RData$")
  steam.ids.processed <- as.integer(sapply(dirs.processed, function(x) gsub('/.*$','',x)))
  steam.ids.toprocess <- setdiff(steam.ids, steam.ids.processed)
  
  #The current downloaded game's ID is recorded. If interrupted after creating its directory but before saving its data, it will be downloaded again
  id.current <- suppressWarnings(readLines(con = "./DataDownload/current.download.ini"))
  if(length(id.current > 0)) {
    unlink(paste("./DataDownload/DataRaw/",id.current[[1]], sep = ""), recursive = TRUE)
    steam.ids.toprocess <- c(id.current, steam.ids.toprocess)
  }
  
  #A log containing errors is recorded
  log.path <- paste("./DataDownload/download_",format(Sys.time(), "%y-%m-%d_%H-%M"),".log", sep = "")
  cat("Download began at: ", as.character(Sys.time()), "\n\n", file = log.path, sep = "")
  
  #Downloads are attempted multiple times with a waiting period
  attempts <- 3
  timeout <- 10
  
  #When in progress, an estimate of remaining time is printed
  time.diff <- c()
  
  #The main loop for data download
  for(i in 1:length(steam.ids.toprocess)) {
    tryCatch({
      time.start <- Sys.time()
      
      success <- FALSE
      msg <- ''
      
      id <- steam.ids.toprocess[i]
      cat(id, file = "./DataDownload/current.download.ini")
      
      if(i > 1) {
        time.estimated <- as.character(seconds_to_period(as.integer(mean(time.diff) * (length(steam.ids.toprocess) - i + 1))))
      } else {
        time.estimated <- "NA"
      }
      console.rewrite(c('Downloading ', i, ' of ', length(steam.ids.toprocess), '. Estimated time remaining: ', time.estimated))
      
      #Where the actual data is stored
      gamedata <- list()
      
      #Steam API -----------------------------------------------------------------------------------------
      steamAPI.url <- paste("http://store.steampowered.com/api/appdetails?cc=us&appids=",id, sep = "") #U.S. data used
      
      for (j in 1:attempts) {
        tryCatch({
          steamAPI.json <- fromJSON(steamAPI.url)
          
          if(steamAPI.json[[1]]$success) {
            success <- TRUE
          } else {
            #Missing records are quite common, store URL gets redirected to the main page, reasons unknown
            msg <- "Record unavailable"
            cat(id, as.character(Sys.time()), "Error - Steam API: ", msg, "\n", file = log.path, sep = " ", append = TRUE)
          }
          
        },
        error=function(cond) {
          cat(id, as.character(Sys.time()), "Error - Steam API: ", as.character(cond), file = log.path, sep = " ", append = TRUE)
        },
        warning=function(cond) {
          cat(id, as.character(Sys.time()), "Error - Steam API: ", as.character(cond), file = log.path, sep = " ", append = TRUE)
        })
        
        if(success) {
          break
        }
        
        #If not successful, Internet connection is checked and keeps waiting for re-establishing
        if(connection.check()) {
          Sys.sleep(timeout)
        } else {
          connection.check(wait = TRUE)
        }
      }
      
      if(!(success)) {
        stop("Failed to load Steam API data")
      }
      success <- FALSE
      msg <- ''
      
      steamAPI.data <- steamAPI.json[[1]]$data
      
      #skip redirects = duplicates
      if(id != steamAPI.data$steam_appid) {
        next()
      }
      
      if(steamAPI.data$release_date$coming_soon | steamAPI.data$type != "game") {
        next()
      }
      
      id.name <- as.character(id)
      gamedata[[id.name]] <- list(steam = steamAPI.data)
      
      #Download screenshots
      save.folder <- paste("./DataDownload/DataRaw/",id.name,"/", sep = "")
      dir.create(save.folder)
      
      steamAPI.screenshots.list <- gamedata[[1]]$steam$screenshots[,2]
      
      for (s in 1:length(steamAPI.screenshots.list)) {
        file.name <- paste(save.folder,"screenshot",s,".jpg", sep = "")
        download.file(steamAPI.screenshots.list[s], file.name, method = "internal", mode = "wb", quiet = TRUE)
      }
      
      #Download thumbnail
      thumbnail.url <- steamAPI.data$header_image
      file.name <- paste(save.folder,"thumbnail.jpg", sep = "")
      download.file(thumbnail.url, file.name, method = "internal", mode = "wb", quiet = TRUE)
      
      #Steam Store data -----------------------------------------------------------------------------------------
      steamStore.url <- paste("http://store.steampowered.com/app/",id,"?cc=us", sep = "")
      
      for (j in 1:attempts) {
        tryCatch({
          steamStore.data <- getURI(steamStore.url, cookie = "birthtime=28801; mature_content=1; path=/; domain=store.steampowered.com")
          date <- regmatches(steamStore.data, regexpr('(?<=Release Date: <span class="date">)[^<]+(?=</span>)', steamStore.data, perl = TRUE))
          
          if(length(date) == 0) {
            stop("Unable to load data")
          } else {
            success <- TRUE
          }
          
        },
        error=function(cond) {
          cat(c(id, as.character(Sys.time()), "Error - Store data: ", as.character(cond)), file = log.path, sep = " ", append = TRUE)
        },
        warning=function(cond) {
          cat(c(id, as.character(Sys.time()), "Error - Store data: ", as.character(cond)), file = log.path, sep = " ", append = TRUE)
        })
        
        if(success) {
          break
        }
        
        if(connection.check()) {
          Sys.sleep(timeout)
        } else {
          connection.check(wait = TRUE)
        }
      }
      
      if(!(success)) {
        stop("Failed to load Store data")
      }
      success <- FALSE
      msg <- ''
      
      #user tags
      tags.raw <- regmatches(steamStore.data, gregexpr ("(?<=class=\"app_tag\" style=\"display: none;\">)[^<]+(?=</a>)", steamStore.data, perl = TRUE))
      tags <- regmatches(tags.raw[[1]], gregexpr ("(?<=\t)[^\t]+(?=\t)", tags.raw[[1]], perl = TRUE))
      tags <- unlist(tags)
      
      if(length(tags) == 0) {
        tags <- NA
      }
      
      tags.list <- list(tags = tags)
      gamedata[[1]] <- c(gamedata[[1]], tags.list)
      
      #release date
      day <- as.numeric(regmatches(date, regexpr ("(?<= )[0-9]+(?=, )", date, perl = TRUE)))
      month.str <- regmatches(date, regexpr ("^[a-zA-Z]+(?= [0-9])", date, perl = TRUE))
      month <- match(month.str, month.abb)
      year <- as.numeric(regmatches(date, regexpr ("(?<=, )[0-9]+(?=$)", date, perl = TRUE)))
      
      if(length(day) == 0 | length(month) == 0 | length(year) == 0) {
        stop("Full release date unavailable")
      }
      
      month.current <- 1 + as.POSIXlt(Sys.Date())$mon
      year.current <- 1900 + as.POSIXlt(Sys.Date())$year
      month.max <- month.add(month.current, year.current, -1)[1]
      year.max <- month.add(month.current, year.current, -1)[2]
      
      #Skip if Steam Charts data not available yet = game too new
      if(year == year.max & month > month.max) {
        unlink(paste("./DataDownload/DataRaw/",id, sep = ""), recursive = TRUE)
        next()
      }
      
      release.date.list <- list(year = year, month = month, day = day)
      gamedata[[1]] <- c(gamedata[[1]], list(release.date = release.date.list))
      
      # #PEGI
      # pegi <- regmatches(steamStore.data, regexpr('(?<=<a href="http://www.pegi.info/"><img src="http://store.akamai.steamstatic.com/public/images/ratings/PEGI/).{0,2}(?=.gif"></a>)'
      #                                             , steamStore.data, perl = TRUE))
      # if(length(pegi) == 0) {
      #   pegi <- 0
      # }
      # pegi.list <- list(pegi = pegi)
      # gamedata[[1]] <- c(gamedata[[1]], pegi.list)
      
      #ESRB
      esrb <- regmatches(steamStore.data, regexpr('(?<=<p><img src="http://store.akamai.steamstatic.com/public/images/ratings/esrb_).{1,2}(?=.gif"></p>)'
                                                  , steamStore.data, perl = TRUE))
      if(length(esrb) == 0) {
        esrb <- 0
      }
      esrb.list <- list(esrb = esrb)
      gamedata[[1]] <- c(gamedata[[1]], esrb.list)
      
      #short description
      description.short <- regmatches(steamStore.data, regexpr('(?<=<meta name="Description" content=")[^">]+(?=">)', steamStore.data, perl = TRUE))
      description.short.list <- list(description.short = description.short)
      gamedata[[1]] <- c(gamedata[[1]], description.short.list)
      
      #DRM notice
      drm.notice <- FALSE
      drm.account <- FALSE
      drm.eula <- FALSE
      drm.securom <- FALSE
      
      if (length(regmatches(steamStore.data, regexpr('<div class="DRM_notice">', steamStore.data, perl = TRUE))) > 0) {
        drm.notice <- TRUE
        
        if(length(regmatches(steamStore.data, regexpr('Requires 3rd-Party Account', steamStore.data, perl = TRUE))) > 0) {
          drm.account <- TRUE
        }
        
        if(length(regmatches(steamStore.data, regexpr('Requires agreement to a 3rd-party EULA', steamStore.data, perl = TRUE))) > 0) {
          drm.eula <- TRUE
        }
        
        if(length(regmatches(steamStore.data, regexpr('Incorporates 3rd-party DRM: SecuROM', steamStore.data, perl = TRUE))) > 0) {
          drm.securom <- TRUE
        }
      }
      
      drm.list <- list(drm.notice = drm.notice, drm.eula = drm.eula, drm.account = drm.account, drm.securom = drm.securom)
      gamedata[[1]] <- c(gamedata[[1]], drm.list)
      
      #Steam reviews -----------------------------------------------------------------------------------------
      #To try and determine whether a game was in Early Access in the past - usually has some "Early Access Reviews"
      steam.reviews.ea.url <- paste("http://steamcommunity.com/app/",id,"/reviews/?browsefilter=toprated&cc=us", sep = "")
      
      for (j in 1:attempts) {
        tryCatch({
          steam.reviews.ea.data <- getURI(steam.reviews.ea.url, cookie = "birthtime=28801; path=/; domain=steamcommunity.com")
          steam.reviews.ea <- regmatches(steam.reviews.ea.data, gregexpr ('<div class="early_access_review">Early Access Review</div>', steam.reviews.ea.data, perl = TRUE))
          success <- TRUE
          
        },
        error=function(cond) {
          cat(id, as.character(Sys.time()), "Error - Steam reviews: ", substr(as.character(cond),1,150), "\n", file = log.path, sep = " ", append = TRUE)
        },
        warning=function(cond) {
          cat(id, as.character(Sys.time()), "Error - Steam reviews: ", substr(as.character(cond),1,150), "\n", file = log.path, sep = " ", append = TRUE)
        })
        
        if(success) {
          break
        }
        
        if(connection.check()) {
          Sys.sleep(timeout)
        } else {
          connection.check(wait = TRUE)
        }
      }
      
      if(!(success)) {
        stop("Failed to load Steam reviews")
      }
      success <- FALSE
      msg <- ''
      
      early.access.reviews <- FALSE
      
      if(length(steam.reviews.ea[[1]]) > 0) {
        early.access.reviews <- TRUE
      }
      
      early.access.reviews.list <- list(early.access.reviews = early.access.reviews)
      gamedata[[1]] <- c(gamedata[[1]], early.access.reviews.list)
      
      
      #SteamCharts -----------------------------------------------------------------------------------------
      #Data about monthly concurrent players
      steamCharts.url <- paste("http://steamcharts.com/app/",id, sep = "")
      
      for (j in 1:attempts) {
        tryCatch({
          steamCharts.tables <- readHTMLTable(steamCharts.url)
          steamCharts.tables.nrows <- unlist(lapply(steamCharts.tables, function(t) dim(t)[1]))
          success <- TRUE
          
        },
        error=function(cond) {
          cat(id, as.character(Sys.time()), "Error - steam charts: ", as.character(cond), file = log.path, sep = " ", append = TRUE)
        },
        warning=function(cond) {
          cat(id, as.character(Sys.time()), "Error - steam charts: ", as.character(cond), file = log.path, sep = " ", append = TRUE)
        })
        
        if(success & length(steamCharts.tables) > 0) {
          break
        }
        
        if(connection.check()) {
          Sys.sleep(timeout)
        } else {
          connection.check(wait = TRUE)
        }
      }
      
      if(!(success) | length(steamCharts.tables) == 0) {
        stop("Failed to load data from steamcharts.com")
      }
      success <- FALSE
      msg <- ''
      
      steamCharts.data <- steamCharts.tables[[which.max(steamCharts.tables.nrows)]]
      steamCharts.data.list <- list(steamCharts.data = steamCharts.data)
      
      gamedata[[1]] <- c(gamedata[[1]], steamCharts.data.list)
      
      
      #Launch price -----------------------------------------------------------------------------------------
      #Steam only lists current price, steamsales.rhekua.com has price history since around 2010, only data since 2012 are needed
      launch.price <- NA
      
      if(!(steamAPI.data$is_free)) {
        rhekua.url <- paste("http://steamsales.rhekua.com/view.php?steam_type=app&steam_id=",id, sep = "")
        
        for (j in 1:attempts) {
          tryCatch({
            rhekua.data  <- getURL(rhekua.url)
            rhekua.prices <- regmatches(rhekua.data, gregexpr ("(?<=<strike>.).+(?=</strike>)", rhekua.data, perl = TRUE))[[1]]
            success <- TRUE
            
          },
          error=function(cond) {
            cat(id, as.character(Sys.time()), "Error - steamsales.rhekua.com: ", as.character(cond), file = log.path, sep = " ", append = TRUE)
          },
          warning=function(cond) {
            cat(id, as.character(Sys.time()), "Error - steamsales.rhekua.com: ", as.character(cond), file = log.path, sep = " ", append = TRUE)
          })
          
          if(success & length(rhekua.prices) > 0) {
            launch.price <- as.numeric(rhekua.prices[length(rhekua.prices)]) #prices are in dollars
            break
          }
          
          #If a game has no record on steamsales.rhekua.com and was released recently, it may be because it hasn't been on sale yet -> price on Steam is OK
          month.current <- 1 + as.POSIXlt(Sys.Date())$mon
          year.current <- 1900 + as.POSIXlt(Sys.Date())$year
          date.max <- month.add(month, year, 6)
          
          if(date.max[2] > year.current | (date.max[2] == year.current & date.max[1] >= month.current)) {
            launch.price <- steamAPI.data$price_overview$initial / 100
            break
          } else {
            msg <- "Price unavailable"
          }
          
          cat(id, as.character(Sys.time()), "Error - steamsales.rhekua.com: ", msg, "\n", file = log.path, sep = " ", append = TRUE)
          
          if(connection.check()) {
            Sys.sleep(timeout)
          } else {
            connection.check(wait = TRUE)
          }
        }
      }
      
      success <- FALSE
      
      gamedata[[1]] <- c(gamedata[[1]], launch.price = launch.price)
      
      
      #Save file -----------------------------------------------------------------------------------------
      save.file.path <- paste(save.folder,id.name,".RData", sep = "")
      save(gamedata, file = save.file.path)
    },
    error=function(cond) {
      unlink(paste("./DataDownload/DataRaw/",id, sep = ""), recursive = TRUE)
      cat(id, as.character(Sys.time()), as.character(cond), file = log.path, sep = " ", append = TRUE)
    },
    warning=function(cond) {
      unlink(paste("./DataDownload/DataRaw/",id, sep = ""), recursive = TRUE)
      cat(id, as.character(Sys.time()), as.character(cond), file = log.path, sep = " ", append = TRUE)
    },
    finally={
      cat("", file = "./DataDownload/current.download.ini")
      Sys.sleep(5)
      time.end <- Sys.time()
      time.diff <- c(time.diff, as.integer(time.end - time.start))
    })
  }
  
  console.rewrite('Done!')
  
  cat("Download finished at: ", as.character(Sys.time()), file = log.path, sep = "", append = TRUE)
}
