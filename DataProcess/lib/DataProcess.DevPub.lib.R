#' Returns a data frame containing info about developers and publishers from dataset based on all games in dataset.all
#' The info includes number of previous games, max and min of players from all previous games and Gini index of the number of players in previous games
#'
#' @param dataset dataset with complete information
#' @param dataset.all dataset containing all games
get.devpub <- function(dataset, dataset.all) {

  devpub <- data.frame(matrix(NA, nrow = nrow(dataset), ncol = 0))

  devpub$ID <- numeric(nrow(dataset))

  value.default <- NA

  devpub$DevPrevGamesCount <- numeric(nrow(dataset))
  devpub$DevPrevGamesMax <- value.default
  devpub$DevPrevGamesMin <- value.default
  devpub$DevPrevGamesIneq <- rep(value.default, nrow(dataset))

  devpub$PubPrevGamesCount <- numeric(nrow(dataset))
  devpub$PubPrevGamesMax <- value.default
  devpub$PubPrevGamesMin <- value.default
  devpub$PubPrevGamesIneq <- rep(value.default, nrow(dataset))

  for(i in 1:nrow(dataset)) {
    id <- dataset$ID[i]

    index.large <- which(dataset.all$ID == id)

    prevgames <- subset(dataset.all[1:(index.large-1),], Developer == dataset$Developer[i])

    devpub$ID[i] <- id

    if(nrow(prevgames) > 0) {
      devpub$DevPrevGamesCount[i] <- nrow(prevgames)

      if(!all(is.na(prevgames$Players))) {
        devpub$DevPrevGamesMax[i] <- round(log2(1 + max(prevgames$Players, na.rm = TRUE)), 2)

        devpub$DevPrevGamesMin[i] <- round(log2(1 + min(prevgames$Players, na.rm = TRUE)), 2)

        devpub$DevPrevGamesIneq[i] <- round(ineq(prevgames$Players, type = "Gini"), 2)
      }
    }

    prevgames <- subset(dataset.all[1:(index.large-1),], Publisher == dataset$Publisher[i])

    if(nrow(prevgames) > 0) {
      devpub$PubPrevGamesCount[i] <- nrow(prevgames)

      if(!all(is.na(prevgames$Players))) {
        devpub$PubPrevGamesMax[i] <- round(log2(1 + max(prevgames$Players, na.rm = TRUE)), 2)

        devpub$PubPrevGamesMin[i] <- round(log2(1 + min(prevgames$Players, na.rm = TRUE)), 2)

        devpub$PubPrevGamesIneq[i] <- round(ineq(prevgames$Players, type = "Gini"), 2)
      }
    }
  }

  return(devpub)
}
