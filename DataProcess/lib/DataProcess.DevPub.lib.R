#' Returns a data frame containing info about developers and publishers from dataset based on all games in dataset.all
#' The info includes number of previous games, max and min of players from all previous games and Gini index of the number of players in previous games
#' 
#' @param dataset dataset with complete information
#' @param dataset.all dataset containing all games
get.devpub <- function(dataset, dataset.all) {
  
  devpub <- data.frame(matrix(NA, nrow = nrow(dataset), ncol = 0))
  
  devpub$ID <- numeric(nrow(dataset))
  
  devpub$DevPrevGamesCount <- numeric(nrow(dataset))
  devpub$DevPrevGamesMax <- NA
  devpub$DevPrevGamesMin <- NA
  devpub$DevPrevGamesIneq <- rep(NA, nrow(dataset))
  
  devpub$PubPrevGamesCount <- numeric(nrow(dataset))
  devpub$PubPrevGamesMax <- NA
  devpub$PubPrevGamesMin <- NA
  devpub$PubPrevGamesIneq <- rep(NA, nrow(dataset))
  
  for(i in 1:nrow(dataset)) {
    id <- dataset$ID[i]
    
    index.large <- which(dataset.all$ID == id)
    
    prevgames <- subset(dataset.all[1:(index.large-1),], Developer == dataset$Developer[i])
    
    devpub$ID[i] <- id
    
    if(nrow(prevgames) > 0) {
      devpub$DevPrevGamesCount[i] <- nrow(prevgames)
      
      if(!all(is.na(prevgames$Players))) {
        devpub$DevPrevGamesMax[i] <- log2(1 + max(prevgames$Players, na.rm = TRUE))
        
        devpub$DevPrevGamesMin[i] <- log2(1 + min(prevgames$Players, na.rm = TRUE))
        
        devpub$DevPrevGamesIneq[i] <- round(ineq(prevgames$Players, type = "Gini"), 2)
      }
    }
    
    prevgames <- subset(dataset.all[1:(index.large-1),], Publisher == dataset$Publisher[i])
    
    if(nrow(prevgames) > 0) {
      devpub$PubPrevGamesCount[i] <- nrow(prevgames)
      
      if(!all(is.na(prevgames$Players))) {
        devpub$PubPrevGamesMax[i] <- log2(1 + max(prevgames$Players, na.rm = TRUE))
        
        devpub$PubPrevGamesMin[i] <- log2(1 + min(prevgames$Players, na.rm = TRUE))
        
        devpub$PubPrevGamesIneq[i] <- round(ineq(prevgames$Players, type = "Gini"), 2)
      }
    }
  }
  
  return(devpub)
}
