#' Training, validation, and test set are processed separately, some data is saved during the creation of train set to make further processing faster
#' 
#' @param dataset.processed dataset passed from DataProcess03.Split.R
#' @param type type of processed dataset: "train", "val" (validation) or "test"
#' @param mode "reg" for regression or "class" for classification
#' @param min.devpub only games by a developer/publisher with this many games on their account are processed
#' @param seed random generator seed
#' @param top.terms how many top terms (based on information gain) from a description's document-term matrix should be selected
process.final <- function(dataset.processed, type, mode, evaluation, min.devpub, seed = 1, top.terms = 50) {
  
  if(!(type == "train" || type == "val" || type == "test")) {
    stop("Wrong type")
  }
  
  # Cache for data that will be needed later
  if(type == "train") {
    cache <- list()
  } else {
    load("./DataProcess/Data/cache.RData")
  }
  
  if(type == "train") {
    cache$seed <- seed
  } else {
    seed <- cache$seed
  }
  
  set.seed(seed)
  
  cat("Processing developers and publishers...", "\n")
  
  # Experiments show that it only makes sense to predict games from developer/publisher who has already released at least two games
  # Only these games are kept
  if(type == "train") {
    pubs.exp.table <- as.data.frame(table(dataset.processed$Publisher))
    pubs.exp <- droplevels(pubs.exp.table[pubs.exp.table$Freq >= abs(min.devpub), 1])
    cache$pubs.exp <- pubs.exp
    
    devs.exp.table <- as.data.frame(table(dataset.processed$Developer))
    devs.exp <- droplevels(devs.exp.table[devs.exp.table$Freq >= abs(min.devpub), 1])
    cache$devs.exp <- devs.exp
  } else {
    pubs.exp <- cache$pubs.exp
    devs.exp <- cache$devs.exp
  }
  
  if(evaluation) {
    if(type == "train") {
      dataset.processed$DevExp <- "other"
      for (i in 1:nrow(dataset.processed)) {
        if(dataset.processed$Developer[i] %in% devs.exp) {
          dataset.processed$DevExp[i] <- dataset.processed$Developer[i]
        }
      }
      dataset.processed$DevExp <- as.factor(dataset.processed$DevExp)
    } else {
      dataset.processed$DevExp <- "other"
      for (i in 1:nrow(dataset.processed)) {
        if(dataset.processed$Developer[i] %in% devs.exp) {
          dataset.processed$DevExp[i] <- dataset.processed$Developer[i]
        }
      }
      dataset.processed$DevExp <- as.factor(dataset.processed$DevExp)
      dataset.processed$DevExp <- droplevels(dataset.processed$DevExp)
      additional.levels <- setdiff(levels(dataset.processed$DevExp), levels(dataset.processed$DevExp))
      levels(dataset.processed$DevExp) <- c(levels(dataset.processed$DevExp), additional.levels)
    }
    
    if(type == "train") {
      dataset.processed$PubExp <- "other"
      for (i in 1:nrow(dataset.processed)) {
        if(dataset.processed$Publisher[i] %in% pubs.exp) {
          dataset.processed$PubExp[i] <- dataset.processed$Publisher[i]
        }
      }
      dataset.processed$PubExp <- as.factor(dataset.processed$PubExp)
    } else {
      dataset.processed$PubExp <- "other"
      for (i in 1:nrow(dataset.processed)) {
        if(dataset.processed$Publisher[i] %in% pubs.exp) {
          dataset.processed$PubExp[i] <- dataset.processed$Publisher[i]
        }
      }
      dataset.processed$PubExp <- as.factor(dataset.processed$PubExp)
      dataset.processed$PubExp <- droplevels(dataset.processed$PubExp)
      additional.levels <- setdiff(levels(dataset.processed$PubExp), levels(dataset.processed$PubExp))
      levels(dataset.processed$PubExp) <- c(levels(dataset.processed$PubExp), additional.levels)
    }
    
    if(min.devpub >= 0) {
      dataset.processed <- subset(dataset.processed, DevExp != "other" | PubExp != "other")
    } else {
      dataset.processed <- subset(dataset.processed, DevExp == "other" & PubExp == "other")
    }
    
    dataset.processed <- dataset.processed[,-which(colnames(dataset.processed) == "DevExp")]
    dataset.processed <- dataset.processed[,-which(colnames(dataset.processed) == "PubExp")]
  }
  
  
  # Add an attributes for developers and publishers containing the developer/publisher or "other" if they don't belong to the top 52
  # (Random Forest has a limit of 53 levels)
  if(type == "train") {
    devs.top.table <- as.data.frame(table(dataset.processed$Developer))
    devs.top.table <- devs.top.table[order(devs.top.table$Freq, decreasing = TRUE),]
    devs.top <- factor(devs.top.table$Var1[1:52], levels = c(as.character(devs.top.table$Var1[1:52]), "other"))
    cache$devs.top <- devs.top
    
    dataset.processed$DevTop <- factor("other", levels = levels(devs.top))
    for (i in 1:nrow(dataset.processed)) {
      if(dataset.processed$Developer[i] %in% devs.top) {
        dataset.processed$DevTop[i] <- dataset.processed$Developer[i]
      }
    }
  } else {
    devs.top <- cache$devs.top
    dataset.processed$DevTop <- factor("other", levels = levels(devs.top))
    for (i in 1:nrow(dataset.processed)) {
      if(dataset.processed$Developer[i] %in% devs.top) {
        dataset.processed$DevTop[i] <- dataset.processed$Developer[i]
      }
    }
  }
  
  
  if(type == "train") {
    pubs.top.table <- as.data.frame(table(dataset.processed$Publisher))
    pubs.top.table <- pubs.top.table[order(pubs.top.table$Freq, decreasing = TRUE),]
    pubs.top <- factor(pubs.top.table$Var1[1:52], levels = c(as.character(pubs.top.table$Var1[1:52]), "other"))
    cache$pubs.top <- pubs.top
    
    dataset.processed$PubTop <- factor("other", levels = levels(pubs.top))
    for (i in 1:nrow(dataset.processed)) {
      if(dataset.processed$Publisher[i] %in% pubs.top) {
        dataset.processed$PubTop[i] <- dataset.processed$Publisher[i]
      }
    }
  } else {
    pubs.top <- cache$pubs.top
    dataset.processed$PubTop <- factor("other", levels = levels(pubs.top))
    for (i in 1:nrow(dataset.processed)) {
      if(dataset.processed$Publisher[i] %in% pubs.top) {
        dataset.processed$PubTop[i] <- dataset.processed$Publisher[i]
      }
    }
  }
  
  class.helper.processed <- dataset.processed$ClassHelper
  dataset.processed <- dataset.processed[,-which(colnames(dataset.processed) == "ClassHelper")]
  
  players.train <- dataset.processed$Players
  dataset.processed <- dataset.processed[,-which(colnames(dataset.processed) == "Players")]
  
  class.train <- dataset.processed$Class
  dataset.processed <- dataset.processed[,-which(colnames(dataset.processed) == "Class")]
  
  # Create document-term matrix and select top top.terms terms based on information gain
  
  if((type == "train" && top.terms > 0) || (type != "train" && !is.null(cache$terms.selected))) {
    cat("Processing descriptions...", "\n")
    
    if(type == "train") {
      description.tdm.list <- create.tdm(dataset.processed$Description, lbound = 10)
      cache$dictionary <- description.tdm.list$dictionary
    } else {
      dictionary <- cache$dictionary
      description.tdm.list <- create.tdm(dataset.processed$Description, dictionary = dictionary)
    }
    
    if(type == "train") {
      description.tdm <- cbind(description.tdm.list$tdm, ClassHelper = class.helper.processed)
      score <- InfoGainAttributeEval(ClassHelper~., description.tdm)
      score <- data.frame(Terms = colnames(description.tdm.list$tdm), AttrImportance = score)
      terms.selected <- score[order(score$AttrImportance, decreasing = T),]$Terms[1:top.terms]
      cache$terms.selected <- sort(as.character(terms.selected))
    } else {
      description.tdm <- description.tdm.list$tdm
      terms.selected <- cache$terms.selected
    }
    
    description.tdm.sig <- subset(description.tdm, select = terms.selected)
    
    colnames(description.tdm.sig) <- gsub("[^\\w]", "", colnames(description.tdm.sig), perl = TRUE)
    colnames(description.tdm.sig) <- sapply(colnames(description.tdm.sig), function(x) paste0("DescTF", x))
    dataset.processed <- cbind(dataset.processed, description.tdm.sig)
  }
  
  cat("Reducing the size of the dataset...", "\n")
  
  # Basic attribute filtering
  if(type == "train") {
    min.support <- 10
    to.delete <- c(1:4,10,11)
    for(i in 1:ncol(dataset.processed)) {
      freqs <- as.data.frame(table(dataset.processed[,i]))$Freq
      if(length(freqs) <= 1 || (length(freqs) == 2 & min(freqs) < min.support)) {
        to.delete <- c(to.delete, i)
      }
    }
    cache$to.delete <- to.delete
  } else {
    to.delete <- cache$to.delete
  }
  
  dataset.processed <- dataset.processed[,-to.delete]
  
  # Imputing missing values
  dataset.processed.na <- sapply(c(1:ncol(dataset.processed)), function(x) !all(is.finite(dataset.processed[,x])))
  
  if(any(dataset.processed.na)) {
    cat("Imputing missing values for:", colnames(dataset.processed)[dataset.processed.na], "\n")
    
    if(type == "train") {
      dataset.processed$Players <- players.train
      
      dataset.processed.imp <- missForest(dataset.processed)
      dataset.processed[,dataset.processed.na] <- dataset.processed.imp$ximp[dataset.processed.na]
      dataset.processed <- dataset.processed[,-ncol(dataset.processed)]
      cache$imp <- dataset.processed
    } else {
      
      # cat("Pass 2/2", "\n")
      if(any(dataset.processed.na)) {
        imp <- cache$imp
        
        dataset.combined <- rbind(imp, dataset.processed)
        dataset.combined.imp <- missForest(dataset.combined)
        
        dataset.processed[,dataset.processed.na] <- dataset.combined.imp$ximp[(nrow(imp) + 1):(nrow(dataset.combined)), dataset.processed.na]
      }
    }
  }
  
  # Attributes are converted to numeric
  for (i in 1:ncol(dataset.processed)) {
    if(!is.numeric(dataset.processed[,i])) {
      dataset.processed[,i] <- as.numeric(dataset.processed[,i])
    }
  }
  
  dataset.processed$Players <- players.train
  dataset.processed$Class <- class.train
  
  # There are way more games with low number of players. Therefore, games with higher numbers are repeated
  if(type == "train") {
    cat("Adjusting class distribution in training data...", "\n")
    
    classes <- lapply(c(1:length(levels(dataset.processed$Class))), function(x) dataset.processed[dataset.processed$Class == x,])
    dataset.processed.tmp <- classes[[1]]
    
    for (i in 2:length(levels(dataset.processed$Class))) {
      dataset.processed.tmp <- rbind(dataset.processed.tmp, classes[[i]][sample(nrow(classes[[i]]), nrow(classes[[i]]) * (i - 1), replace = T),])
    }
    dataset.processed <- dataset.processed.tmp
  }
  
  cat("Finishing...", "\n")
  
  dataset.processed <- dataset.processed[,-(ncol(dataset.processed) - mode)]
  
  if(type == "train") {
    save(cache, file = "./DataProcess/Data/cache.RData")
  }
  
  if(mode == 0) {
    if(type == "train") {
      dataset.processed.name <- paste("./DataProcess/Datasets/dataset.train.reg",".RData",sep = "")
      dataset.train <- dataset.processed
      save(dataset.train, file = dataset.processed.name)
      return(NULL)
    }
    if(type == "val") {
      dataset.processed.name <- paste("./DataProcess/Datasets/dataset.val.reg",".RData",sep = "")
      dataset.val <- dataset.processed
      save(dataset.val, file = dataset.processed.name)
      return(NULL)
    }
    if(evaluation && type == "test") {
      dataset.processed.name <- paste("./DataProcess/Datasets/dataset.test.reg",".RData",sep = "")
      dataset.test <- dataset.processed
      save(dataset.test, file = dataset.processed.name)
      return(NULL)
    }
    if(!evaluation && type == "test") {
      dataset.processed.name <- paste("./DataProcess/Datasets/dataset.new.reg",".RData",sep = "")
      dataset.new <- dataset.processed
      return(dataset.new)
    }
  } else {
    if(type == "train") {
      dataset.processed.name <- paste("./DataProcess/Datasets/dataset.train.class",".RData",sep = "")
      dataset.train <- dataset.processed
      save(dataset.train, file = dataset.processed.name)
      return(NULL)
    }
    if(type == "val") {
      dataset.processed.name <- paste("./DataProcess/Datasets/dataset.val.class",".RData",sep = "")
      dataset.val <- dataset.processed
      save(dataset.val, file = dataset.processed.name)
      return(NULL)
    }
    if(evaluation && type == "test") {
      dataset.processed.name <- paste("./DataProcess/Datasets/dataset.test.class",".RData",sep = "")
      dataset.test <- dataset.processed
      save(dataset.test, file = dataset.processed.name)
      return(NULL)
    }
    if(!evaluation && type == "test") {
      dataset.processed.name <- paste("./DataProcess/Datasets/dataset.new.class",".RData",sep = "")
      dataset.new <- dataset.processed
      return(dataset.new)
    }
  }
  
  cat("Done!", "\n\n")
}
