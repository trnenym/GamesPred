#Evaluation of classification of Steam games descriptions
#Note: set paths accordingly before use

setwd("C:/GamesPred/desc.inf/eval")
library(RWeka)
library(RTextTools)
library(NLP)
library(tm)
library(openNLP)
library(caret)
library(ROCR)
library(SnowballC)

#function used for stemming and toLowercase
stemstr <- function(string) {
  string <- tolower(string)
  #result <- string
  tokens <- MC_tokenizer(string)
  result <- paste(wordStem(tokens), collapse = " ")
  result <- gsub("\\s+", " ", result)
  return (result)
}

#dataset containing descriptions and class attribute
data <- read.arff("desc.inf.text.arff")

data$class_attr <- data$class
data <- data[-c(2)]

i0 <- subset(data, class_attr == 0)
i1 <- subset(data, class_attr == 1)

k <- 8
l <- nrow(i0) / k

results.predicted <- data.frame(matrix(NA, nrow = l*2, ncol = k))
results.actual <- data.frame(matrix(NA, nrow = l*2, ncol = k))

#k-fold cross-validation
for(i in 1:k) {
  t0 <- i0[(i*l-(l-1)):(i*l),]
  t1 <- i1[(i*l-(l-1)):(i*l),]
  test <- rbind(t0, t1)
  
  if(i == 1) {
    s1 <- i1[c((i*l+1):128),]
    tmp <- i0[c((i*l+1):72),]
  } else {
    if(i == k) {
      s1 <- i1[c(1:((i-1)*l), (i*l+1):128),]
      tmp <- i0[c(1:((i-1)*l)),]
    } else {
      s1 <- i1[c(1:((i-1)*l), (i*l+1):128),]
      tmp <- i0[c(1:((i-1)*l), (i*l+1):72),]
    }
  }
  
  s0 <- tmp
  train <- rbind(s0, s0, s0, s1)
  
  #stemming
  train$desc <- sapply(train$desc, stemstr)
  test$desc <- sapply(test$desc, stemstr)
  
  write.arff(train, file = "./desc.inf.text.200.train.text.arff")
  
  #creating training document-term matrix
  system('java -classpath "C:\\Program Files (x86)\\Weka-3-6\\weka.jar" weka.filters.unsupervised.attribute.StringToWordVector -R first -W 50 -prune-rate -1.0 -C -N 0 -stemmer weka.core.stemmers.NullStemmer -M 2 -tokenizer "weka.core.tokenizers.WordTokenizer -delimiters \\" \\r\\n\\t.,;:\\\'\\\"()?!\\"" -i "C:\\Temp\\desc.inf.text.200.train.text.arff" -o "C:\\Temp\\desc.inf.text.200.train.tdm.arff"', wait = T)
  
  train.tdm <- read.arff("desc.inf.text.200.train.tdm.arff")
  train.cls_attr <- train.tdm$class_attr
  train.tdm <- train.tdm[,-1]
  train.tdm <- train.tdm[,order(names(train.tdm))]
  
  train.tdm.tm <- as.TermDocumentMatrix(t(train.tdm), weighting = function(x) x)
  
  vs <- VectorSource(test$desc)
  corpus <- VCorpus(vs)
  #test DTM must use the same terms
  test.tdm.tm <- TermDocumentMatrix(corpus, control = list(dictionary=Terms(train.tdm.tm)))
  
  test.tdm <- as.data.frame(as.matrix(t(test.tdm.tm)))
  
  #PCA
#   train.matrix <- as.matrix(train.tdm)
#   pca <- princomp (train.matrix, cor = TRUE, scores = TRUE)
#   
#   #   screeplot (pca, npcs = ncol(train.matrix), las = 3, main = "Scree plot")
#   #   
#   #   plot (1:ncol(train.matrix), cumsum (pca$sdev**2) / ncol(train.matrix), type = "b", ylim = c(0.0, 1.0), xlab = "r", ylab = "variability fraction")
#   #   abline(h = 0.8)
#   
#   loadings <- pca$loadings[,1:50]
#   train.pca <- train.matrix %*% loadings
#   test.pca <- as.matrix(test.tdm) %*% loadings
#   
#   train.tdm <- as.data.frame(train.pca)
#   test.tdm <- as.data.frame(test.pca)
  
  
  test.tdm$cls_attr <- test$class_attr
  train.tdm$cls_attr <- train.cls_attr
  
  write.arff(train.tdm, file = "./desc.inf.text.200.train.arff")
  write.arff(test.tdm, file = "./desc.inf.text.200.test.arff")
  
  #calling Naive Bayes Multinomial from Weka
  results <- system('java -classpath "C:\\Program Files (x86)\\Weka-3-7\\weka.jar" weka.classifiers.bayes.NaiveBayesMultinomial -t "C:\\Temp\\desc.inf.text.200.train.arff" -T "C:\\Temp\\desc.inf.text.200.test.arff" -classifications "weka.classifiers.evaluation.output.prediction.CSV"', intern = T, wait = T)
  
  #processing the results
  write(results[5:length(results)-1], file = "./desc.inf.text.200.results.csv")
  results.table <- read.csv2("./desc.inf.text.200.results.csv", sep = ",")
  results.table$actual <- as.numeric(results.table$actual) - 1
  results.table$predicted <- as.numeric((results.table$predicted)) - 1
  
  results.predicted[,i] <- results.table$predicted
  results.actual[,i] <- results.table$actual
}

results.all <- data.frame(matrix(NA, nrow = k+1, ncol = 10))

#generating metrics
for(j in 1:k) {
  metrics <- list()
  
  metrics <- c(metrics, accuracy = nrow(results.actual[results.actual[,j] == results.predicted[,j],]) / nrow(results.table))
  
  cm <- confusionMatrix(results.predicted[,j], reference = results.actual[,j], positive = "1")
  cm.table <- as.data.frame(cm$table)
  TP <- cm.table[4,3]
  FP <- cm.table[2,3]
  FN <- cm.table[3,3]
  TN <- cm.table[1,3]
  
  metrics <- c(metrics, precision_1 = precision1 <- TP / (TP + FP))
  metrics <- c(metrics, recall_1 = recall1 <- TP / (TP + FN))
  metrics <- c(metrics, fscore_1 = fscore1 <- (2 * precision1 * recall1) / (precision1 + recall1))
  
  metrics <- c(metrics, precision_0 = precision0 <- TN / (TN + FN))
  metrics <- c(metrics, recall_0 = recall0 <- TN / (TN + FP))
  metrics <- c(metrics, fscore_0 = fscore0 <- (2 * precision0 * recall0) / (precision0 + recall0))
  
  metrics <- c(metrics, precision_avg = precision.avg <- (precision1 * (TP+FN) + precision0 * (FP+TN)) / (2*l))
  metrics <- c(metrics, recall_avg = recall.avg <- (recall1 * (TP+FN) + recall0 * (FP+TN)) / (2*l))
  metrics <- c(metrics, fscore_avg = fscore.avg <- (fscore1 * (TP+FN) + fscore0 * (FP+TN)) / (2*l))
  
  results.all[j,] <- metrics
}

names(results.all) <- names(metrics)
results.all[k+1,] <- colMeans(results.all[1:k,])

results.means <- round(results.all[k+1,],2)
results.means

