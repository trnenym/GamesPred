#Classification of descriptions
#Note: set paths accordingly before use
setwd("C:/GamesPred/desc.inf")
library(RWeka)
library(RTextTools)
library(NLP)
library(tm)
library(openNLP)

#function used for stemming and toLowercase
stemstr <- function(string) {
  string <- tolower(string)
  tokens <- MC_tokenizer(string)
  result <- paste(wordStem(tokens), collapse = " ")
  result <- gsub("\\s+", " ", result)
  return (result)
}

#preprocessing training data
# train.data <- read.arff("desc.inf.text.arff")
# train.data$desc <- sapply(train.data$desc, stemstr)
# write.arff(train.data, file = "desc.inf.text.arff")

#preprocessing predicted data
# dataset <- read.arff("../steam.arff")
# 
# for(i in 1:nrow(dataset)) {
#   d <- dataset[i,3]
#   l <- nchar(d)
#   if(l < 400) {
#     dataset[i,3] <- substr(d, 1, l)
#   }
#   else {
#     j <- 400
#     while(unlist(strsplit(d,""))[j] != " ") {
#       j <- j - 1
#     }
#     dataset[i,3] <- substr(d, 1, j)
#   }
# }
# 
# dataset$Description <- sapply(dataset$Description, stemstr)
# write.arff(dataset$Description, file = "desc.all.arff")

#create training document-term matrix
system('java -classpath "C:\\Program Files (x86)\\Weka-3-6\\weka.jar" weka.filters.unsupervised.attribute.StringToWordVector -R first -W 50 -prune-rate -1.0 -C -N 0 -stemmer weka.core.stemmers.NullStemmer -M 1 -tokenizer "weka.core.tokenizers.WordTokenizer -delimiters \\" \\r\\n\\t.,;:\\\'\\\"()?!\\"" -i "C:\\GamesPred\\desc.inf\\desc.inf.text.arff" -o "C:\\GamesPred\\desc.inf\\desc.inf.tdm.arff"', wait = T)
train.tdm <- read.arff("desc.inf.tdm.arff")

negative <- subset(train.tdm, class == 0)
train.tdm <- rbind(train.tdm, negative)
train.tdm <- train.tdm[sample(nrow(train.tdm), size = nrow(train.tdm), replace = F),]

cls_attr <- train.tdm$class
train.tdm <- train.tdm[,-1]
train.tdm <- train.tdm[,order(names(train.tdm))]
train.tdm.tm <- as.TermDocumentMatrix(t(train.tdm), weighting = function(x) x)

#create document-term matrix for predicted data
pred.data <- read.arff("desc.all.arff")
vs <- VectorSource(pred.data$x)
corpus <- VCorpus(vs)
pred.tdm.tm <- TermDocumentMatrix(corpus, control = list(dictionary=Terms(train.tdm.tm)))

pred.tdm <- as.data.frame(as.matrix(t(pred.tdm.tm)))
pred.tdm$cls_attr <- factor(0)
levels(pred.tdm$cls_attr) <- c(0,1)
train.tdm$cls_attr <- cls_attr

write.arff(train.tdm, file = "desc.inf.train.arff")

#actual predictions
results.predicted <- c()

for(i in 1:nrow(pred.tdm)) {
  row <- subset(pred.tdm[i,])
  
  write.arff(row, file = "desc.inf.pred.arff")
  
  results <- system('java -classpath "C:\\Program Files (x86)\\Weka-3-7\\weka.jar" weka.classifiers.bayes.NaiveBayesMultinomial -t "C:\\GamesPred\\desc.inf\\desc.inf.train.arff" -T "C:\\GamesPred\\desc.inf\\desc.inf.pred.arff" -classifications "weka.classifiers.evaluation.output.prediction.CSV"', intern = T, wait = T)
  write(results[5:length(results)-1], file = "desc.inf.results.csv")
  results.table <- read.csv2("desc.inf.results.csv", sep = ",")
  
  if(results.table$predicted == "2:1") {
    results.predicted <- c(results.predicted, 1)
  } else {
    results.predicted <- c(results.predicted, 0)
  }
  print(i)
}

#final result
results.predicted <- data.frame(DescIsInf = results.predicted)
results.predicted$DescIsInf <- as.logical(results.predicted$DescIsInf)
write.arff(results.predicted, file = "desc.all.result.arff")
