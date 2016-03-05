#preparation of dataset for training in Weka

setwd("C:/GamesPred")
library(RWeka)

#required data files
dataset <- read.arff("steam.arff")
desc.inf <- read.arff("./desc.inf/desc.all.result.arff")

#Players is the predicted attribute
players <- dataset$Players

#keep only neccessary attributes
#as.data.frame(colnames(dataset))
dataset.temp <- dataset[,-c(1,2,3,4,5,8,24,26,27,49,50,51)]
dataset.temp$DescIsInf <- desc.inf$DescIsInf
dataset.temp$Players <- players

#raw players numbers are unevenly distributed, log2 provides more readable results and actually improves accuracy
dataset.temp$Players <- log(dataset.temp$Players + 1, base = 2)
dataset.temp$Players <- round(dataset.temp$Players,2)

dataset <- dataset.temp

#the final dataset
write.arff(dataset, file = "steam.data.log.arff")
