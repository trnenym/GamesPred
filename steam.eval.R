#evaluation of models

#note: adjust paths before use
setwd("C:/GamesPred")
library(RWeka)
library(Metrics)
library(caret)
library(plotrix)

#pick one
results <- system('java -classpath "C:\\Program Files (x86)\\Weka-3-7\\weka.jar" weka.classifiers.trees.RandomForest -I 100 -K 0 -S 1 -num-slots 1 -t "C:\\GamesPred\\steam.data.log.arff" -classifications "weka.classifiers.evaluation.output.prediction.CSV"', intern = T, wait = T)
#results <- system('java -classpath "C:\\Program Files (x86)\\Weka-3-7\\weka.jar" weka.classifiers.functions.LinearRegression -S 0 -R 1.0E-8 -num-decimal-places 4 -t "C:\\GamesPred\\steam.data.log.arff" -classifications "weka.classifiers.evaluation.output.prediction.CSV"', intern = T, wait = T)
#results <- system('java -classpath "C:\\Program Files (x86)\\Weka-3-7\\weka.jar" weka.classifiers.functions.SMOreg -C 1.0 -N 0 -I "weka.classifiers.functions.supportVector.RegSMOImproved -T 0.001 -V -P 1.0E-12 -L 0.001 -W 1" -K "weka.classifiers.functions.supportVector.PolyKernel -E 1.0 -C 250007" -t "C:\\GamesPred\\steam.data.log.arff" -classifications "weka.classifiers.evaluation.output.prediction.CSV"', intern = T, wait = T)

#process results
write(results[5:length(results)-1], file = "weka.results.csv")
results.table <- read.csv2("weka.results.csv", sep = ",")
actual <- as.numeric(levels(results.table$actual))[results.table$actual]
predicted <- as.numeric(levels(results.table$predicted))[results.table$predicted]

#basic metrics
rmse(actual, predicted)
mae(actual, predicted)
cor(actual, predicted, method = "pearson")

#plot the results
table <- data.frame(actual = actual, predicted = predicted)
table <- table[order(table$actual),]

plot(table$predicted, col = "red", ylab = "log2(players+1)", xlab = "games", ylim = c(-1,17))
points(table$actual, col = "blue")
# points(table$actual+1.5, col = "green", cex = 0.2)
# points(table$actual-1.5, col = "green", cex = 0.2)

#percentage of accurately covered games, depends on the definition of "accurately"
x <- 1.5
nrow(subset(table, predicted >= (actual-x) & predicted <= (actual+x)))/nrow(table)
