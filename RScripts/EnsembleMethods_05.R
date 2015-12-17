rm(list =ls())

RScriptPath <- paste0("./RScripts/")
PlotsPath <- paste0("./Plots/")
DataPath <- paste0("./Data/")

source("./RScripts/head_Files.R")
source("./RScripts/fn_Library.R")
########################################################################


setwd("~/Stat/Courses/OnlineNewsPopularity/")

load("./Results/results_30000_1.RData")
load("./Data/News_std.RData")
neural <- read.csv("./Results/neural.txt",header= T)


load("./Results/results_30000_1.RData")
load("./Data/News_std.RData")
load("./Results/svmScores.RData")
neural <- read.csv("./Results/neural.txt",header= T)

aucScore$neural <- auc(roc(probability$neural,as.factor(y.te)))
aucScore$svm <- auc(roc(probability$svm,as.factor(y.te)))
accuracyM(probability$svm>=0.5,y.te)


probability$neural <- neural[,2]
probability$svm <- attr(pred3.svm,"probabilities")[,2]
str(probability)
prob <- data.frame(probability)
prob$gbm <- 1/(1+exp(-prob$gbm))
str(prob)

EnsembleClassifier1 <- rowMeans(prob)
plot(roc1 <- roc(EnsembleClassifier1,as.factor(y.te)))
auc(roc1)
EnsembleClassifier2 <- as.matrix(prob) %*% matrix(unlist(aucScore),ncol=1) 
plot(roc2<- roc(EnsembleClassifier2,as.factor(y.te)))
auc(roc2)

EnsembleClassifier2 <- apply(prob, MARGIN = 1, median)
plot(roc2<- roc(EnsembleClassifier2,as.factor(y.te)))
auc(roc2)

#majority vote
EnsembleClassifier3 <- apply(prob>0.5, MARGIN = 1, median)
mean(EnsembleClassifier3==y.te)


