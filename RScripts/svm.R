
rm(list =ls())

RScriptPath <- paste0("./RScripts/")
PlotsPath <- paste0("./Plots/")
DataPath <- paste0("./Data/")

source("./RScripts/head_Files.R")
source("./RScripts/fn_Library.R")


#####################################################################################


train <- read.csv("./Data/train.csv")
test <- read.csv("./Data/test.csv")

train <- train[,-1]
test <- test[,-1]
train$shares <- ( train$shares>1400)*1.0 
test$shares <-  ( test$shares>1400)*1.0 
(table(train$shares)/nrow(train))
(table(test$shares)/nrow(test))


dim(train)
dim(test)
x.tr <- train[-ncol(train)]
y.tr <- train[,ncol(train)]
x.te <- test[-ncol(test)]
y.te <- test[,ncol(test)]


x.transformed <- DataStandardization(x.tr,x.te)
x.tr <- x.transformed$train
x.te <- x.transformed$test
newx.tr = data.frame(cbind(y.tr, x.tr))
newx.te = data.frame(cbind(y.te, x.te))



##################################################################################################
## Fit SVM
##################################################################################################
library(e1071)
## Select the best cost parameter with linear kernel

#tune1 = tune(svm, train.x=as.matrix(x.tr), train.y=as.factor(y.tr), kernel='linear',
#             range=list(cost=2^seq(-5,5,length=10)), 
#             control=tune.control(sampling='cross', cross=10)
#)

## Fit the "optimal" SVM
fit1.svm = svm(x.tr[1:1000,], as.factor(y.tr[1:1000]), kernel='linear', cost=1,probability = T)

## Prediction on test data
pred1.svm = predict(fit1.svm, x.te, probability = T) ## predict probability
pred1.svm1 = predict(fit1.svm,x.te, probability = FALSE) ## predict class
mean(pred1.svm1==y.te) 

## Fit the "optimal" SVM
fit1.svm = svm(x.tr[1:10000,], as.factor(y.tr[1:10000]), kernel='linear', cost=1,probability = T)

## Prediction on test data
pred2.svm = predict(fit1.svm, x.te, probability = T) ## predict probability
pred2.svm1 = predict(fit1.svm,x.te, probability = FALSE) ## predict class
mean(pred2.svm1==y.te)  ## 0.627

# Fit the "optimal" SVM
fit1.svm = svm(x.tr, as.factor(y.tr), kernel='linear', cost=1,probability = T)

## Prediction on test data
pred3.svm = predict(fit1.svm, x.te, probability = T) ## predict probability
pred3.svm1 = predict(fit1.svm,x.te, probability = F) ## predict class
mean(pred3.svm1==y.te) 
mean((attr(pred3.svm,"probabilities")[,2]>=0.5)==y.te)  #= 0.641

save(pred1.svm,pred2.svm,pred3.svm,file="./Results/svmScores.RData")
## Select the best cost parameter with radial kernel (Gaussian kernel)
## The tuning can take a long time, because there are two parameters to tune.
# tune2 = tune(svm, train.x=as.matrix(x.tr), 
#              train.y=as.factor(y.tr), kernel='radial', range=list(cost=2^seq(-5:5), gamma=2^(-5:5)), 
#              control=tune.control(sampling='cross', cross=10))
# fit2.svm =  svm(x.tr, as.factor(y.tr), kernel='radial', cost=2, gamma=0.03125)
# pred2.svm = predict(fit2.svm, x.te)
# mean(pred2.svm!=y.te) 
