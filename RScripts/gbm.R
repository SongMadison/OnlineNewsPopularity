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
##    Fit boosting
##################################################################################################
library(gbm)
## Use adaboost loss (exponential loss) with additive model. Conduct 10-fold CV
fit.gbm = gbm(y.tr ~ ., n.trees=10000, distribution='adaboost', 
              interaction.depth = 1, cv.folds=10, data=newx.tr) 

## Output the cv-errors.  This is NOT misclassificaiton error
fit.gbm$cv.error 


## Find the best iteration number
## gbm response must (0,1)
best.iter <- gbm.perf(fit.gbm, method="cv") 

## score predicted by gbm
score.gbm <- predict(fit.gbm, x.te, n.trees = best.iter )
write.csv(data.frame(score.gbm), "./Results/gbmScore.csv")

## Prediction on test set
pred = (predict(fit.gbm, x.te, n.trees=best.iter)>0)*TRUE 

## Calculate misclassification error on test data
mean(pred==y.te) 

## report the importance score of variables
summary(fit.gbm)


