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



####################################################################################
## random Forest
####################################################################################
library(randomForest)
## Try different mtryStart values manually
set.seed(111)
tune.rf = tuneRF(x=x.tr, 
                 y=as.factor(y.tr), ntree=1000, mtryStart=4, stepFactor=2,
                 improve=0.05, nodesize=1, trace=T, plot=T,doBest=T)

fit.rf <- randomForest(as.factor(y.tr)~.,  mtry =8, ntree =1000, nodesize = 1,
                       data = newx.tr)
pred.rf1 <- predict(fit.rf,newdata = x.te,type="prob")[,2]
mean((pred.rf1>.5)==y.te )


## Get the variable importance score
varimp = varImpPlot(fit.rf,type=2)


fit.rf <- randomForest(as.factor(y.tr)~.,  mtry =8, ntree =1000, nodesize = 3,
                       data = newx.tr)
pred.rf2 <- predict(fit.rf,newdata = x.te,type="prob")[,2]
mean((pred.rf2>.5)==y.te )
## Get the variable importance score
varimp = varImpPlot(fit.rf,type=2)

write.csv(data.frame(cbind(pred.rf1,pred.rf2)), file = "./Results/rfScore.csv")