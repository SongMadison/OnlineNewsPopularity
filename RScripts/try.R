
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
write.csv(newx.tr,"./Data/train_std.csv")
write.csv(newx.te,"./Data/test_std.csv")


save(x.tr,y.tr,x.te,y.te, file ="./Data/News_std.RData")

load("./Results/results_20000_1.RData")
aucScore
accuracy


plot(roc(probability$glm, as.factor(y.te)))

roc.rf <- roc(probability$rf, as.factor(y.te))
points(roc.rf$fpr, roc.rf$tpr,pch=".", col="red" )

roc.glmnet<- roc(probability$glmnet, as.factor(y.te))
points(roc.glmnet$fpr, roc.glmnet$tpr,pch=".", type= "b",col="cyan" )

roc.knn<- roc(probability$knn, as.factor(y.te))
points(roc.knn$fpr, roc.knn$tpr,pch=".", type ="b",col="green" )

roc.gbm<- roc(probability$gbm, as.factor(y.te))
points(roc.gbm$fpr, roc.gbm$tpr,pch=".", type= "b",col="darkgreen" )

roc.rpart<- roc(probability$rpart, as.factor(y.te))
points(roc.rpart$fpr, roc.rpart$tpr, pch=".", type= "b",col="darkblue")




xtable(t(accu))
xtable(accu)



accuracy
