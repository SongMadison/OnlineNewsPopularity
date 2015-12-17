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



####### KNN Method

library(class)

fit.knn5<- knn(train = x.tr, test =x.te, cl = y.tr, k = 5, l = 0, prob = FALSE, use.all = TRUE)
mean(fit.knn5==y.te)
fit.knn10<- knn(train = x.tr, test =x.te, cl = y.tr, k = 10, l = 0, prob = FALSE, use.all = TRUE)
mean(fit.knn10==y.te)
knn.cv1 <- knn.cv(train = x.tr, cl= as.factor(y.tr), k = 1, l = 0, prob = FALSE, use.all = TRUE)

write.csv(data.frame(rbind(fit.knn5,fit.knn10)), file="./Results/knnPred_5_10.csv")