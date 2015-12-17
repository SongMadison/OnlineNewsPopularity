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


#####################################tree algorithm






library(tree)
## Fit classification trees
newx.tr = data.frame(cbind(y.tr, x.tr))
newx.te = data.frame(cbind(y.te, x.te))
fit.tree = tree(as.factor(y.tr)~.,  data=newx.tr)
summary(fit.tree)
plot(fit.tree)
text(fit.tree)
pred1.tree = predict(fit.tree,newx.te,type="class")
mean(pred1.tree!=y.te)

## Prune the fitted tree
cvtree = cv.tree(fit.tree ,FUN=prune.misclass )
cvtree
prunedtree = prune.misclass(fit.tree, best=4)   
summary(prunedtree)
plot(prunedtree)
text(prunedtree)
pred2.tree = predict(prunedtree,newx.te,type="class")
mean(pred2.tree!=y.te)


library(rpart)
fit.rpart <- rpart(as.factor(y.tr) ~ ., method="class", 
                   control = rpart.control(minsplit=10, cp=0.001),data=newx.tr)
xmat <- xpred.rpart(fit.rpart)
xmat <- 1*(xmat==2)
cv.rpart <- apply (X = xmat, MARGIN = 2,function(x) mean(x==y.tr))
which(cv.rpart == max(cv.rpart))
#“vector”, “prob”, “class”, “matrix”
pred.rpart <- predict(fit.rpart,newdata = x.te,type = "prob")[,2] 
write.csv(pred.rpart, file="./Results/pred.rpart.csv")
mean(y.te==(1*(pred.rpart>0.5)))
