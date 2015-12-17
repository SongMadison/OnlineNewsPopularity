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

accuracy<- list()
aucScore <- list()
probability <- list()
####################################################################################
## Logistic Regression 
####################################################################################

fit.glm <- glm(as.factor(y.tr) ~ . ,
               family = binomial(link = "logit"), data =newx.tr)

predicted.tr <- (predict(fit.glm, type="response")>0.5)*1
hist(predict(fit.glm,type="link")) # log (odd ratio)
(confusionM.tr <- table(y.tr, predicted.tr))
mean(y.tr==predicted.tr)

mean(y.te==(predict(fit.glm,newdata = x.te,type="response")>0.5))

auc(roc.logit.tr <- roc(fit.glm$fitted,as.factor(y.tr)))
plot(roc.logit.tr, as.factor(y.tr))

y.prob <- predict(fit.glm,newdata = x.te,type = "response")
probability$glm <- y.prob

(aucScore$glm <- auc( roc.glm.te <- roc( y.prob ,as.factor(y.te) )) )

(accuracy$glm <- unlist(accuracyM( (y.prob>0.5)*1, y.te )))


#####################################################################################
## Fit LASSO-logistic regression
#####################################################################################
## Select lambda using 10-fold CV
library(glmnet)
cv.lasso = cv.glmnet(as.matrix(x.tr), as.matrix(as.factor(y.tr)), family='binomial', nfolds=10)
lambda.min = cv.lasso$lambda.min
lambda.1se = cv.lasso$lambda.1se ## Result in a sparser model

## Fit the final model with lambda.min
fit.min = glmnet(as.matrix(x.tr), as.matrix(y.tr), family='binomial', lambda=lambda.min)
fit.min$a0 ## intercept
fit.min$beta ## betahat in logistic regression
pred.min = predict(fit.min, as.matrix(x.te), type='response')
probability$glmnet <- pred.min
(aucScore$glmnet <- auc( roc(pred.min ,as.factor(y.te) )) )
(accuracy$glmnet <- unlist(accuracyM((pred.min>0.5)*1,y.te)))


## Fit the final model with lambda.1se
fit.1se = glmnet(as.matrix(x.tr), as.matrix(y.tr), family='binomial', lambda=lambda.1se)
fit.1se$a0 ## intercept
fit.1se$beta ## betahat in logistic regression
pred.1se = predict(fit.1se, as.matrix(x.te), type='class')
mean(pred.1se==y.te)

####################################################################################
## random Forest
####################################################################################
library(randomForest)
## Try different mtryStart values manually
set.seed(111)
tune.rf = tuneRF(x=x.tr, y=as.factor(y.tr), ntree=1000, mtryStart=4, stepFactor=2,
                 improve=0.05, nodesize=1, trace=T, plot=T,doBest=T)

fit.rf <- randomForest(as.factor(y.tr)~.,  mtry =8, ntree =1000, nodesize = 1,
                       data = newx.tr)
pred.rf <- predict(fit.rf,newdata = x.te,type="prob")
mean((pred.rf[,2]>.5)==test$shares )
#10 ,500 , 3  0.3363749
probability$rf <- pred.rf [,2]
(aucScore$rf <- auc(roc( probability$rf,as.factor(y.te) )) )
(accuracy$rf <- unlist(accuracyM((pred.rf[,2]>0.5)*1,y.te)))
## Get the variable importance score
varimp = varImpPlot(fit.rf,type=2)


library(rpart)
fit.rpart <- rpart(as.factor(y.tr) ~ ., method="class", 
                   control = rpart.control(minsplit=10, cp=0.001),data=newx.tr)
xmat <- xpred.rpart(fit.rpart)
xmat <- 1*(xmat==2)
cv.rpart <- apply (X = xmat, MARGIN = 2,function(x) mean(x==y.tr))
cps <- as.numeric(colnames(xmat))
(cp.max <- cps[which(cv.rpart == max(cv.rpart))])

#“vector”, “prob”, “class”, “matrix”
fit.rpart <- rpart(as.factor(y.tr) ~ ., method="class", 
                   control = rpart.control(minsplit=10, cp=cp.max),data=newx.tr)
probability$rpart <- predict(fit.rpart,newdata = x.te,type = "prob")[,2]
(aucScore$rpart <- auc(roc( probability$rpart,as.factor(y.te) )) )
(accuracy$rpart <- unlist(accuracyM(1*(probility$rpart>0.5), y.true =y.te)))


##################################################################################################
##    Fit boosting
##################################################################################################
library(gbm)
## Use adaboost loss (exponential loss) with additive model. Conduct 10-fold CV
fit.gbm = gbm(y.tr ~ ., n.trees=10000, distribution='adaboost', interaction.depth = 1, cv.folds=10, data=newx.tr) 

## Output the cv-errors.  This is NOT misclassificaiton error
fit.gbm$cv.error 

## Find the best iteration number
best.iter <- gbm.perf(fit.gbm, method="cv") 

## Prediction on test set
pred = (predict(fit1.gbm, x.te, n.trees=best.iter)>0)*TRUE 

## Calculate misclassification error on test data
mean(pred==y.te) 

## report the importance score of variables
summary(fit.gbm)

probability$gbm <- predict(fit.gbm, x.te, n.trees=best.iter)
(aucScore$gbm <- auc(roc( probability$gbm,as.factor(y.te) )) )
(accuracy$rpart <- unlist(accuracyM(1*(probability$gbm>0), y.true =y.te)))


####### KNN Method

library(class)
#select k
knn.cv1 <- knn.cv(train = x.tr, cl= as.factor(y.tr), k = 5, l = 0, prob = FALSE, use.all = TRUE)

fit.knn<- knn(train = x.tr, test =x.te, cl =y.tr, k = 5, l = 0, prob = TRUE, use.all = TRUE)
prob <- numeric(length(y.te))
prob[which(fit.knn=="1")]=attr(fit.knn,"prob")[which(fit.knn=="1")]
prob[which(fit.knn=="0")]=1-attr(fit.knn,"prob")[which(fit.knn=="0")]
mean((1*(prob>0.5))==y.te)

write.csv(data.frame(prob), file="./Results/knnPred.csv")
probability$knn <- prob
(aucScore$knn <- auc(roc( probability$knn,as.factor(y.te) )) )
(accuracy$knn <- unlist(accuracyM(prob>0.5, y.true =y.te)))

save(probability,accuracy,aucScore, file="./Results/results.RData")
