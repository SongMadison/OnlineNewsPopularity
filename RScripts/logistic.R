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
## Logistic Regression 
####################################################################################

fit.glm <- glm(as.factor(y.tr) ~ . ,
               family = binomial(link = "logit"), data =newx.tr)

predicted.tr <- (predict(fit.glm, type="response")>0.5)*1
hist(predict(fit.glm,type="link")) # log (odd ratio)
(confusionM.tr <- table(y.tr, predicted.tr))
mean(y.tr==predicted.tr)

mean(y.te==(predict(fit.glm,newdata = x.te,type="response")>0.5))
pred.glm <- predict(fit.glm,newdata = x.te,type="response")

auc(roc.logit.tr <- roc(fit.glm$fitted,as.factor(y.tr)))
plot(roc.logit.tr, as.factor(y.tr))
auc(roc.glm.te <- roc( predict(fit.glm,newdata = x.te,type = "response") ,as.factor(y.te) ))
plot(roc.glm.te)



#####################################################################################
## Fit LASSO-logistic regression
#####################################################################################
## Select lambda using 10-fold CV
library(glmnet)

cv.lasso = cv.glmnet(as.matrix(x.tr), as.matrix(y.tr), family='binomial', nfolds=10)
lambda.min = cv.lasso$lambda.min
lambda.1se = cv.lasso$lambda.1se ## Result in a sparser model

## Fit the final model with lambda.min
fit.min = glmnet(as.matrix(x.tr), as.matrix(y.tr), family='binomial', lambda=lambda.min)
fit.min$a0 ## intercept
fit.min$beta ## betahat in logistic regression
pred.min = predict(fit.min, as.matrix(x.te), type='response')

mean((pred.min>0.5)==y.te)

## Fit the final model with lambda.1se
fit.1se = glmnet(as.matrix(x.tr), as.matrix(y.tr), family='binomial', lambda=lambda.1se)
fit.1se$a0 ## intercept
fit.1se$beta ## betahat in logistic regression
pred.lse = predict(fit.1se, as.matrix(x.te), type='response')
mean((pred.lse>0.5)==y.te)

res <- list(pred.glm = pred.glm, pred.min =pred.min, pred.lse =pred.lse)
write.csv(data.frame(res), file="./Results/logisticScores.csv")