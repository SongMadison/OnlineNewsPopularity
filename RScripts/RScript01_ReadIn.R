
rm(list =ls())

RScriptPath <- paste0("./RScripts/")
PlotsPath <- paste0("./Plots/")
DataPath <- paste0("./Data/")

source("./RScripts/head_Files.R")
source("./RScripts/fn_Library.R")
library(AUC)

news = read.csv(file = paste0(DataPath,"OnlineNewsPopularity.csv"), header = TRUE)
trainIdx <- sample(1:nrow(news), 30000) # 30000
testIdx <- (1:nrow(news))[-trainIdx]  # 9644
train <- news[trainIdx,]
test <- news[testIdx,]


# combining some the factor variable into a factor variable 
## Caution !! some methods doesn't accept factor variable.  Using the given coding for
#  factor variable is good in this sense!!

# train <- PrepareData(train)
# test <- PrepareData(test)
# 
# write.csv(train, paste0(DataPath,"train.csv"))
# write.csv(test, paste0(DataPath,"test.csv"))



train.classification <- train[,-1]
test.classification <- test[,-1]
train.classification$shares <- ( train.classification$shares>1400)*1.0 
test.classification$shares <-  ( test.classification$shares>1400)*1.0 
(table(train.classification$shares)/nrow(train.classification))
(table(test.classification$shares)/nrow(test.classification))


dim(train.classification)
dim(test.classification)
x.tr <- train.classification[-ncol(train.classification)]
y.tr <- train.classification[,ncol(train.classification)]
x.te <- test.classification[-ncol(test.classification)]
y.te <- test.classification[,ncol(test.classification)]
rm(train, test, news)

####################################################################################
## Logistic Regression 
####################################################################################
fit.glm <- glm(as.factor(train.classification$shares) ~ . ,
                   family = binomial(link = "logit"), data =train.classification)

predicted.tr <- (predict(fit.glm, type="response")>0.5)*1
hist(predict(fit.glm,type="link")) # log (odd ratio)
(confusionM.tr <- table(y.tr, predicted.tr))
mean(y.tr==predicted.tr)

mean(y.te==(predict(fit.glm,newdata = x.te,type="response")>0.5))

library(AUC)
auc(roc.logit.tr <- roc(fit.glm$fitted,as.factor(y.tr)))
plot(roc.logit.tr, as.factor(y.tr))
auc( roc.glm.te <- roc( predict(fit.glm,newdata = x.tr,type = "response") ,as.factor(y.tr) ))
plot(roc.glm.tr)



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
pred.min = predict(fit.min, as.matrix(x.te), type='class')

mean(pred.min!=y.te)

## Fit the final model with lambda.1se
fit.1se = glmnet(as.matrix(x.tr), as.matrix(y.tr), family='binomial', lambda=lambda.1se)
fit.1se$a0 ## intercept
fit.1se$beta ## betahat in logistic regression
pred.1se = predict(fit.1se, as.matrix(x.te), type='class')
mean(pred.1se!=y.te)

####################################################################################
## random Forest
####################################################################################
library(randomForest)
## Try different mtryStart values manually
set.seed(111)
tune.rf = tuneRF(x=train.classification[-ncol(train.classification)], 
                 y=train.classification[,ncol(train.classification)], ntree=1000, mtryStart=4, stepFactor=2,
                 improve=0.05, nodesize=1, trace=T, plot=T,doBest=T)

fit.rf <- randomForest(shares~.,  mtry =10, ntree =500, nodesize = 3,
                          data = train.classification)
pred.rf <- predict(fit.rf,newdata = test.classification,type="prob")
mean((pred.rf[,2]>.5)!=test.classification$shares )
#10 ,500 , 3  0.3363749

## Get the variable importance score
varimp = varImpPlot(fit.rf,type=2)

library(tree)
## Fit classification trees
newx.tr = data.frame(cbind(y.tr, x.tr))
newx.te = data.frame(cbind(y.te, x.te))
fit.tree = tree(as.factor(y.tr)~., data=newx.tr)
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


##################################################################################################
##    Fit boosting
##################################################################################################
library(gbm)
## Use adaboost loss (exponential loss) with additive model. Conduct 10-fold CV
fit1.gbm = gbm(shares ~ ., n.trees=1000, distribution='adaboost', interaction.depth = 1, cv.folds=10, data=train.classification) 

## Output the cv-errors.  This is NOT misclassificaiton error
fit1.gbm$cv.error 

## Find the best iteration number
best.iter <- gbm.perf(fit1.gbm, method="cv") 

## Prediction on test set
pred1 = (predict(fit1.gbm, x.te, n.trees=best.iter)>0)*TRUE 

## Calculate misclassification error on test data
mean(pred1!=y.te) 

## report the importance score of variables
summary(fit1.gbm)




##################################################################################################
## Fit SVM
##################################################################################################
library(e1071)
## Select the best cost parameter with linear kernel

tune1 = tune(svm, train.x=as.matrix(x.tr), train.y=as.factor(y.tr), kernel='linear',
             range=list(cost=2^seq(-5,5,length=10)), 
             control=tune.control(sampling='cross', cross=10)
)

## Fit the "optimal" SVM
fit1.svm = svm(x.tr, as.factor(y.tr), kernel='linear', cost=0.03125)

## Prediction on test data
pred1.svm = predict(fit1.svm, x.te)
mean(pred1.svm!=y.te) 

## Select the best cost parameter with radial kernel (Gaussian kernel)
## The tuning can take a long time, because there are two parameters to tune.
tune2 = tune(svm, train.x=as.matrix(x.tr), 
             train.y=as.factor(y.tr), kernel='radial', range=list(cost=2^seq(-5:5), gamma=2^(-5:5)), 
             control=tune.control(sampling='cross', cross=10))
fit2.svm =  svm(x.tr, as.factor(y.tr), kernel='radial', cost=2, gamma=0.03125)
pred2.svm = predict(fit2.svm, x.te)
mean(pred2.svm!=y.te) 


