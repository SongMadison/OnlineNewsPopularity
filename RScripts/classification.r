# install.packages("gbm")
# install.packages("e1071")
# install.packages("randomForest")
# install.packages("tree")
# install.packages("glmnet")

library(gbm)
library(e1071)
library(randomForest)
library(tree)
library(glmnet)

set.seed(10000)

## generate training data
n = 100
p = 500
xa = matrix(rnorm(n*5),ncol=5)  
xb = matrix(rnorm(n*5,1.5,1),ncol=5)
x1 = rbind(xa,xb)
x2 = matrix(rnorm(2*n*(p-5)),ncol=(p-5))
x  = cbind(x1,x2) 
y = c(rep(0,n),rep(1,n))
x.tr = x
y.tr = y
data.tr = as.data.frame(cbind(y.tr,x.tr)) ## gbm requires the data to be input as a dataframe

## only first 5 variables are important for classification
## check the importance using t.test
t.test(x.tr[1:n,1], x.tr[-(1:n),1])
t.test(x.tr[1:n,10], x.tr[-(1:n),10])


## generate test data
n = 1000
p = 500
xa = matrix(rnorm(n*5),ncol=5)
xb = matrix(rnorm(n*5,1.5,1),ncol=5)
x1 = rbind(xa,xb)
x2 = matrix(rnorm(2*n*(p-5)),ncol=(p-5))
x  = cbind(x1,x2) 
y = c(rep(0,n),rep(1,n))
x.te = x
y.te = y
data.te = as.data.frame(cbind(y.te,x.te))



## Fit boosting

## Use adaboost loss (exponential loss) with additive model. Conduct 10-fold CV
fit1 = gbm(y.tr~., n.trees=10000, distribution='adaboost', interaction.depth = 1, cv.folds=10, data=data.tr) 

## Output the cv-errors.  This is NOT misclassificaiton error
fit1$cv.error 

## Find the best iteration number
best.iter <- gbm.perf(fit1, method="cv") 

## Prediction on test set
pred1 = (predict(fit1, data.te, n.trees=best.iter)>0)*TRUE 

## Calculate misclassification error on test data
mean(pred1!=y.te) 

## report the importance score of variables
summary(fit1)


## Use logistic loss
fit1a = gbm(y.tr~., n.trees=10000, distribution='bernoulli', interaction.depth = 1, cv.folds=10, data=data.tr)
best.iter <- gbm.perf(fit1a, method="cv") 
pred1a = (predict(fit1a, data.te, n.trees=best.iter)>0)*TRUE 
mean(pred1a!=y.te) 

## Use two-way interaction model
fit2 = gbm(y.tr~., n.trees=10000, distribution='adaboost', interaction.depth = 2, cv.folds=10, data=data.tr)
plot(fit2$cv.error)
best.iter <- gbm.perf(fit2, method="cv") 
pred2 = (predict(fit2, data.te, n.trees=best.iter)>0)*TRUE 
mean(pred2!=y.te)
plot(summary(fit2)$rel.inf)

set.seed(100)
fit1.rf=randomForest(as.factor(y.tr)~.,ntree=1000,mtry=10,importantce=T,proximity=TRUE,
                     keep.forest=FALSE,na.action=na.roughfix,data=data.tr)
MDSplot(fit1.rf, as.factor(data.tr$y.tr))


## Fit SVM

## Select the best cost parameter with linear kernel
tune1 = tune(svm, train.x=as.matrix(x.tr), train.y=as.factor(y.tr), kernel='linear',
             range=list(cost=2^seq(-5,5,length=100)), 
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



## Fit randomforest

## Try different mtryStart values manually
set.seed(111)
tune.rf = tuneRF(x=x.tr, y=as.factor(y.tr), ntree=1000, mtryStart=4, stepFactor=2,
                 improve=0.05, nodesize=1, trace=T, plot=T,doBest=T)

## Fit the model
fit.rf  = randomForest(x=x.tr, y=as.factor(y.tr), ntree=1000, mtry=32, 
                       nodesize=1, importance=T,proximity=TRUE)
pred.rf = predict(object=fit.rf, newdata=x.te, type='prob')
mean((pred.rf[,1]<.5)!=y.te)

## Get the variable importance score
varimp = varImpPlot(fit.rf,type=2)


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



## Fit LASSO-logistic regression

## Select lambda using 10-fold CV
cv.lasso = cv.glmnet(x.tr, y.tr, family='binomial', nfolds=10)
lambda.min = cv.lasso$lambda.min
lambda.1se = cv.lasso$lambda.1se ## Result in a sparser model

## Fit the final model with lambda.min
fit.min = glmnet(x.tr, y.tr, family='binomial', lambda=lambda.min)
fit.min$a0 ## intercept
fit.min$beta ## betahat in logistic regression
pred.min = predict(fit.min, x.te, type='class')

mean(pred.min!=y.te)

## Fit the final model with lambda.1se
fit.1se = glmnet(x.tr, y.tr, family='binomial', lambda=lambda.1se)
fit.1se$a0 ## intercept
fit.1se$beta ## betahat in logistic regression
pred.1se = predict(fit.1se, x.te, type='class')
mean(pred.1se!=y.te)

