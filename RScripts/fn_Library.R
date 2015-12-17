PrepareData <- function(trainData) {
    colNames <- colnames(trainData)
    # trainData <- trainData[,-1]
    newData <-list()
    for( i in 1:13){
        newData[[colNames[i]]] <- trainData[,i]
    }
    # 14 - 19 colunums of newData are different data channels:
    channel <- character(nrow(trainData))
    values <- c("lifestyle","entertainment","bus","socmed","tech","world")
    for ( i in 1:nrow(trainData)){
        if(sum ( trainData[i,14:19] ==1)>0) {
            channel[i] <- values[trainData[i,14:19]==1] 
        } else{
            channel[i] <-"others"
        }
    }
    newData[["data_channel"]] <- channel
    for ( i in 20:31){
        newData[[colNames[i]]] <- trainData[,i]
    }
    weekday <- character(nrow(trainData))
    values <- c("Monday","Tuesday","Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    for( i in 1: 1:nrow(trainData)){
        weekday[i]<-  values[trainData[i,32:38]==1] 
    }
    newData[["weekday"]] <- weekday
    for( i in 39:61){
        newData[[colNames[i]]] <- trainData[,i]
    }
    newData <- data.frame(newData)
}

accuracyM <- function(y.pred, y.true){
    ## 1 0
    #1 A B
    #0 C D
    A <- sum((y.pred==1)* (y.true==1))
    B <- sum((y.pred==0)* (y.true==1))
    C <- sum((y.pred==1)* (y.true==0))
    D <- sum((y.pred==0)* (y.true==0))
    confusionMatrix <- matrix(c(A,B,C,D),nrow=2, byrow = T)
    accuracy <- sum(diag(confusionMatrix))/sum(confusionMatrix)
    precision <- confusionMatrix[1,1]/(confusionMatrix[1,1]+confusionMatrix[2,1])
    recall <- confusionMatrix[1,1]/(confusionMatrix[1,1]+confusionMatrix[1,2])
    res <- list(accuracy = accuracy, precision =precision, recall = recall)
    return(res)
}

DataStandardization <- function(train, test){
    for(i in 1:ncol(train)){
        min_i <- min(train[,i])
        max_i <- max(train[,i])
        train[,i] <- (train[,i]-min_i)/(max_i- min_i)
        test[,i]<- (test[,i]-min_i)/(max_i- min_i)
    }
    res <- list(train= train, test =test)
    return(res)
}
DataStandardization2 <- function(train,test){
    for(i in 1:ncol(train)){
        mean_i <- mean(train[,i])
        sd_i <- sd(train[,i])
        train[,i] <- (train[,i]-mean_i)/sd_i
        test[,i] <- (test[,i]-mean_i)/sd_i
    }
    res <- list(train= train, test =test)
    return(res)
}

learningCurve <- function(sampleN, rep , XX.tr , YY.tr, XX.te, YY.te) {
    
    
    samp <- sample(length(YY.tr), sampleN)
    x.tr <- XX.tr[samp,]
    y.tr <- YY.tr[samp]
    x.te <- XX.te
    y.te <- YY.te
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
    fit.min = glmnet(as.matrix(x.tr), as.matrix(y.tr), family='binomial', lambda=lambda.min)
    pred.min = predict(fit.min, as.matrix(x.te), type='response')
     probability$glmnet <- pred.min
    (aucScore$glmnet <- auc( roc(pred.min ,as.factor(y.te) )) )
    (accuracy$glmnet <- unlist(accuracyM((pred.min>0.5)*1,y.te)))
    
    
    
    ####################################################################################
    ## random Forest
    ####################################################################################
    #library(randomForest)
    ## Try different mtryStart values manually
    set.seed(111)
    
    fit.rf <- randomForest(as.factor(y.tr)~.,  mtry =10, ntree =1000, nodesize = 1,
                           data = newx.tr)
    pred.rf <- predict(fit.rf,newdata = x.te,type="prob")
    
    probability$rf <- pred.rf [,2]
    (aucScore$rf <- auc(roc( probability$rf,as.factor(y.te) )) )
    (accuracy$rf <- unlist(accuracyM((pred.rf[,2]>0.5)*1,y.te)))
    
    ###############################################
    ## Tree Algorithm
    #######################################################################3
    #library(rpart)
    fit.rpart <- rpart(as.factor(y.tr) ~ ., method="class", 
                       control = rpart.control(minsplit=10, cp=0.001),data=newx.tr)
    xmat <- xpred.rpart(fit.rpart)
    xmat <- 1*(xmat==2)
    cv.rpart <- apply (X = xmat, MARGIN = 2,function(x) mean(x==y.tr))
    cps <- as.numeric(colnames(xmat))
    cp.max <- cps[which(cv.rpart == max(cv.rpart))]
    
    #“vector”, “prob”, “class”, “matrix”
    fit.rpart <- rpart(as.factor(y.tr) ~ ., method="class", 
                       control = rpart.control(minsplit=10, cp=cp.max),data=newx.tr)
    probability$rpart <- predict(fit.rpart,newdata = x.te,type = "prob")[,2]
    (aucScore$rpart <- auc(roc( probability$rpart,as.factor(y.te) )) )
    (accuracy$rpart <- unlist(accuracyM(1*(probability$rpart>0.5), y.true =y.te)))
    
    
    ##################################################################################################
    ##    Fit boosting
    ##################################################################################################
    library(gbm)
    ## Use adaboost loss (exponential loss) with additive model. Conduct 10-fold CV
    fit.gbm = gbm(y.tr ~ ., n.trees=10000, distribution='adaboost', interaction.depth = 1, cv.folds=10, data=newx.tr) 
    
    ## Find the best iteration number
    best.iter <- gbm.perf(fit.gbm, method="cv") 
    
    probability$gbm <- predict(fit.gbm, x.te, n.trees=best.iter)
    (aucScore$gbm <- auc(roc( probability$gbm,as.factor(y.te) )) )
    (accuracy$gbm <- unlist(accuracyM(1*(probability$gbm>0), y.true =y.te)))
    
    ##################################################################################################
    ##    KNN
    ##################################################################################################
    
    library(class)
    #select k
    # knn.cv1 <- knn.cv(train = x.tr, cl= as.factor(y.tr), k = 5, l = 0, prob = FALSE, use.all = TRUE)
    
    fit.knn<- knn(train = x.tr, test =x.te, cl =y.tr, k = 5, l = 0, prob = TRUE, use.all = TRUE)
    prob <- numeric(length(y.te))
    prob[which(fit.knn=="1")]=attr(fit.knn,"prob")[which(fit.knn=="1")]
    prob[which(fit.knn=="0")]=1-attr(fit.knn,"prob")[which(fit.knn=="0")]
    mean((1*(prob>0.5))==y.te)
    
    probability$knn <- prob
    (aucScore$knn <- auc(roc( probability$knn,as.factor(y.te) )) )
    (accuracy$knn <- unlist(accuracyM(prob>0.5, y.true =y.te)))
    
    save(probability,accuracy,aucScore, file=paste0("./Results/results_",sampleN,"_",rep,".RData"))
}

