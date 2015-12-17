
rm(list =ls())

RScriptPath <- paste0("./RScripts/")
PlotsPath <- paste0("./Plots/")
DataPath <- paste0("./Data/")

source("./RScripts/head_Files.R")
source("./RScripts/fn_Library.R")
########################################################################

### learning :

load("./Results/results_1000_1.RData")
data1000<- data.frame(accuracy)
load("./Results/results_2000_1.RData")
data2000<- data.frame(accuracy)
load("./Results/results_5000_1.RData")
data5000<- data.frame(accuracy)
load("./Results/results_10000_1.RData")
data10000<- data.frame(accuracy)
load("./Results/results_20000_1.RData")
data20000<- data.frame(accuracy)
load("./Results/results_30000_1.RData")
data30000<- data.frame(accuracy)

accu <- rbind( data1000, data2000,data5000, data10000,data20000,data30000)
accu <- accu[c(1,4,7,10,13,16),]

accu<- cbind(accu,NeuralNet)


size <- c(1000,2000,5000,10000,20000,30000)
plot(size, accu[,1],type="b", ylim= c(0.55, 0.69), 
     main="Effects of trainsize", xlab= "train size", ylab="accuracy")
points(size, accu[,2],type="b", col ="red")
points(size, accu[,3],type="b",col="blue")
points(size, accu[,4],type="b"
       , col="cyan")
points(size, accu[,5],type="b", col="purple")
points(size, accu[,6],type="b",col="darkgreen")
#points(c(1000,2000,5000,10000,20000,30000), accu[,7],type="b",col="black")

legend("bottomright", legend=c("logistic","glmLasso", "rForest","Tree", "adaBoost", "knn(5)"),
       col =c("black","red","blue","cyan","purple","darkgreen"), lty =1,cex=0.8,pch =1)




### Accuracy table
load("./Results/results_30000_1.RData")
load("./Data/News_std.RData")
neural <- read.csv("./Results/neural.txt",header= T)

load("./Results/svmScores.RData")

probability$neural <- neural[,2]
probability$svm <- attr(pred3.svm,"probabilities")[,2]

accuracy$neural <- unlist(accuracyM(probability$neural>0.5,y.te))
aucScore$neural <- auc(roc(probability$neural,as.factor(y.te)))



accuracy <- data.frame(accuracy)
F1Score <- accuracy[2,]*accuracy[3,]*2/(accuracy[2,]+accuracy[3,])
accuracy <- rbind(accuracy,F1Score)
accuracy <- rbind(accuracy,unlist(aucScore))
rownames(accuracy)[4:5] <- c("F1Score","AUC")
accuracy <-t(accuracy)


library(xtable)
xtable(accuracy)



### ROC curves on the full data sets

plot(roc(probability$glm, as.factor(y.te)),main="ROC for Different Classifiers")

roc.rf <- roc(probability$rf, as.factor(y.te))
points(roc.rf$fpr, roc.rf$tpr,pch=".", col="green" ,type ="b")

roc.glmnet<- roc(probability$glmnet, as.factor(y.te))
points(roc.glmnet$fpr, roc.glmnet$tpr,pch=".", type= "b",col="cyan" )

roc.knn<- roc(probability$knn, as.factor(y.te))
points(roc.knn$fpr, roc.knn$tpr,pch="x", type ="b",col="darkgreen",cex=0.2 )

roc.gbm<- roc(probability$gbm, as.factor(y.te))
points(roc.gbm$fpr, roc.gbm$tpr,pch=".", type= "b",col="red" )

roc.rpart<- roc(probability$rpart, as.factor(y.te))
points(roc.rpart$fpr, roc.rpart$tpr, pch=".", type= "b",col="blue")

roc.neural <- roc(probability$neural, as.factor(y.te)) 
points(roc.neural$fpr, roc.neural$tpr, pch=".", type= "b", col="purple")

legend("bottomright",legend=c("logistic","rForest","glmLasso","knn(5)",
                              "adaBoost","Tree","neuralNet"),
       lty=1, col=c("black","green", "cyan", "darkgreen","red","blue","purple"))



