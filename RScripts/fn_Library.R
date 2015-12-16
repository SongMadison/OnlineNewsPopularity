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

accuracyM <- function(confusionMatrix){
    ## 1 0
    #1 A B
    #0 C D
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