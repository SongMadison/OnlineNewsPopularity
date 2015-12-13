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
    return(sum(diag(confusionMatrix))/sum(confusionMatrix))
}

