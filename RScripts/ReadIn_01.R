
rm(list =ls())

RScriptPath <- paste0("./RScripts/")
PlotsPath <- paste0("./Plots/")
DataPath <- paste0("./Data/")

source("./RScripts/head_Files.R")
source("./RScripts/fn_Library.R")
library(AUC)


#####################################################################################


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
write.csv(train, paste0(DataPath,"train.csv"),row.names = F)
write.csv(test, paste0(DataPath,"test.csv"), row.names = F)


