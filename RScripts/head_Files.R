## choose USA (IA) as the CRAN mirror
# Mirrors <- getCRANmirrors(all = FALSE, local.only = FALSE) # data.frame
# which (getCRANmirrors(all = FALSE, local.only = FALSE)$Name == 'USA (IA)')
chooseCRANmirror(graphics = F, ind = 99 )

Packages <- c("e1071", "tree", "randomForest", "gbm", "glmnet", "AUC","rpart")

##for loop for requiring packages and installiing them if something doesn't exist 
for (Package in Packages){
    if (require(package = Package, character.only = T) == F){
        print(paste ('installing ', Package))
        try(install.packages(Package,dependecies = TRUE))
    }else{
        print(paste(Package,'already exists'))
        require(package = Package, character.only =T)
    }
}

## For parallel processing, when passing a the list of packages ot load
## in all the cores, could be different from Packages

MyAutoLoads <- Packages
