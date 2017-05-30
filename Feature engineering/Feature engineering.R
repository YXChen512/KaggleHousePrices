#  == Feature engineering ==
#

CleanTrain<-readRDS("clean_train.rds")
imputeCol<-colnames(CleanTrain[sapply(CleanTrain,
                                      function(x) {sum(is.na(x))>0})])
imputeCol
colSums(is.na(CleanTrain[,imputeCol]))
#MasVnrType   MasVnrArea BsmtExposure BsmtFinType2 
#8            8            1            1 

# 1. Normalization of numeric features
