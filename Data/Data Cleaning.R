train <- read.csv("train.csv")
train_size <-dim(train)[1]
test<- read.csv("test.csv")
test_size <- dim(test)[1]
raw_features <- rbind(train[,c(-1,-81)],test[,c(-1)])
# column 1: ID, column 81: SalePrice

#Some features have numeric values
#but they are /should be considered as categorical

numFea<- names(raw_features[,sapply(raw_features,is.numeric)])
numFea

#Mis-classified features by hand pickiing:
misClass <- c("MSSubClass","OverallQual","OverallCond","Fireplaces",
              "MoSold","TotRmsAbvGrd","BsmtFullBath","BsmtHalfBath",
              "FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","GarageCars")
misClass

raw_features$OverallQual <- factor(raw_features$OverallQual,levels = 1:10,ordered = TRUE)
raw_features$OverallCond <- factor(raw_features$OverallCond,levels = 1:10,ordered = TRUE)
raw_features$Fireplaces <- factor(raw_features$Fireplaces,levels = 0:3,ordered = TRUE)
raw_features$TotRmsAbvGrd <- factor(raw_features$TotRmsAbvGrd,levels = 1:15,ordered = TRUE)
raw_features$BsmtFullBath <- factor(raw_features$BsmtFullBath,levels = 0:3,ordered = TRUE)
raw_features$BsmtHalfBath <- factor(raw_features$BsmtHalfBath,levels = 0:2,ordered = TRUE)
raw_features$FullBath <- factor(raw_features$FullBath,levels = 0:3,ordered = TRUE)
raw_features$HalfBath <- factor(raw_features$HalfBath,levels = 0:2,ordered = TRUE)
raw_features$BedroomAbvGr<- factor(raw_features$BedroomAbvGr,levels = 0:8,ordered = TRUE)
raw_features$MoSold<- as.factor(raw_features$MoSold)
raw_features$MSSubClass<- as.factor(raw_features$MSSubClass)
raw_features$KitchenAbvGr<- factor(raw_features$KitchenAbvGr,levels = 0:3,ordered = TRUE)

#barplot(table(train$GarageCars))
raw_features$GarageCars<- factor(raw_features$GarageCars,levels = 0:4,ordered = TRUE)

factorFea<- names(raw_features[,sapply(raw_features,is.factor)])
factorFea

numFea<- names(raw_features[,sapply(raw_features,is.numeric)])
numFea

#Note that, there are ordered categorical features vs. non-ordinal
# carefully examine which ones are ordinal
# in particular, quality involved features are ordinal
#but romove NA first to reduce the amount of work


# Examine features with NAs
which(colSums(is.na(raw_features[,numFea]))!=0)
#LotFrontage  MasVnrArea  BsmtFinSF1  BsmtFinSF2   BsmtUnfSF TotalBsmtSF GarageYrBlt 
#1           5           6           7           8           9          14 
#GarageArea 
#15 
which(colSums(is.na(raw_features[,factorFea]))!=0)
#MSZoning        Alley    Utilities  Exterior1st  Exterior2nd   MasVnrType 
#2            4            7           19           20           21 
#BsmtQual     BsmtCond BsmtExposure BsmtFinType1 BsmtFinType2   Electrical 
#25           26           27           28           29           33 
#BsmtFullBath BsmtHalfBath     FullBath  KitchenQual   Functional   Fireplaces 
#34           35           36           40           42           43 
#FireplaceQu   GarageType GarageFinish   GarageCars   GarageQual   GarageCond 
#44           45           46           47           48           49 
#PoolQC        Fence  MiscFeature     SaleType 
#51           52           53           55 


# =====Caution!===============
# for some features, NA is a level, not missing data

# Bsmt including features, NA -- NoBsmt
addLevel1 <- factorFea[which(grepl("Bsmt", factorFea))]
addLevel1
#[1] "BsmtQual"     "BsmtCond"     "BsmtExposure" "BsmtFinType1" "BsmtFinType2"
#[6] "BsmtFullBath" "BsmtHalfBath"
# the last two was in numeric, now is factor. but NoBsmt is "0",so already good

# if total Bsmt SF == 0, no basement. Leave the rest NAs for MICE
#sapply(raw_features[,addLevel1[1:5]],function(x) levels(x)<-c(levels(x),"NoBsmt"))
for (name in addLevel1[1:5]) {
  levels(raw_features[,name]) <- c(levels(raw_features[,name]),"NoBsmt")
}

raw_features[which(raw_features$TotalBsmtSF==0),addLevel1[1:5]]<-"NoBsmt"
colSums(is.na(raw_features[,addLevel1[1:5]]))
#   BsmtQual     BsmtCond BsmtExposure BsmtFinType1 BsmtFinType2 
#          3            4            4            1            2 




# fireplace including features, NA -- NoFire
levels(raw_features$FireplaceQu) <- c(levels(raw_features$FireplaceQu),"NoFire")
raw_features$FireplaceQu[is.na(raw_features$FireplaceQu)] <- "NoFire"

# Fence including features, NA -- NoFence
levels(raw_features$Fence) <- c(levels(raw_features$Fence),"NoFence")
raw_features$Fence[is.na(raw_features$Fence)] <- "NoFence"

# garage including features, NA -- NoGrg
addLevel2 <- factorFea[which(grepl("Garage", factorFea))]
addLevel2
#[1] "GarageType"   "GarageFinish" "GarageCars"   "GarageQual"   "GarageCond" 
# the 3rd feature contains no NA, as level 0 indicates no Garage
for (name in addLevel2[c(-3)]) {
  levels(raw_features[,name]) <- c(levels(raw_features[,name]),"NoGrg")
}
raw_features[which(raw_features$GarageArea==0),addLevel2[c(-3)]]<-"NoGrg"

# features with TOO MANY NAs
drop_for_NA <- which(sapply(raw_features,function(x) {sum(is.na(x))/length(x) >0.05}))
drop_for_NA



#drop the features with only one dominating level, population ratio >0.9
drop_for_singular<-which(sapply(raw_features,
                                function(x) {sum(table(x)>length(x)*0.9)>0}))
drop_for_singular

# remove the columns in either drop_for_NA or dorp_for_singular
drop<- unique(c(drop_for_NA,drop_for_singular))
drop
clean_features <- raw_features[,-drop]
dim(clean_features)


imputeCol<-colnames(clean_features[sapply(clean_features,
                                          function(x) {sum(is.na(x))>0})])
imputeCol
colSums(is.na(clean_features[,imputeCol]))
#MSZoning  Exterior1st  Exterior2nd   MasVnrType   MasVnrArea     BsmtQual 
#4            1            1           24           23            3 
#BsmtCond BsmtExposure BsmtFinType1   BsmtFinSF1 BsmtFinType2   BsmtFinSF2 
#4            4            1            1            2            1 
#BsmtUnfSF  TotalBsmtSF   Electrical BsmtFullBath BsmtHalfBath     FullBath 
#1            1            1            2            2            4 
#KitchenQual   Functional   Fireplaces GarageFinish   GarageCars   GarageArea 
#1            2            1            2            2            1 
#GarageQual   GarageCond     SaleType 
#2            2            1 

CleanTrain<- clean_features[1:train_size,]
CleanTest<- clean_features[(train_size+1):(train_size+test_size),]
CleanTrain$SalePrice <- train$SalePrice
# for NA in Mas, MICE
#imp <- mice(clean_features, m=1, printFlag=FALSE)
#colSums(complete(imp.features)[,imputeCol]!=clean_features[,imputeCol])

dim(CleanTrain)
dim(CleanTest)
#write.csv(clean_features, file = "clean_train.csv",row.names = FALSE)
saveRDS(CleanTrain,"clean_train.rds")
saveRDS(CleanTest,"clean_test.rds")
# saveRDS() keeps the factor information
# whereas read.csv() doesNOT
