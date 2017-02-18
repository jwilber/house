
library(data.table)
library(xgboost)
library(Metrics)
library(Matrix)
library(mice)
library(dplyr)

TRAIN='data/train.csv'
TEST='data/test.csv'
SUBMISSION= "data/sample_submission.csv"

#load data
train=fread(TRAIN,showProgress = T)
test=fread(TEST,showProgress = T)
y_train=train$SalePrice

#Remove Id since of no use
train$Id=NULL
train$SalePrice=NULL
test$Id=NULL

#Row binding train & test set for feature engineering
train_test = rbind(train, test)
ntrain=nrow(train)


# create some new features
# sold - built 
train_test$YrSold_YearBuilt      <- train_test$YrSold - train_test$YearBuilt
# Remodel - built
train_test$YearRemodel_YearBuilt <- train_test$YearRemodAdd - train_test$YearBuilt
# sold - remodel
train_test$YrSold_YearRemodel   <- train_test$YrSold - train_test$YearRemodAdd




features=names(train)


#convert character into integer
for(f in features){
  if(class(train_test[[f]])=="character"){
    levels=sort(unique(train_test[[f]]))
    train_test[[f]]=as.integer(factor(train_test[[f]],levels = levels))
  }
}

#feature to exclude
features_to_drop<-c("Utilities","LotFrontage","Alley","MasVnrType","MasVnrArea","BsmtQual",
                    "BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2",
                    "Electrical","FireplaceQu","GarageType","GarageYrBlt",
                    "GarageFinish","GarageQual","GarageCond","PoolQC",
                    "Fence","MiscFeature")


#splitting whole data back again
train_x <- train_test[1:ntrain,-features_to_drop,with= FALSE]
test_x <- train_test[(ntrain+1):nrow(train_test),-features_to_drop,with= FALSE]



#missing values imputation with mice
set.seed(144)
to_impute <- as.data.frame(test_x)
impute <- to_impute[c("MSZoning","Exterior1st","Exterior2nd","BsmtFinSF1",
                   "BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","BsmtFullBath","BsmtHalfBath",
                   "KitchenQual","Functional","GarageCars","GarageArea","SaleType", "YrSold_YearBuilt", 
                   "YearRemodel_YearBuilt", "YrSold_YearRemodel")]
imputed <- complete(mice(impute,m=2))

to_impute$MSZoning              <- imputed$MSZoning
to_impute$Utilities             <- imputed$Utilities
to_impute$Exterior1st           <- imputed$Exterior1st
to_impute$Exterior2nd           <- imputed$Exterior2nd
to_impute$BsmtFinSF1            <- imputed$BsmtFinSF1
to_impute$BsmtFinSF2            <- imputed$BsmtFinSF2
to_impute$BsmtUnfSF             <- imputed$BsmtUnfSF
to_impute$TotalBsmtSF           <- imputed$TotalBsmtSF
to_impute$BsmtHalfBath          <- imputed$BsmtHalfBath
to_impute$BsmtFullBath          <- imputed$BsmtFullBath
to_impute$KitchenQual           <- imputed$KitchenQual
to_impute$Functional            <- imputed$Functional
to_impute$GarageCars            <- imputed$GarageCars
to_impute$GarageArea            <- imputed$GarageArea
to_impute$SaleType              <- imputed$SaleType
to_impute$YrSold_YearBuilt      <- imputed$YrSold_YearBuilt
to_impute$YearRemodel_YearBuilt <- imputed$YearRemodel_YearBuilt
to_impute$YrSold_YearRemodel    <- imputed$YrSold_YearRemodel


test_x <- as.data.table(to_impute)

#convert into numeric for XGBoost implementation

train_x[] <- lapply(train_x, as.numeric)
test_x[]<-lapply(test_x, as.numeric)
train_x$SalePrice <- y_train


# Save data
#write.csv(train_x, file = "data/cleanData/clean_train.csv")
#write.csv(test_x, file = "data/cleanData/clean_test.csv")
