# ====================================================================================
# utils
# ====================================================================================
# Impute na values: integers: mean, factors: None
convert_na <- function(data) {
  
  for (predictor in colnames(data)){
    pred = eval(quote(predictor))
    selected_pred = data[, pred]
    
    # if column is factor, convert na value to "None"
    if (is.factor(selected_pred)) {    
      
      tmp = as.character(selected_pred)
      tmp[is.na(tmp)] = "None"
      tmp = as.factor(tmp)
      data[, pred] = tmp
    }
    # else, if coclimn is integer, convert na to mean(column)
    if (is.integer(selected_pred)) {
      
      selected_pred[is.na(selected_pred)] = mean(selected_pred, na.rm = TRUE)
      data[, pred] = selected_pred
    }
    
  }
  return(data)
  
}


# Count number of na values per feature
get_number_none <- function(data) {
  
  number_of_nones <- apply(data, 1, function(x) {
    sum(x == 'None')
  })
  
  return(number_of_nones)
}

# Find factors with less than 2 levels
less_than_two_levels <- function(col) {
  
  if (is.factor(col)) {
    return (length(levels(col)) < 2)
  } else {
    return (FALSE)
  }
  
}

#table(sapply(data_all, function(x) less_than_two_levels(x)))
# ====================================================================================
#
# ====================================================================================

library(dplyr)
library(ggplot2)
library(caret)
library(doMC)
library(stringr)
library(glmnet)
library(corrplot)


# import data
setwd("~/Desktop/housedata/")
data_train    <- read.csv("data/train.csv")
data_test     <- read.csv("data/test.csv")
data_sample   <- read.csv("data/sample_submission.csv")

# label is SalePrice

# ====================================================================================
# Preprocessing
# ====================================================================================

# ======== part 1 =========

# organize data so we can transform it all at once
#   - factorize categorical variables (same as one-hot encoding)
#   - make sure numeric features are numeric

# ======== part 2 =========

# assess model diagnostics in various ways
# remove outliers
# variable selection 
#         - Use ANOVA + VIF to identify useless vairables and drop them
#         - Use correlation matrix on features to identify highly correlated variables and drop them
#         - Use LASSO for variable selection
# transform data accordingly
# Question:
#       How to identify how to transform my data; e.g. do i just look at the residuals after having fit the model, and if they're not
#       linear then transform the RESPONSE variable?
#             (so am i only transforming the response variable)?

names(data_train)

# remove useless Id columns
data_train$Id       <- NULL
data_test$Id        <- NULL

# Add type predictor
data_train$type     <- "train"
data_test$type      <- "test"
# Add data_test$SalePrice so the two datasets have uniform columns
data_test$SalePrice <- 0

# Scale log transform
data_train$SalePrice <- log(data_train$SalePrice + 1)

# remove outliers 22 w/o log, 17 points with log trans

# ESD: Keep only observations in range [mean - 3*sd, mean + 3*sd]
upper_range <- mean(data_train$SalePrice) + 3 * sd(data_train$SalePrice)
lower_range <- mean(data_train$SalePrice) - 3 * sd(data_train$SalePrice)
data_train <- filter(data_train, lower_range < SalePrice, SalePrice < upper_range)


# combine train, test
data_all <- rbind(data_train, data_test)
data_all$data_type <- as.factor(data_all$type)

# ========== Feature Engineering ===========
# sold - built
data_all$YrSold_YearBuilt <- data_all$YrSold - data_all$YearBuilt

# Remodel - built
data_all$YearRemodel_YearBuilt <- data_all$YearRemodAdd - data_all$YearBuilt

# sold - remodel
data_all$YrSold_YearRemodel <- data_all$YrSold - data_all$YearRemodAdd

head(data_all)


apply(data_all, 2, class)

#   - factorize categorical variables (same as one-hot encoding)
#   - make sure numeric features are numeric

data_all$MSSubClass <- as.factor(data_all$MSSubClass) 
data_all$YearBuilt <- as.factor(data_all$YearBuilt)
data_all$YrSold <- as.factor(data_all$YrSold)
data_all$MoSold <- as.factor(data_all$MoSold)
data_all$GarageYrBlt <- as.factor(data_all$GarageYrBlt)

# impute NAs 
data_all <- convert_na(data_all)

# add number of nones
data_all$num_none <- get_number_none(data_all)

# number of factors with no variance (less than 2 factor levels)
data_all$less_than_two <- less_than_two_levels(data_all)



# add existance
data_all$pool_exist <- as.factor(data_all$PoolArea != 0)
data_all$garage_exist <- as.factor(data_all$GarageArea != 0)
data_all$masVnrArea_exist <- as.factor(data_all$MasVnrArea != 0)


data_all$LotArea <- log(data_all$LotArea + 1)
data_all$GrLivArea <- log(data_all$GrLivArea + 1)


data_type <- data_all$data_type
data_all$type <- NULL

# Can't make a model matrix becase "contrasts can be applied only to factors w 2 or more levels
# Must do below first
# get names of factor variables
l <- sapply(data_all, function(x) is.factor(x))
m <- data_all[,names(which(l==TRUE))]
good <- ifelse(n<-sapply(m,function(x)length(levels(x)))==1,"DROP","NODROP")


data_all_matrix <- as.data.frame(model.matrix( ~ ., data = data_all))
data_all_matrix$`(Intercept)` <- NULL
data_all_matrix$data_type <- data_type


# save RData
save(data_all, file = "data/cleanedData/data_all.RData")
save(data_all_matrix, file = "data/cleanedData/data_all_matrix.RData")


# ====================================================================================
# Preprocessing
# ====================================================================================






# save this until AFTER we've cleaned/sorted data
# library(car)
# m0 <- lm(SalePrice ~ ., data=data_all)
# plot(m0, which=2)
# summary(m0)

# Ideas: 1. normal multiple-regression and use VIF to remove highly correlated variables
#           - we can also use anova to see how our models fit and find the best nonlinear transformation for the data

# Idea 2: Use LASSO ti idenfity the important variables and use that





























