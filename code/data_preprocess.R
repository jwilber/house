# ====================================================================================
# load utils functions to use and import libraries
# ====================================================================================
source("utils.R")

library(dplyr)





# ====================================================================================
# Load Data
# ====================================================================================
data_train    <- read.csv("../data/train.csv")
data_test     <- read.csv("../data/test.csv")
data_sample   <- read.csv("../data/sample_submission.csv")

# label is SalePrice

# ====================================================================================
# Preprocessing
# ====================================================================================


# =========== prepare data to be combined ===============
# remove useless Id columns
data_train$Id       <- NULL
data_test$Id        <- NULL
# Add type predictor
data_train$type     <- "train"
data_test$type      <- "test"
# Add SalePrice to test so the two datasets have uniform columns
data_test$SalePrice <- 0


# Scale log transform outcome variable so large deviations don't have a large effect
data_train$SalePrice <- log(data_train$SalePrice + 1)

# ESD: Keep only observations in range [mean - 3*sd, mean + 3*sd]
upper_range         <- mean(data_train$SalePrice) + 3 * sd(data_train$SalePrice)
lower_range         <- mean(data_train$SalePrice) - 3 * sd(data_train$SalePrice)
data_train          <- filter(data_train, lower_range < SalePrice, SalePrice < upper_range)


# combine train, test
data_all           <- rbind(data_train, data_test)
data_all$data_type <- as.factor(data_all$type)

# ========== Feature Engineering ===========
# sold - built 
data_all$YrSold_YearBuilt      <- data_all$YrSold - data_all$YearBuilt
# Remodel - built
data_all$YearRemodel_YearBuilt <- data_all$YearRemodAdd - data_all$YearBuilt
# sold - remodel
data_all$YrSold_YearRemodel   <- data_all$YrSold - data_all$YearRemodAdd




#   - factorize categorical variables (same as one-hot encoding)
#   - make sure numeric features are numeric

data_all$MSSubClass  <- as.factor(data_all$MSSubClass) 
data_all$YearBuilt   <- as.factor(data_all$YearBuilt)
data_all$YrSold      <- as.factor(data_all$YrSold)
data_all$MoSold      <- as.factor(data_all$MoSold)
data_all$GarageYrBlt <- as.factor(data_all$GarageYrBlt)

# impute NAs 
data_all <- convert_na(data_all)

# add number of nones
data_all$num_none <- get_number_none(data_all)

# number of factors with no variance (less than 2 factor levels)
data_all$less_than_two <- less_than_two_levels(data_all)



# add existance
data_all$pool_exist       <- as.factor(data_all$PoolArea != 0)
data_all$garage_exist     <- as.factor(data_all$GarageArea != 0)
data_all$masVnrArea_exist <- as.factor(data_all$MasVnrArea != 0)


data_all$LotArea   <- log(data_all$LotArea + 1)
data_all$GrLivArea <- log(data_all$GrLivArea + 1)


data_type     <- data_all$data_type
data_all$type <- NULL

# Can't make a model matrix becase "contrasts can be applied only to factors w 2 or more levels
# Must do below first
# get names of factor variables
factors    <- sapply(data_all, function(x) is.factor(x))
factors    <- data_all[,names(which(l==TRUE))]
good <- ifelse(n<-sapply(factors,function(x)length(levels(x)))==1,"DROP","NODROP")


data_all_matrix               <- as.data.frame(model.matrix( ~ ., data = data_all))
data_all_matrix$`(Intercept)` <- NULL
data_all_matrix$data_type     <- data_type


# ====================================================================================
# Save cleaned datasets
# ====================================================================================
#save(data_all, file = "../data/cleanedData/data_all.RData")
#save(data_all_matrix, file = "../data/cleanedData/data_all_matrix.RData")





























