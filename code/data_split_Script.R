# Split data_all into train, validation, and test sets
library(dplyr)
library(caret)

# Note: We ha

# Get training data
load("data/cleanData/data_all.RData")
data.train <- filter(data_all_matrix, data_type == 'train')
# split into train, validation sets
load("data/cleanData/data_all_matrix.RData")
data.all.matrix <- as.data.frame(data_all_matrix)
data.train.matrix <- filter(data.all.matrix, data_type == "train")
data.train.matrix$data_type = NULL



# Get indices for train/validation split
set.seed(420)
train_percent <- 0.7
train_ind <- sample(nrow(data.train.matrix), floor(train_percent * nrow(data.train.matrix)))
train.matrix <- data.train.matrix[train_ind,]
validation.matrix <- data.train.matrix[-train_ind,]
# save
save(train.matrix, file = "data/split_data/train.matrix.RData")
write.csv(train.matrix, file="data/split_data/train.matrix.csv")
save(validation.matrix, file = "data/split_data/validation.matrix.RData")
write.csv(validation.matrix, file="data/split_data/validation.matrix.csv")

# set up test data
test.matrix <- filter(data.all.matrix, data_type == "test")
test.matrix$data_type <- NULL
# save
save(test.matrix, file = "data/split_data/test.matrix.RData")
write.csv(test.matrix, file="data/split_data/test.matrix.csv")
