library(dplyr)

fileName <- "./data/dataset_edited_without_time.csv"
df <- read.csv(fileName, header = TRUE, sep =",");
head(df)


train <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- df[train,]
test_data <- df[-train,]