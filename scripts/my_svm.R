library(kernlab)
library(pROC)
library(dplyr)
library(pastecs)

source("./scripts/confusion_matrix_rates.R")
source("./scripts/utils.R")

threshold <- 0.5
fileName <- "./data/dataset_edited_without_time.csv"
df <- read.csv(fileName, header = TRUE, sep =",");
head(df)

NUM_METRICS <- 7
confMatDataFrame <- matrix(ncol=NUM_METRICS, nrow=1)
colnames(confMatDataFrame) <- c("MCC", "F1 score", "accuracy", "TP rate", "TN rate", "PR AUC", "ROC AUC")
target_index <- dim(df)[2]

execution_number <- 100
cat("Number of executions = ", execution_number, "\n", sep="")

for(exe_i in 1:execution_number){
  
  dataset_dim_retriever(df)
  imbalance_retriever(df$death_event)
  
  target_index <- dim(df)[2]
  
  train <- sample(1:nrow(df), 0.8 * nrow(df))
  train_data <- df[train,]
  test_data <- df[-train,]
  
  svm_model<-ksvm(death_event~., data = train_data, kernel='vanilladot', type = "C-svc")#linear kernel
  
  svm_pred<-predict(svm_model,test_data)
  
  thisConfMat <- confusion_matrix_rates(test_data[,target_index], svm_pred, "@@@ Test set @@@")
  
  if (exe_i == 1)  confMatDataFrame <-  thisConfMat
  else  confMatDataFrame <- rbind(confMatDataFrame, thisConfMat)
  
}

cat("\n\n\n=== final results ===\n")

cat("Number of executions = ", execution_number, "\n", sep="")
# statistics on the dataframe of confusion matrices
statDescConfMatr <- stat.desc(confMatDataFrame)
medianAndMeanRowResults <- (statDescConfMatr)[c("median", "mean"),]
print(dec_three(medianAndMeanRowResults))
cat("\n\n=== === === ===\n")

target_csv <- paste("./results/", "svm_",execution_number,"_exec.csv", sep = "")

write.csv(dec_three(medianAndMeanRowResults), target_csv)
cat("file saved in: ",target_csv)

# train<-sample(1:nrow(df), 0.8 * nrow(df))
# train_data<-df[train,]
# test_data<-df[-train,]
# 
# 
# svm_model<-ksvm(death_event~., data = train_data, kernel='vanilladot', type = "C-svc")#linear kernel
# svm_model
# 
# svm_pred<-predict(svm_model,test_data)
# table(svm_pred,test_data$death_event)
# agree <- svm_pred==test_data$death_event
# svm_acc <- prop.table(table(agree))#accuracy
# svm_acc
