options(stringsAsFactors = FALSE)

fileName <- "./data/dataset_edited_without_time.csv"
targetName <- "death_event"

cat("fileName: ", fileName, "\n", sep="")
cat("targetName: ", targetName, "\n", sep="")

list.of.packages <- c("easypackages", "PRROC", "e1071", "randomForest","class", "gmodels", "formula.tools", "dplyr", "pastecs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

rm(list.of.packages, new.packages)

source("./scripts/confusion_matrix_rates.R")
source("./scripts/utils.R")

NUM_METRICS <- 7
confMatDataFrame <- matrix(ncol=NUM_METRICS, nrow=1)
colnames(confMatDataFrame) <- c("MCC", "F1 score", "accuracy", "TP rate", "TN rate", "PR AUC", "ROC AUC")

threshold <- 0.5

patients_data <- read.csv(file=fileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("fileName = ", fileName, "\n", sep="")

# let's put the target label last on the right 
patients_data <- patients_data%>%dplyr::select(-targetName,targetName)
patients_data_original <- patients_data

# cycle of executions

execution_number <- 100
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{
  
  patients_data <- patients_data[sample(nrow(patients_data)),] # shuffle the rows
  
  target_index <- dim(patients_data)[2]
  cat("target_index = ", target_index, "\n", sep="") 
  
  training_set_percentage <- 80
  
  cat("training_set_percentage = ", training_set_percentage, "%\n", sep="")
  
  # the training set is the first training_set_percentage% of the whole dataset
  training_set_first_index <- 1 # NEW
  training_set_last_index <- round(dim(patients_data)[1]*training_set_percentage/100) # NEW
  
  # the test set is the last 20% of the whole dataset
  test_set_first_index <- round(dim(patients_data)[1]*training_set_percentage/100)+1 # NEW
  test_set_last_index <- dim(patients_data)[1] # NEW
  
  cat("[Creating the subsets for the values]\n")
  prc_data_train <- patients_data[training_set_first_index:training_set_last_index, 1:(target_index-1)] # NEW
  prc_data_test <- patients_data[test_set_first_index:test_set_last_index, 1:(target_index-1)] # NEW
  
  
  cat("[Creating the subsets for the labels \"1\"-\"0\"]\n")
  prc_data_train_labels <- patients_data[training_set_first_index:training_set_last_index, target_index] # NEW
  prc_data_test_labels <- patients_data[test_set_first_index:test_set_last_index, target_index]   # NEW
  
  library(class)
  library(gmodels)
  
  # apply k-NN with k_best to the test set
  
  cat("\n[Training the linear regression model on training set & applying the linear regression to test set]\n", sep="")
  
  lin_reg_model_new <- lm(prc_data_train_labels ~ ., data=prc_data_train)
  prc_data_test_pred <- predict(lin_reg_model_new, prc_data_test)
  
  prc_data_test_pred_bin <- as.numeric(prc_data_test_pred) #delete id;
  prc_data_test_pred_bin[prc_data_test_pred_bin>=threshold]<-1
  prc_data_test_pred_bin[prc_data_test_pred_bin<threshold]<-0
  
  thisConfMat <- confusion_matrix_rates(prc_data_test_labels, prc_data_test_pred_bin, "@@@ Test set @@@")
  if (exe_i == 1){
    confMatDataFrame <-  thisConfMat
  }
  else{
    confMatDataFrame <- rbind(confMatDataFrame, thisConfMat)
  }
  
}

cat("\n\n\n=== final results ===\n")

cat("Number of executions = ", execution_number, "\n", sep="")
# statistics on the dataframe of confusion matrices
statDescConfMatr <- stat.desc(confMatDataFrame)
medianAndMeanRowResults <- (statDescConfMatr)[c("median", "mean"),]
print(dec_three(medianAndMeanRowResults))
cat("\n\n=== === === ===\n")

target_csv <- paste("./results/","lin_reg_classification_",execution_number,"_exec.csv", sep = "") 

write.csv(dec_three(medianAndMeanRowResults), target_csv)
cat("file saved in: ",target_csv)
