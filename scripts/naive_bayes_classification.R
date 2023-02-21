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


source("./scripts/confusion_matrix_rates.R")
source("./scripts/utils.R")

NUM_METRICS <- 7
confMatDataFrame <- matrix(ncol=NUM_METRICS, nrow=1)
colnames(confMatDataFrame) <- c("MCC", "F1 score", "accuracy", "TP rate", "TN rate", "PR AUC", "ROC AUC")

threshold <- 0.5

patients_data <- read.csv(file=fileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("fileName = ", fileName, "\n", sep="")

## patients_data[,targetName] <- as.factor(patients_data[,targetName])

# # let's put the target label last on the right 
# patients_data <- patients_data %>% select(-targetName,targetName)
# patients_data_original <- patients_data

# cycle of executions

# edited by reza
execution_number <- 1000
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{
  
  patients_data <- patients_data[sample(nrow(patients_data)),] # shuffle the rows
  
  target_index <- dim(patients_data)[2]
  
  training_set_perce = 80
  cat("training_set_perce = ", training_set_perce, "%\n", sep="")
  
  # the training set is the first 80% of the whole dataset
  training_set_first_index <- 1 # NEW
  training_set_last_index <- round(dim(patients_data)[1]*training_set_perce/100) # NEW
  
  # the test set is the last 20% of the whole dataset
  test_set_first_index <- training_set_last_index+1 # NEW
  test_set_last_index <- dim(patients_data)[1] # NEW
  
  cat("[Creating the subsets for the values]\n")
  patients_data_train <- patients_data[training_set_first_index:training_set_last_index, 1:(target_index)] # NEW
  patients_data_test <- patients_data[test_set_first_index:test_set_last_index, 1:(target_index)] # NEW
  
  cat("[Creating the subsets for the labels \"1\"-\"0\"]\n")
  patients_data_train_labels <- patients_data[training_set_first_index:training_set_last_index, target_index] # NEW
  patients_data_test_labels <- patients_data[test_set_first_index:test_set_last_index, target_index]   # NEW
  
  
  print("dim(patients_data_train)")
  print(dim(patients_data_train))
  
  print("dim(patients_data_test)")
  print(dim(patients_data_test))
  
  allFeaturesFormula <- as.formula(paste(as.factor(colnames(patients_data)[target_index]), '.', sep=' ~ ' ))
  naive_bayes_model <-  naiveBayes(allFeaturesFormula, data=patients_data_train)
  
  patients_data_test_PRED <- predict((naive_bayes_model), patients_data_test)
  patients_data_test_PRED_binary <- as.numeric(patients_data_test_PRED)-1
  
  patients_data_test_PRED_binary[patients_data_test_PRED_binary>=threshold]=1
  patients_data_test_PRED_binary[patients_data_test_PRED_binary<threshold]=0
  
  thisConfMat <- confusion_matrix_rates(patients_data_test_labels, patients_data_test_PRED_binary, "@@@ Test set @@@")
  
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

target_csv <- paste("./results/","naive_bayes_classification_",execution_number,"_exec.csv", sep = "") 

write.csv(dec_three(medianAndMeanRowResults), target_csv)
cat("file saved in: ",target_csv)

