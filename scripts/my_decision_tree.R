library(easypackages)
libraries(c("rpart","rpart.plot","pastecs"))

#source("scripts/my_data_split.R")
source("scripts/confusion_matrix_rates.R")

threshold <- 0.5
execution_number <- 100

## exe_i <- 1
for(exe_i in 1:execution_number){
  source("scripts/my_data_split.R")
  tree_model<-rpart(death_event~.,data = train_data, method = "class")
  target_png <- paste("./results/my_decision_tree/", "dec_tree_",exe_i,".png", sep = "")
  png(target_png, width = 1920, height = 1080, pointsize = 20)
  rpart.plot(tree_model, extra = 100)
  dev.off()
  
  tree_pred <- predict(tree_model, test_data)
  
  for (var_i in 1:dim(tree_pred)[1]) {
    if(tree_pred[var_i,2] >= threshold){
      tree_pred[var_i,2] <- 1
      tree_pred[var_i,1] <- 0
    } else {
      tree_pred[var_i,2] <- 0
      tree_pred[var_i,1] <- 1
    }
  }
  
  thisConfMat <- confusion_matrix_rates(test_data[,"death_event"], tree_pred[,2], "@@@ Test set @@@")
  
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

# tree_model<-rpart(death_event~.,data=train_data)
# rpart.plot(tree_model)
# tree_pred <- predict(tree_model, test_data, type='vector')
# tree_pred[tree_pred >= treshold] <- 1; tree_pred[tree_pred < treshold] <- 0
# plotcp(tree_model)
# dt_acc<-sum(diag(table(test_data$death_event,tree_pred)))/sum(table(test_data$death_event,tree_pred))
# print(paste("%" ,round(dt_acc*100, 1)))
# 
target_csv <- paste("./results/","dec_tree_classification_",execution_number,"_exec.csv", sep = "") 

write.csv(dec_three(medianAndMeanRowResults), target_csv)
cat("file saved in: ",target_csv)
