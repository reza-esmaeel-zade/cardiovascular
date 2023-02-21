options(stringsAsFactors = FALSE)

fileNameData <- "./data/dataset_edited_without_time.csv"
targetName <- "death_event"

list.of.packages <- c("easypackages", "randomForest", "ggplot2", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library("easypackages")
libraries(list.of.packages)

source("./scripts/confusion_matrix_rates.R")
source("./scripts/utils.R")

FEATURE_RANKING_PLOT_DEPICTION <- T
TWO_FEATURES_PLOT <- T

patients_data <- read.csv(fileNameData, header = TRUE, sep =",");
cat("Read data from file ", fileNameData, "\n", sep="")

# rename target
names(patients_data)[names(patients_data) == targetName] <- "target"

cat("application of dplyr::select()\n")
patients_data <- patients_data%>%dplyr::select(-target,target)
target_index <- dim(patients_data)[2]    

# allFeaturesFormula <- as.formula(paste(as.factor(colnames(patients_data)[target_index]), '.', sep=' ~ ' ))
# rf_output <- randomForest(allFeaturesFormula, data=patients_data, importance=TRUE, proximity=TRUE)

allExecutionsFinalRanking <- data.frame(Doubles=double(),
                 Ints=integer(),
                 Factors=factor(),
                 Logicals=logical(),
                 Characters=character(),
                 stringsAsFactors=FALSE)

execution_number <- 100 
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{

    cat("\n\n\n Execution number ", exe_i,"\n", sep="")
    cat("[Randomizing the rows]\n")
    patients_data <- patients_data[sample(nrow(patients_data)),] # shuffle the rows


    cat("application of randomForest()\n")
    rf_output <- randomForest(as.factor(patients_data$target) ~ ., data=patients_data, importance=TRUE, proximity=TRUE)
    
    dd <- as.data.frame(rf_output$importance);
    
    mergedRankingGeneralRank <- agregateTwoSortedRankings(dd, "MeanDecreaseAccuracy", "MeanDecreaseGini")
    
    rownames(mergedRankingGeneralRank) <- (removeDot(removeUnderscore(rownames(mergedRankingGeneralRank))))
    mergedRankingGeneralRank$features <- removeDot(removeUnderscore(mergedRankingGeneralRank$features))

    print(mergedRankingGeneralRank[, c("finalPos", "MeanDecreaseAccuracy", "MeanDecreaseGini"), drop=FALSE])

    finalRankingOneExecution <- mergedRankingGeneralRank[, c("features", "finalPos", "MeanDecreaseAccuracy", "MeanDecreaseGini"), drop=FALSE]
    finalRankingOneExecutionAlphaBeta <- finalRankingOneExecution[order(finalRankingOneExecution$"features"), , drop=FALSE]

    if (exe_i == 1) {
        allExecutionsFinalRanking <- finalRankingOneExecutionAlphaBeta
    } else {
        
        allExecutionsFinalRanking$MeanDecreaseAccuracy <- allExecutionsFinalRanking$MeanDecreaseAccuracy + finalRankingOneExecutionAlphaBeta$MeanDecreaseAccuracy
        allExecutionsFinalRanking$MeanDecreaseGini <- allExecutionsFinalRanking$MeanDecreaseGini + finalRankingOneExecutionAlphaBeta$MeanDecreaseGini
        allExecutionsFinalRanking$finalPos <- allExecutionsFinalRanking$finalPos + finalRankingOneExecutionAlphaBeta$finalPos
    }
}



allExecutionsFinalRanking$MeanDecreaseAccuracy <- allExecutionsFinalRanking$MeanDecreaseAccuracy / execution_number
allExecutionsFinalRanking$MeanDecreaseGini <- allExecutionsFinalRanking$MeanDecreaseGini / execution_number
allExecutionsFinalRanking$finalPos <- allExecutionsFinalRanking$finalPos / execution_number

# # let's eliminate the target index from the rank
# targetRow <-  which(allExecutionsFinalRanking==targetName)
# allExecutionsFinalRanking <- allExecutionsFinalRanking[-c( which(allExecutionsFinalRanking==targetName)), ]

cat("\n\n\n\n== final ranking after ", execution_number, " executions == \n", sep="")

allExecutionsFinalRanking_mse_Gini <-  allExecutionsFinalRanking[, c("MeanDecreaseAccuracy", "MeanDecreaseGini")]
aggregateRankings <- agregateTwoSortedRankings(allExecutionsFinalRanking_mse_Gini, "MeanDecreaseAccuracy", "MeanDecreaseGini")

print(aggregateRankings[, c("finalPos", "MeanDecreaseAccuracy", "MeanDecreaseGini")])

# aggregateRankings[c("features",  )


if (FEATURE_RANKING_PLOT_DEPICTION == TRUE) {
    
        # print(colnames(dd_sorted_IncNodePurity_only))
        x_upper_lim <- -1
          
         barPlotOfRanking(aggregateRankings, aggregateRankings$MeanDecreaseAccuracy, aggregateRankings$features, aggregateRankings$firstColPos, execution_number, "features", "MeanDecreaseAccuracy", x_upper_lim)
         
         barPlotOfRanking(aggregateRankings, aggregateRankings$MeanDecreaseGini, aggregateRankings$features, aggregateRankings$secondColPos, execution_number, "features", "MeanDecreaseGini", x_upper_lim)
            
}
        
if (TWO_FEATURES_PLOT == TRUE) {

        num_of_patients <- dim(patients_data)[1]
        num_of_features <- dim(patients_data)[2] - 1

        # Print the model tree
        # reprtree:::plot.getTree(rf_new)

        dotSize <- 3
        pngHeight <- 1080 # inches
        pngWidth <- 1920 # inches
        textSize <- 30
        
        print(head(patients_data))

        pngFile_dfFile_plot_death_age <- paste("./results/scatterplot_serum_creatinine_VS_ejection_fraction_", execution_number, ".png", sep="")
        png(pngFile_dfFile_plot_death_age, height=pngHeight, width=pngWidth)
        p <- ggplot(patients_data, aes(x=serum_creatinine, y=ejection_fraction, color=factor(patients_data[, "target"], labels = c("survived", "dead")) )) + geom_point(size = dotSize)  + xlab("serum creatinine")   + ylab("ejection fraction") +  labs(colour="patient status", size="") + theme(text = element_text(size=textSize))
        plot(p)
        dev.off()
}
