library("ggplot2")

list.of.packages <- c("easypackages", "ggplot2", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library("easypackages")
libraries(list.of.packages)

dataFile <- "./data/S1Data_months_and_deaths.csv"


monthDeathsData <- read.csv(dataFile, header = TRUE, sep =",")

dotSize <- 3
textSize <- 30


pngFile_plot <- paste("./results/barplot_months_survivals_", ".png", sep="")
png(pngFile_plot, height=1080, width=1920)

p <- ggplot(data=monthDeathsData, aes(x=TIME_MONTH, y=survived_percent)) + geom_bar(stat="identity", fill="steelblue", width=0.5) + xlab("month")   + ylab("survived percentage %") + scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9))  + theme(text = element_text(size=textSize)) 

plot(p)
dev.off()

cat("The file ", pngFile_plot, " has been saved\n", sep="")