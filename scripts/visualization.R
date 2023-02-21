library(tidyverse)

fileName <- "./data/dataset_edited_without_time.csv"
event <- "death_event"

devoff <- function() {
  while (length(dev.list()))
    dev.off()
}

mainData <- read.csv(fileName)
w <- 1920; h <- 1080

mainData$death_event <- factor(mainData$death_event)

target_png <- "./results/age.png"
png(filename = target_png, width = w, height = h)

ggplot(mainData, aes(x = age)) +
  geom_histogram(binwidth = 5,fill = "royalblue",color="black")+
  labs(y = "count",x = "Age") +
  theme(text = element_text(size = 30))+
  ggtitle("Distribution of Age")
devoff()

target_png <- "./results/age_boxplot.png"
png(target_png, width = 900, height = 300)
ggplot(mainData, aes(x = age)) +
  geom_boxplot()
devoff()

target_png <- "./results/age_survival.png"

png(filename = target_png, width = w, height = h)

mainData %>%
  ggplot(aes(age, fill = death_event)) +
  geom_histogram(position = "fill")+
  ggtitle("age survival") +
  ylab("rate")+
  theme(text = element_text(size = 30))
devoff()

target_png <- "./results/death_event_sex.png"
png(target_png , width = w/2, height = h/2)
ggplot(data = mainData, mapping = aes(x = sex, y = after_stat(count), fill = death_event)) + 
  geom_bar(stat = "count", position='dodge')+
  labs(title = "sex death event plot")+
  theme_bw()
devoff()



target_png <- "./results/death_event_smoking.png"
png(target_png , width = w/2, height = h/2)
mainData %>%
  ggplot(aes(smoking, fill = death_event)) +
  theme_gray() +
  geom_bar(position = "fill")+
  theme(text = element_text(size = 30))
devoff()

target_png <- "./results/cpk_sodim_event.png"
png(target_png , width = w, height = h)

mainData %>%
  ggplot( aes(x=creatinine_phosphokinase, y=serum_sodium,
              color = death_event) ) +
    geom_point(size = 6)  + 
    xlab("cpk[0,2e3]")   + 
    ylab("serum_sodium") +  
    labs(colour="patient status", size="") + 
    theme(text = element_text(size=20)) +
    xlim(0,2e3)
devoff()

source("scripts/eval.R")