library(tidyverse) 
library(caret) 
library(tidymodels)
library(ggthemes)


data <- read.csv("./data/dataset_edited_without_time.csv")

data$death_event <- as.factor(data$death_event)

execution_number <- 100
for(exe_i in 1:execution_number){
  
  data_split <- initial_split(data)
  data_test <- testing(data_split)
  data_train <- training(data_split)
  
  recipe_data <- recipe(death_event~. , data = data_train) %>%
    themis::step_downsample(death_event) %>% #balance dataset by death_event
    step_dummy(all_nominal(), -all_outcomes()) %>% #convert all nominal variables to dummy variables
    step_zv(all_numeric()) %>% #remove all variables with zero variance
    step_normalize(all_numeric()) %>% #scale numeric variables
    prep() #apply recipe
  
  train_prep <- juice(recipe_data)
  test_proc <- bake(recipe_data, new_data = data_test) #apply recipe to test data
  
  head(train_prep)
  glm_model <- train(death_event ~ ., data=train_prep, method="glm")
  this_impt_vars <- varImp(glm_model)
  
  if(exe_i == 1) impt_vars <- this_impt_vars
  else impt_vars <- rbind(impt_vars, this_impt_vars)
}

mean_impt_vars <- this_impt_vars

temp <- replicate(11,0)
for(exe_j in 1:execution_number){
  temp <- temp + impt_vars[exe_j,]$importance[,1]
}

mean_impt_vars$importance[,1] <- temp / execution_number

target_png <- paste("./results/","my_feature_ranking_",execution_number,"_exec.png", sep = "")
png(target_png, width = 850*2, height = 550*2)
ggplot(mean_impt_vars) +
  geom_col(fill = "steelblue") +
  ylim(0,100) +
  theme(text = element_text(size = 30))
while(length(dev.list()))  dev.off()
cat("plot saved in: ", target_png)
