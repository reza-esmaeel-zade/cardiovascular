files <- list.files("./results/", pattern = ".*csv")
files <- files[-2:-3]

for (i in 1:length(files)) {
  df <- read.csv(paste('./results/',files[i], sep = ''))
  if(i == 1) data <- df
  else data <- rbind.data.frame(data, df)
}

names <- c('CART','dec_tree','knn','lin_reg','log_reg_2top','naive_bayes','oneR','RF','new_svm','svm_lin','svm_rad','xgb')
method <- rep(names, each = 2)[-10]

data <- cbind.data.frame(method, data)

# ggplot(dataset, aes())+
#   geom_bar()

new_data <- data[which(data$X == "mean"),]

col <- colnames(new_data)[3:9]
new_data <- new_data %>% 
  gather(col, key = "eval", value = "point") %>%
  select(c(1,3,4))

target_png <- './results/all_methods_comparision.png'
png(target_png,width = 1920, height = 1080)
ggplot(new_data, aes(eval, point))+
  geom_point()+
  facet_wrap(~method)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), text = element_text(size = 18))
while (length(dev.list())) dev.off()

