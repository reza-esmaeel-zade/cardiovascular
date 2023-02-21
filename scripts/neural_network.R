library(keras)

source("scripts/my_data_split.R")
nn_train_data<-as.matrix(scale(train_data[,1:11]))
nn_test_data<-as.matrix(scale(test_data[,1:11]))

train_data$death_event <- as.numeric(train_data$death_event)
test_data$death_event <- as.numeric(test_data$death_event)

nn_train_data_label<-to_categorical(train_data[,12])
nn_test_data_label<-to_categorical(test_data[,12])

nnmodel<-keras_model_sequential() %>%
  layer_dense(32,activation = 'relu',input_shape = c(11)) %>%
  layer_dense(16,activation = 'relu') %>%
  layer_dense(16,activation = 'selu') %>%
  layer_dense(2,activation = 'sigmoid')

nnmodel %>% 
  compile(
  optimizer='rmsprop',
  loss='binary_crossentropy',
  metrics=c('accuracy')
)

nnmodel %>%
  fit(nn_train_data,nn_train_data_label, epochs = 100, batch_size = 16)

nn_acc <- nnmodel %>% evaluate(nn_test_data,nn_test_data_label)
# nn_acc <- nnmodel %>% evaluate(nn_train_data,nn_train_data_label)

nn_acc[[2]]