# model.R
#
# William John Zywiec
# The George Washington University

Model <- function(dataset, layers, loss, opt.alg, learning.rate) {

  # library(keras)
  # library(magrittr)

  layers <- strsplit(layers, '-') %>% unlist() %>% as.integer()

  if (length(layers) == 1) {
    model <- keras_model_sequential() %>%
    layer_dense(units = layers[1], activation = 'relu', input_shape = dim(dataset$training.df)[2]) %>%
    layer_dense(units = 1, activation = 'linear')
  } else if (length(layers) == 2) {
    model <- keras_model_sequential() %>%
    layer_dense(units = layers[1], activation = 'relu', input_shape = dim(dataset$training.df)[2]) %>%
    layer_dense(units = layers[2], activation = 'relu') %>%
    layer_dense(units = 1, activation = 'linear')
  } else if (length(layers) == 3) {
    model <- keras_model_sequential() %>%
    layer_dense(units = layers[1], activation = 'relu', input_shape = dim(dataset$training.df)[2]) %>%
    layer_dense(units = layers[2], activation = 'relu') %>%
    layer_dense(units = layers[3], activation = 'relu') %>%
    layer_dense(units = 1, activation = 'linear')
  } else if (length(layers) == 4) {
    model <- keras_model_sequential() %>%
    layer_dense(units = layers[1], activation = 'relu', input_shape = dim(dataset$training.df)[2]) %>%
    layer_dense(units = layers[2], activation = 'relu') %>%
    layer_dense(units = layers[3], activation = 'relu') %>%
    layer_dense(units = layers[4], activation = 'relu') %>%
    layer_dense(units = 1, activation = 'linear')
  } else if (length(layers) == 5) {
    model <- keras_model_sequential() %>%
    layer_dense(units = layers[1], activation = 'relu', input_shape = dim(dataset$training.df)[2]) %>%
    layer_dense(units = layers[2], activation = 'relu') %>%
    layer_dense(units = layers[3], activation = 'relu') %>%
    layer_dense(units = layers[4], activation = 'relu') %>%
    layer_dense(units = layers[5], activation = 'relu') %>%
    layer_dense(units = 1, activation = 'linear')
  } else if (length(layers) == 6) {
    model <- keras_model_sequential() %>%
    layer_dense(units = layers[1], activation = 'relu', input_shape = dim(dataset$training.df)[2]) %>%
    layer_dense(units = layers[2], activation = 'relu') %>%
    layer_dense(units = layers[3], activation = 'relu') %>%
    layer_dense(units = layers[4], activation = 'relu') %>%
    layer_dense(units = layers[5], activation = 'relu') %>%
    layer_dense(units = layers[6], activation = 'relu') %>%
    layer_dense(units = 1, activation = 'linear')
  } else if (length(layers) == 7) {
    model <- keras_model_sequential() %>%
    layer_dense(units = layers[1], activation = 'relu', input_shape = dim(dataset$training.df)[2]) %>%
    layer_dense(units = layers[2], activation = 'relu') %>%
    layer_dense(units = layers[3], activation = 'relu') %>%
    layer_dense(units = layers[4], activation = 'relu') %>%
    layer_dense(units = layers[5], activation = 'relu') %>%
    layer_dense(units = layers[6], activation = 'relu') %>%
    layer_dense(units = layers[7], activation = 'relu') %>%
    layer_dense(units = 1, activation = 'linear')
  } else if (length(layers) == 8) {
    model <- keras_model_sequential() %>%
    layer_dense(units = layers[1], activation = 'relu', input_shape = dim(dataset$training.df)[2]) %>%
    layer_dense(units = layers[2], activation = 'relu') %>%
    layer_dense(units = layers[3], activation = 'relu') %>%
    layer_dense(units = layers[4], activation = 'relu') %>%
    layer_dense(units = layers[5], activation = 'relu') %>%
    layer_dense(units = layers[6], activation = 'relu') %>%
    layer_dense(units = layers[7], activation = 'relu') %>%
    layer_dense(units = layers[8], activation = 'relu') %>%
    layer_dense(units = 1, activation = 'linear')
  }

  if (opt.alg == 'adadelta') {
    model %>% compile(
      loss = loss,
      optimizer = optimizer_adadelta(lr = learning.rate),
      metrics = c('mean_absolute_error'))
  } else if (opt.alg == 'adagrad') {
    model %>% compile(
      loss = loss,
      optimizer = optimizer_adagrad(lr = learning.rate),
      metrics = c('mean_absolute_error'))
  } else if (opt.alg == 'adam') {
    model %>% compile(
      loss = loss,
      optimizer = optimizer_adam(lr = learning.rate),
      metrics = c('mean_absolute_error'))
  } else if (opt.alg == 'adamax') {
    model %>% compile(
      loss = loss,
      optimizer = optimizer_adamax(lr = learning.rate),
      metrics = c('mean_absolute_error'))
  } else if (opt.alg == 'nadam') {
    model %>% compile(
      loss = loss,
      optimizer = optimizer_nadam(lr = learning.rate),
      metrics = c('mean_absolute_error'))
  } else if (opt.alg == 'rmsprop') {
    model %>% compile(
      loss = loss,
      optimizer = optimizer_rmsprop(lr = learning.rate),
      metrics = c('mean_absolute_error'))
  }

}
