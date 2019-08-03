# model.R
#
# William John Zywiec
# The George Washington University
#
# ...

Model <- function() {

  model <- keras_model_sequential() %>%
    layer_dense(
      units = 8192,
      activation = 'relu',
      input_shape = dim(training.df)[2]) %>%
    layer_dense(
      units = 128,
      activation = 'relu') %>%
    layer_dense(
      units = 256,
      activation = 'relu') %>%
    layer_dense(
      units = 512,
      activation = 'relu') %>%
    layer_dense(
      units = 256,
      activation = 'relu') %>%
    layer_dense(
      units = 8,
      activation = 'relu') %>%
    layer_dense(
      units = 1,
      activation = 'linear')

  model %>% compile(
    loss = 'mean_squared_error',
    optimizer = optimizer_adam(),
    metrics = c('mean_absolute_error'))

}
