# model.R
#
# William John Zywiec
# The George Washington University
#
# ...

Model <- function(neurons) {

	model <- keras_model_sequential() %>%
		layer_dense(
			units = neurons,
			activation = 'relu',
			input_shape = dim(training.df)[2]) %>%
		layer_dense(
			units = neurons,
			activation = 'relu') %>%
		layer_dense(
			units = neurons / 2,
			activation = 'relu') %>%
		layer_dense(
			units = 1,
			activation = 'linear')

	model %>% compile(
		loss = 'mse',
		optimizer = optimizer_adamax(),
		metrics = c('mean_absolute_error'))

}
