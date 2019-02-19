# fit.R
#
# William John Zywiec
# The George Washington University
#
# ...

Fit <- function(model, batch.size, epochs, validation.split, i) {

	early.stop <- callback_early_stopping(monitor = 'val_mean_absolute_error', patience = 1000)

	if (missing(i)) {
		model %>% fit(
			training.df,
			training.data$keff,
			batch_size = batch.size,
			epochs = epochs,
			validation_split = validation.split,
			verbose = FALSE,
			callbacks = early.stop)
	} else {
		checkpoint <- callback_model_checkpoint(paste0(training.directory, '/hdf5/model_', i, '_{epoch:1d}.h5'), monitor = 'mean_absolute_error')
		model %>% fit(
			training.df,
			training.data$keff,
			batch_size = batch.size,
			epochs = epochs,
			validation_split = validation.split,
			verbose = FALSE,
			callbacks = checkpoint)
	}

}
