# fit.R
#
# William John Zywiec
# The George Washington University

Fit <- function(data.set, model, batch.size, epochs, val.split, remodel.dir, i) {

  # library(keras)
  # library(magrittr)

  if (missing(i)) {
    early.stop <- callback_early_stopping(monitor = 'val_mean_absolute_error', patience = 500)
    model %>% fit(
      data.set$training.df,
      data.set$training.data$keff,
      batch_size = batch.size,
      epochs = epochs,
      validation_split = val.split,
      verbose = FALSE,
      callbacks = c(early.stop))
  } else {
    checkpoint <- callback_model_checkpoint(paste0(remodel.dir, '/', i, '-{epoch:1d}.h5'), monitor = 'mean_absolute_error')
    model %>% fit(
      data.set$training.df,
      data.set$training.data$keff,
      batch_size = batch.size,
      epochs = epochs,
      validation_split = val.split,
      verbose = FALSE,
      callbacks = c(checkpoint))
  }

}
