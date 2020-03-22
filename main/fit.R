# fit.R
#
# William John Zywiec
# The George Washington University

Fit <- function(data.set, test.dir, model, batch.size, epochs, val.split, i) {

  if (missing(i)) {
    early.stop <- callback_early_stopping(monitor = "val_mean_absolute_error", patience = 250)
    model %>% fit(
      data.set$training.df,
      data.set$training.data$keff,
      batch_size = batch.size,
      epochs = epochs,
      validation_split = val.split,
      verbose = FALSE,
      callbacks = early.stop)
  } else {
    checkpoint <- callback_model_checkpoint(paste0(test.dir, "/rebuild/model-", i, "-{epoch:1d}.h5"), monitor = "mean_absolute_error")
    model %>% fit(
      data.set$training.df,
      data.set$training.data$keff,
      batch_size = batch.size,
      epochs = epochs,
      validation_split = val.split,
      verbose = FALSE,
      callbacks = checkpoint)
  }

}
