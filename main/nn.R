# nn.R
#
# William John Zywiec
# The George Washington University
#
# ...

NN <- function(ensemble.size, learning.rate) {

  # load packages
  library(ggplot2)
  library(keras)

  # set variables
  batch.size <- 4096 # 32768
  epochs <- 1000
  val.split <- 0.2

  # load functions
  source(paste0(source.dir, '/tabulate.R'))
  source(paste0(source.dir, '/model.R'))
  source(paste0(source.dir, '/fit.R'))
  source(paste0(source.dir, '/plot.R'))
  source(paste0(source.dir, '/subset.R'))

  setwd(test.dir)

  # delete old files
  unlink(paste0(test.dir, '/hdf5'), recursive = TRUE)
  files <- list.files()
  for (i in 1:length(files)) {
    if (files[i] != 'data-set.csv' && files[i] != 'training-data.csv' && files[i] != 'training-df.csv' && files[i] != 'test-data.csv' && files[i] != 'test-df.csv') {
      file.remove(files[i])
    }
  }

  dir.create(paste0(test.dir, '/hdf5'), showWarnings = FALSE)
  hdf5.files <- list.files(pattern = '\\.h5$')

  # tabulate data
  Tabulate()

  # build and train models
  if (length(hdf5.files) == 0) {
    model <- Model(learning.rate)
    history <- Fit(model, batch.size, epochs, val.split)
    Plot('model-0', history)
  }

  if (length(hdf5.files) < ensemble.size) {
    ensemble.model <- ensemble.history <- rep(list(0), length(hdf5.files))
    for (i in (length(hdf5.files) + 1):ensemble.size) {
      ensemble.model[[i]] <- Model(learning.rate)
      ensemble.history[[i]] <- Fit(ensemble.model[[i]], batch.size, 5 * epochs, val.split)
      Plot(paste0('model-', i), ensemble.history[[i]])
      save_model_hdf5(ensemble.model[[i]], paste0('model-', i, '.h5')) # save model
    }
  } else {
    Plot('model-0')
    for (i in 1:ensemble.size) {
      Plot(paste0('model-', i))
    }
  }

  # rebuild and validate models
  setwd(paste0(test.dir, '/hdf5'))
  hdf5.files <- list.files(pattern = '\\.h5$')

  ensemble.model <- ensemble.history <- list()

  if (length(hdf5.files) < ensemble.size * epochs / 2) {
    for (i in 1:ensemble.size) {
      setwd(test.dir)
      hdf5.files <- list.files(pattern = '\\.h5$')
      ensemble.model[[i]] <- load_model_hdf5(hdf5.files[i])
      setwd(paste0(test.dir, '/hdf5'))
      hdf5.files <- list.files(pattern = paste0('model-', i, '-.+\\.h5$'))
      if (length(hdf5.files) < epochs / 2) {
        ensemble.history[[i]] <- Fit(ensemble.model[[i]], batch.size, epochs / 2, val.split, i)
        Plot(paste0('model-', i), ensemble.history[[i]])
        ensemble.model[[i]] <- load_model_hdf5(paste0('model-', i, '-', which.min(ensemble.history[[i]]$metrics$val_mean_absolute_error), '.h5'))
      } else {
        Plot(paste0('model-', i))
      }
    }
  } else {
    for (i in 1:ensemble.size) {
      Plot(paste0('model-', i))
    }
  }

  mae <- val.mae <- test.mae <- numeric()
  predictions <- list()

  for (i in 1:ensemble.size) {
    metrics <- read.csv(paste0('model-', i, '.csv'))
    ensemble.model[[i]] <- load_model_hdf5(paste0('model-', i, '-', metrics$epoch[which.min(metrics$val.mae)], '.h5'))
    mae[i] <- metrics$mae[which.min(metrics$val.mae)]
    val.mae[i] <- min(metrics$val.mae)
    predictions[[i]] <- ensemble.model[[i]] %>% predict(test.df)
    test.mae[i] <- mean(abs(rowMeans(as.data.frame(predictions, col.names = 1:i)) - test.data$keff))
  }

  cat('Training MAE = ', mean(mae) %>% sprintf('%.5f', .), '\n', sep = '')
  cat('Validation MAE = ', mean(val.mae) %>% sprintf('%.5f', .), '\n', sep = '')
  cat('Test MAE = ', mean(abs(rowMeans(as.data.frame(predictions, col.names = 1:length(ensemble.model))) - test.data$keff)) %>% sprintf('%.5f', .), '\n', sep = '')

  ensemble.model <<- ensemble.model
  test.mae <<- test.mae

  setwd(test.dir)
  write.csv(test.mae, file = 'test-mae.csv', row.names = FALSE)

}
