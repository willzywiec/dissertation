# nn.R
#
# William John Zywiec
# The George Washington University
#
# ...

NN <- function(ensemble.size) {

  # load packages
  library(keras)

  # set variables
  batch.size <- 4096
  epochs <- 1000
  val.split <- 0.2

  # load functions
  source(paste0(source.dir, '/tabulate.R'))
  source(paste0(source.dir, '/model.R'))
  source(paste0(source.dir, '/fit.R'))
  source(paste0(source.dir, '/plot.R'))
  source(paste0(source.dir, '/subset.R'))

  # load models
  dir.create(paste0(test.dir, '/hdf5'), showWarnings = FALSE)
  setwd(test.dir)
  hdf5.files <- list.files(pattern = '\\.h5$')

  # tabulate data
  setwd(test.dir)
  Tabulate()

  # build and train models
  if (length(hdf5.files) == 0) {
    model <- Model()
    history <- Fit(model, batch.size, epochs, val.split)
    Plot('model-0', history)
    cat('Training MAE = ', min(history$metrics$mean_absolute_error) %>% sprintf('%.5f', .), '\n', sep = '')
    cat('Validation MAE = ', min(history$metrics$val_mean_absolute_error) %>% sprintf('%.5f', .), '\n', sep = '')
    if (min(history$metrics$mean_absolute_error) > 0.01) {
      stop('Stopped (Training MAE > 0.01)\n')
    }
  }

  if (length(hdf5.files) < ensemble.size) {
    ensemble.model <- ensemble.history <- rep(list(0), length(hdf5.files))
    for (i in (length(hdf5.files) + 1):ensemble.size) {
      Subset(na.omit(data.set))
      ensemble.model[[i]] <- Model()
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
    for (i in (round(length(hdf5.files) * 2 / epochs) + 1):ensemble.size) {
      setwd(test.dir)
      hdf5.files <- list.files(pattern = '\\.h5$')
      ensemble.model[[i]] <- load_model_hdf5(hdf5.files[i])
      setwd(paste0(test.dir, '/hdf5'))
      ensemble.history[[i]] <- Fit(ensemble.model[[i]], batch.size, epochs / 2, val.split, i)
      Plot(paste0('model-', i), ensemble.history[[i]])
      ensemble.model[[i]] <- load_model_hdf5(paste0('model-', i, '-', which.min(ensemble.history[[i]]$metrics$val_mean_absolute_error), '.h5'))
    }
  } else {
    for (i in 1:ensemble.size) {
      Plot(paste0('model-', i))
    }
  }

  for (i in 1:ensemble.size) {
    metrics <- read.csv(paste0('model-', i, '.csv'))
    ensemble.model[[i]] <- load_model_hdf5(paste0('model-', i, '-', metrics$epoch[which.min(metrics$val.mae)], '.h5'))
    cat('Model ', i,
      ': Validation MAE = ', min(metrics$val.mae) %>% sprintf('%.5f', .),
      ' (Epoch ', which.min(metrics$val.mae), ')\n', sep = '')
  }

  # test models
  for (i in 1:ensemble.size) {
    test <- ensemble.model[[i]] %>% predict(test.df)
    test.mae <- mean(abs(test - test.data$keff))
    cat('Model ', i,
      ': Test MAE = ', test.mae %>% sprintf('%.5f', .), '\n', sep = '')
  }

  ensemble.model <<- ensemble.model

}
