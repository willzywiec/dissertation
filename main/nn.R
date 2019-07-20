# nn.R
#
# William John Zywiec
# The George Washington University
#
# ...

NN <- function(ensemble.size, source.dir, training.dir) {

  # load packages
  library(keras)

  # set variables
  deck.size <- 1e+04
  batch.size <- 4096
  epochs <- 4000
  neurons <- 4096
  val.split <- 0.2

  # load functions
  source(paste0(source.dir, '/tabulate.R'))
  source(paste0(source.dir, '/subset.R'))
  source(paste0(source.dir, '/build.R'))
  source(paste0(source.dir, '/model.R'))
  source(paste0(source.dir, '/fit.R'))
  source(paste0(source.dir, '/plot.R'))

  # load models
  dir.create(paste0(training.dir, '/hdf5'), showWarnings = FALSE)
  setwd(paste0(training.dir, '/hdf5'))
  hdf5.files <- list.files(pattern = '\\.h5$')

  # tabulate data
  setwd(training.dir)
  Tabulate()

  # build and train models
  if (length(hdf5.files) == 0) {
    model <- Model(neurons)
    history <- Fit(model, batch.size, epochs, val.split)
    Plot(history, 'model')
    cat('Training MAE = ', min(history$metrics$mean_absolute_error) %>% sprintf('%.5f', .), '\n', sep = '')
    if (min(history$metrics$mean_absolute_error) > 0.01) {
    	cat('Stopped (Training MAE > 0.01)\n')
    }
  }

  if (length(hdf5.files) < ensemble.size) {
  	ensemble.model <- ensemble.history <- rep(list(0), length(hdf5.files))
    i <- length(hdf5.files) + 1 # counter
    while (length(hdf5.files) < ensemble.size) {
      setwd(paste0(training.dir, '/hdf5'))
      ensemble.model[[i]] <- Model(neurons)
      ensemble.history[[i]] <- Fit(ensemble.model[[i]], batch.size, 5 * epochs, val.split)
      results <- ensemble.model[[i]] %>% evaluate(test.df, test.data$keff, verbose = FALSE)
      Plot(ensemble.history[[i]], paste0('model-', i))
      save_model_hdf5(ensemble.model[[i]], paste0('model-', i, '.h5')) # save model
      hdf5.files <- list.files(pattern = '\\.h5$')
      i = i + 1
    }
  }

  # rebuild and validate models
  setwd(paste0(training.dir, '/hdf5'))
  ensemble.model <- ensemble.history <- list()

  if (length(hdf5.files) >= ensemble.size * epochs / 2 + ensemble.size) {
    for (i in 1:ensemble.size) {
      metrics <- read.csv(paste0('model-', i, '-val.csv'))
      ensemble.model[[i]] <- load_model_hdf5(paste0('model-', i, '-', metrics$epoch[which.min(metrics$val.mae)], '.h5'))
      cat('Model ', i,
        ': Validation MAE = ', min(metrics$val.mae) %>% sprintf('%.5f', .),
        ' (Epoch ', which.min(metrics$val.mae), ')\n', sep = '')
    }
  } else {
    for (i in 1:ensemble.size) {
      ensemble.model[[i]] <- load_model_hdf5(hdf5.files[i])
      ensemble.history[[i]] <- Fit(ensemble.model[[i]], batch.size, epochs / 2, val.split, i)
      Plot(ensemble.history[[i]], paste0('model-', i, '-val'))
    } 
    ensemble.model <- list()
    for (i in 1:ensemble.size) {
      ensemble.model[[i]] <- load_model_hdf5(paste0('model-', i, '-', which.min(ensemble.history[[i]]$metrics$val_mean_absolute_error), '.h5'))
      cat('Model ', i,
        ': Validation MAE = ', min(ensemble.history[[i]]$metrics$val_mean_absolute_error) %>% sprintf('%.5f', .),
        ' (Epoch ', which.min(ensemble.history[[i]]$metrics$val_mean_absolute_error), ')\n', sep = '')
    }
  }

  # test models
  for (i in 1:ensemble.size) {
    predictions <- ensemble.model[[i]] %>% predict(test.df)
    test.mae <- mean(abs(predictions - test.data$keff))
    cat('Model ', i,
      ': Test MAE = ', test.mae %>% sprintf('%.5f', .), '\n', sep = '')
  }

  ensemble.model <<- ensemble.model

}
