# nn.R
#
# William John Zywiec
# The George Washington University
#
# ...

NN <- function(ensemble.size, source.directory, training.directory) {

  # load packages
  library(keras)

  # set variables
  deck.size <- 1e+04
  batch.size <- 512
  epochs <- 1000
  neurons <- 512
  validation.split <- 0.2

  # load functions
  source(paste0(source.directory, '/tabulate.R'))
  source(paste0(source.directory, '/subset.R'))
  source(paste0(source.directory, '/generate.R'))
  source(paste0(source.directory, '/build.R'))
  source(paste0(source.directory, '/model.R'))
  source(paste0(source.directory, '/fit.R'))
  source(paste0(source.directory, '/plot.R'))

  # load models
  dir.create(paste0(training.directory, '/hdf5'), showWarnings = FALSE)
  setwd(paste0(training.directory, '/hdf5'))
  hdf5.files <- list.files(pattern = '\\.h5$')

  # load data
  setwd(training.directory)
  Tabulate()
    
  if (length(hdf5.files) < ensemble.size) {

    # generate data
    if (length(hdf5.files) == 0) {
      if (file.exists('data_set.csv')) {
        if (nrow(data.set) < deck.size) {
          Generate(deck.size - nrow(data.set))
        }
      } else {
        Generate(deck.size)
      }
      model <- Model(neurons)
      history <- Fit(model, batch.size, epochs, validation.split)
      if (min(history$metrics$mean_absolute_error) > 0.001) {
        while (min(history$metrics$mean_absolute_error) > 0.001) {
          Generate(0.1 * deck.size)
          model <- Model(neurons)
          history <- Fit(model, batch.size, epochs, validation.split)
        }
      } else {
        cat('Training MAE = ', min(history$metrics$mean_absolute_error) %>% signif(2), '\n', sep = '')
      }
    }

    # build and train models
    setwd(paste0(training.directory, '/hdf5'))
    ensemble.model <- ensemble.history <- rep(list(0), length(hdf5.files))

    i <- length(hdf5.files) + 1 # counter

    while (length(hdf5.files) < ensemble.size) {
      ensemble.model[[i]] <- Model(neurons)
      ensemble.history[[i]] <- Fit(ensemble.model[[i]], batch.size, 5 * epochs, validation.split)
      results <- ensemble.model[[i]] %>% evaluate(test.df, test.data$keff, verbose = FALSE)
      svg(filename = paste0('model_', i, '.svg')) # save plot
      Plot(ensemble.history[[i]], paste0('model_', i))
      dev.off()
      save_model_hdf5(ensemble.model[[i]], paste0('model_', i, '.h5')) # save model
      hdf5.files <- list.files(pattern = '\\.h5$')
      i = i + 1
    }

  }

  # rebuild and test models
  setwd(paste0(training.directory, '/hdf5'))
  ensemble.model <- ensemble.history <- list()

  if (length(hdf5.files) >= ensemble.size * epochs + ensemble.size) {
    for (i in 1:ensemble.size) {
      test.metrics <- read.csv(paste0('model_', i, '_test.csv'))
      ensemble.model[[i]] <- load_model_hdf5(paste0('model_', i, '_', test.metrics$epoch[which.min(test.metrics$val.mae)], '.h5'))
    }
  } else {
    for (i in 1:ensemble.size) {
      ensemble.model[[i]] <- load_model_hdf5(hdf5.files[i])
      ensemble.history[[i]] <- Fit(ensemble.model[[i]], batch.size, epochs / 2, validation.split)
      svg(filename = paste0('model_', i, '_test.svg')) # save plot
      Plot(ensemble.history[[i]], paste0('model_', i, '_test'))
      dev.off()
    } 
    ensemble.model <- list()
    for (i in 1:ensemble.size) {
      ensemble.model[[i]] <- load_model_hdf5(paste0('model_', i, '_', which.min(ensemble.history[[i]]$metrics$val_mean_absolute_error), '.h5'))
      cat('Test MAE = ', min(ensemble.history[[i]]$metrics$val_mean_absolute_error) %>% signif(2), '\n', sep = '')
    }
  }

  ensemble.model <<- ensemble.model

}
