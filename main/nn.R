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

    # build and train models
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
      if (min(history$metrics$mean_absolute_error) > 0.5) {
        while (min(history$metrics$mean_absolute_error) > 0.5) {
          Generate(0.1 * deck.size)
          model <- Model(neurons)
          history <- Fit(model, batch.size, epochs, validation.split)
        }
      } else {
        cat('Training MAE = ', min(history$metrics$mean_absolute_error) %>% signif(2), '\n', sep = '')
      }
    }

    # rebuild and test models
    setwd(paste0(training.directory, '/hdf5'))
    ensemble.model <- ensemble.history <- rep(list(0), length(hdf5.files))

    i <- length(hdf5.files) + 1 # counter

    while (length(hdf5.files) < ensemble.size) {
      ensemble.model[[i]] <- Model(neurons)
      ensemble.history[[i]] <- Fit(ensemble.model[[i]], batch.size, 5 * epochs, validation.split)
      results <- ensemble.model[[i]] %>% evaluate(test.df, test.data$keff, verbose = FALSE)
      svg(filename = paste0('model_', i, '.svg')) # save plot
      Plot(ensemble.history[[i]], paste('Model', i))
      dev.off()
      save_model_hdf5(ensemble.model[[i]], paste0('model_', i, '.h5')) # save model
      cat('Test MAE = ', min(results$mean_absolute_error) %>% signif(2), '\n', sep = '')
      hdf5.files <- list.files(pattern = '\\.h5$')
      i = i + 1
    }

  }

  # rebuild models
  setwd(paste0(training.directory, '/hdf5'))
  ensemble.model <- ensemble.history <- list()

  for (i in 1:ensemble.size) {
    ensemble.model[[i]] <- load_model_hdf5(hdf5.files[i])
    ensemble.history[[i]] <- Fit(ensemble.model[[i]], batch.size, 100, validation.split, i)
    svg(filename = paste0('model_', i, '_1.svg')) # save plot
    Plot(ensemble.history[[i]], paste('Model', i))
    dev.off()
  } 

  ensemble.model <- list()

  for (i in 1:ensemble.size) {
    ensemble.model[[i]] <- load_model_hdf5(paste0('model_', i, '_', which.min(ensemble.history[[i]]$metrics$mean_absolute_error), '.h5'))
  }

  ensemble.model <<- ensemble.model
  ensemble.history <<- ensemble.history

}
