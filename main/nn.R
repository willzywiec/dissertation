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
  batch.size <- 512
  epochs <- 1000
  neurons <- 512
  val.split <- 0.2

  # load functions
  source(paste0(source.dir, '/tabulate.R'))
  source(paste0(source.dir, '/subset.R'))
  source(paste0(source.dir, '/generate.R'))
  source(paste0(source.dir, '/build.R'))
  source(paste0(source.dir, '/model.R'))
  source(paste0(source.dir, '/fit.R'))
  source(paste0(source.dir, '/plot.R'))

  # load models
  dir.create(paste0(training.dir, '/hdf5'), showWarnings = FALSE)
  setwd(paste0(training.dir, '/hdf5'))
  hdf5.files <- list.files(pattern = '\\.h5$')

  # load data
  setwd(training.dir)
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
      history <- Fit(model, batch.size, epochs, val.split)
      svg(filename = 'model_0.svg') # save plot
      Plot(history, 'model_0')
      dev.off()
      if (min(history$metrics$mean_absolute_error) > 0.05) {
        cat('Training MAE = ', min(history$metrics$mean_absolute_error) %>% signif(2), '\n', sep = '')
        while (min(history$metrics$mean_absolute_error) > 0.05) {
          Generate(0.1 * deck.size)
          model <- Model(neurons)
          history <- Fit(model, batch.size, epochs, val.split)
          svg(filename = 'model_0.svg') # save plot
      		Plot(history, 'model_0')
      		dev.off()
        }
      }
    }

    # build and train models
    cat('Training MAE = ', min(history$metrics$mean_absolute_error) %>% signif(2), '\n', sep = '')
    setwd(paste0(training.dir, '/hdf5'))
    ensemble.model <- ensemble.history <- rep(list(0), length(hdf5.files))

    i <- length(hdf5.files) + 1 # counter

    while (length(hdf5.files) < ensemble.size) {
      ensemble.model[[i]] <- Model(neurons)
      ensemble.history[[i]] <- Fit(ensemble.model[[i]], batch.size, 5 * epochs, val.split)
      results <- ensemble.model[[i]] %>% evaluate(test.df, test.data$keff, verbose = FALSE)
      svg(filename = paste0('model_', i, '.svg')) # save plot
      Plot(ensemble.history[[i]], paste0('model_', i))
      dev.off()
      save_model_hdf5(ensemble.model[[i]], paste0('model_', i, '.h5')) # save model
      hdf5.files <- list.files(pattern = '\\.h5$')
      i = i + 1
    }

  }

  # rebuild and validate models
  setwd(paste0(training.dir, '/hdf5'))
  ensemble.model <- ensemble.history <- list()

  if (length(hdf5.files) >= ensemble.size * epochs / 2 + ensemble.size) {
    for (i in 1:ensemble.size) {
      metrics <- read.csv(paste0('model_', i, '_val.csv'))
      ensemble.model[[i]] <- load_model_hdf5(paste0('model_', i, '_', metrics$epoch[which.min(metrics$val.mae)], '.h5'))
      cat('Model ', i,
        ': Validation MAE = ', min(metrics$val.mae) %>% signif(2),
        ' (Epoch ', which.min(metrics$val.mae), ')\n', sep = '')
    }
  } else {
    for (i in 1:ensemble.size) {
      ensemble.model[[i]] <- load_model_hdf5(hdf5.files[i])
      ensemble.history[[i]] <- Fit(ensemble.model[[i]], batch.size, epochs / 2, val.split, i)
      svg(filename = paste0('model_', i, '_val.svg')) # save plot
      Plot(ensemble.history[[i]], paste0('model_', i, '_val'))
      dev.off()
    } 
    ensemble.model <- list()
    for (i in 1:ensemble.size) {
      ensemble.model[[i]] <- load_model_hdf5(paste0('model_', i, '_', which.min(ensemble.history[[i]]$metrics$val_mean_absolute_error), '.h5'))
      cat('Model ', i,
        ': Validation MAE = ', min(ensemble.history[[i]]$metrics$val_mean_absolute_error) %>% signif(2),
        ' (Epoch ', which.min(ensemble.history[[i]]$metrics$val_mean_absolute_error), ')\n', sep = '')
    }
  }

  # test models
  for (i in 1:ensemble.size) {
    predictions <- ensemble.model[[i]] %>% predict(test.df)
    test.mae <- mean(abs(predictions - test.data$keff))
    cat('Model ', i,
      ': Test MAE = ', test.mae %>% signif(2), '\n', sep = '')
  }

  ensemble.model <<- ensemble.model

}
