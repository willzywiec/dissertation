# nn.R
#
# William John Zywiec
# The George Washington University

NN <- function(data.set, ensemble.size, replot, source.dir, test.dir) {

  library(keras)

  # set variables
  batch.size <- 4096 # 32768
  epochs <- 1000 # 2000
  val.split <- 0.2

  # load functions
  source(paste0(source.dir, '/model.R'))
  source(paste0(source.dir, '/fit.R'))
  source(paste0(source.dir, '/plot.R'))

  build.dir <- paste0(test.dir, '/build')
  dir.create(build.dir, showWarnings = FALSE)

  setwd(build.dir)
  build.files <- list.files(pattern = '\\.h5$')

  # build and train ensemble model
  if (length(build.files) == 0) {
    model <- Model()
    history <- Fit(data.set, test.dir, model, batch.size, epochs, val.split)
    Plot('model-0', history)
  }

  ensemble.model <- ensemble.history <- rep(list(0), length(build.files))

  if (length(build.files) < ensemble.size) {
    for (i in (length(build.files) + 1):ensemble.size) {
      ensemble.model[[i]] <- Model()
      ensemble.history[[i]] <- Fit(data.set, test.dir, ensemble.model[[i]], batch.size, 5 * epochs, val.split)
      Plot(paste0('model-', i), ensemble.history[[i]])
      save_model_hdf5(ensemble.model[[i]], paste0('model-', i, '.h5')) # save model
    }
  } else if (replot == TRUE) {
    Plot('model-0')
    for (i in 1:ensemble.size) {
      Plot(paste0('model-', i))
    }
  }

  setwd(build.dir)
  build.files <- list.files(pattern = '\\.h5$')

  for (i in 1:ensemble.size) {
    ensemble.model[[i]] <- load_model_hdf5(build.files[i])
  }

  # rebuild and validate ensemble model
  rebuild.dir <- paste0(test.dir, '/rebuild')
  dir.create(rebuild.dir, showWarnings = FALSE)

  setwd(rebuild.dir)
  rebuild.files <- list.files(pattern = '\\.h5$')

  ensemble.history <- list()

  if (length(rebuild.files) < ensemble.size * epochs / 2) {
    for (i in 1:ensemble.size) {
      rebuild.files <- list.files(pattern = paste0('model-', i, '-.+\\.h5$'))
      if (length(rebuild.files) < epochs / 2) {
        ensemble.history[[i]] <- Fit(data.set, test.dir, ensemble.model[[i]], batch.size, epochs / 2, val.split, i)
        Plot(paste0('model-', i), ensemble.history[[i]])
      } else if (replot == TRUE) {
        Plot(paste0('model-', i))
      }
    }
  } else if (replot == TRUE) {
    for (i in 1:ensemble.size) {
      Plot(paste0('model-', i))
    }
  }

  # test ensemble model
  mae <- val.mae <- numeric()
  predictions <- matrix(nrow = nrow(data.set$test.df), ncol = ensemble.size)

  # subset test data
  # data.set$test.df <- as.data.frame(cbind(data.set$test.df, keff = data.set$test.data$keff))
  # data.set$test.df <- subset(data.set$test.df, keff >= 0.95)
  # data.set$test.df <- as.matrix(data.set$test.df[-27]) # convert data frame to matrix (Keras requirement)
  # data.set$test.data <- subset(data.set$test.data, keff >= 0.95)

  for (i in 1:ensemble.size) {
    metrics <- read.csv(paste0('model-', i, '.csv'))
    ensemble.model[[i]] <- load_model_hdf5(paste0('model-', i, '-', metrics$epoch[which.min(metrics$val.mae)], '.h5'))
    mae[i] <- metrics$mae[which.min(metrics$val.mae)]
    val.mae[i] <- min(metrics$val.mae)
    predictions[ , i] <- ensemble.model[[i]] %>% predict(data.set$test.df)
  }

  setwd(test.dir)
  
  training.data <- data.frame(mae = mae, val.mae = val.mae)
  write.csv(training.data, file = 'training-data.csv', row.names = FALSE)

  test.data <- data.set$test.data
  test.data$error <- rowMeans(predictions) - data.set$test.data$keff
  write.csv(test.data, file = 'test-data.csv', row.names = FALSE)

  cat('Training MAE = ', mean(mae) %>% sprintf('%.5f', .), '\n', sep = '')
  cat('Validation MAE = ', mean(val.mae) %>% sprintf('%.5f', .), '\n', sep = '')
  cat('Test MAE = ', mean(abs(rowMeans(predictions) - data.set$test.data$keff)) %>% sprintf('%.5f', .), '\n', sep = '')

  return(ensemble.model)

}
