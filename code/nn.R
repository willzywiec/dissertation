# nn.R
#
# William John Zywiec
# The George Washington University

NN <- function(data.set, batch.size, ensemble.size, epochs, layers, lr, replot, code.dir, test.dir) {

  library(keras)

  # build custom loss function
  SSE <- function(y_true, y_pred) k_sum(k_pow(y_true - y_pred, 2))

  # set variables
  loss <- 'mean_squared_error' # SSE
  val.split <- 0.2

  # load functions
  source(paste0(code.dir, '/model.R'))
  source(paste0(code.dir, '/fit.R'))
  source(paste0(code.dir, '/plot.R'))

  model.dir <- paste0(test.dir, '/model')
  dir.create(model.dir, recursive = TRUE, showWarnings = FALSE)

  setwd(model.dir)

  model.files <- list.files(pattern = '\\.h5$')

  # build and train ensemble model
  if (length(model.files) == 0) {
    model <- Model(layers, loss, lr)
    history <- Fit(data.set, model, batch.size, epochs, val.split)
    Plot('model-0', history)
  }

  ensemble.model <- ensemble.history <- rep(list(0), length(model.files))

  if (length(model.files) < ensemble.size) {
    for (i in (length(model.files) + 1):ensemble.size) {
      ensemble.model[[i]] <- Model(layers, loss, lr)
      ensemble.history[[i]] <- Fit(data.set, ensemble.model[[i]], batch.size, 5 * epochs, val.split)
      Plot(paste0('model-', i), ensemble.history[[i]])
      save_model_hdf5(ensemble.model[[i]], paste0('model-', i, '.h5'))
    }
  } else if (replot == TRUE) {
    Plot('model-0')
    for (i in 1:ensemble.size) {
      Plot(paste0('model-', i))
    }
  }

  setwd(model.dir)

  model.files <- list.files(pattern = '\\.h5$')

  for (i in 1:ensemble.size) {
    ensemble.model[[i]] <- load_model_hdf5(model.files[i], custom_objects = c(loss = loss))
  }

  # rebuild ensemble model
  remodel.dir <- paste0(test.dir, '/remodel')
  dir.create(remodel.dir, showWarnings = FALSE)

  setwd(remodel.dir)

  remodel.files <- list.files(pattern = '\\.h5$')

  ensemble.history <- list()

  if (length(remodel.files) < ensemble.size * epochs / 2) {
    for (i in 1:ensemble.size) {
      remodel.files <- list.files(pattern = paste0('model-', i, '-.+\\.h5$'))
      if (length(remodel.files) < epochs / 2) {
        ensemble.history[[i]] <- Fit(data.set, ensemble.model[[i]], batch.size, epochs / 2, val.split, remodel.dir, i)
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

  # validate and test ensemble model
  mae <- val.mae <- test.mae <- x <- y <- numeric()
  training.pred <- matrix(nrow = nrow(data.set$training.df), ncol = ensemble.size)
  test.pred <- matrix(nrow = nrow(data.set$test.df), ncol = ensemble.size)

  for (i in 1:ensemble.size) {
    metrics <- read.csv(paste0('model-', i, '.csv'))
    ensemble.model[[i]] <- load_model_hdf5(paste0('model-', i, '-', metrics$epoch[which.min(metrics$mae + metrics$val.mae)], '.h5'), custom_objects = c(loss = loss))
    mae[i] <- metrics$mae[which.min(metrics$mae + metrics$val.mae)]
    val.mae[i] <- metrics$val.mae[which.min(metrics$mae + metrics$val.mae)]
    training.pred[ , i] <- ensemble.model[[i]] %>% predict(data.set$training.df)
    test.pred[ , i] <- ensemble.model[[i]] %>% predict(data.set$test.df)
    test.mae[i] <- mean(abs(data.set$test.data$keff - test.pred[ , i]))
    x[i] <- mean(abs(data.set$training.data$keff - rowMeans(training.pred, na.rm = TRUE)))
    y[i] <- mean(abs(data.set$test.data$keff - rowMeans(test.pred, na.rm = TRUE)))
  }

  library(ggplot2)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x = 1:length(x), y = x)) + geom_line() + geom_line(aes(x = 1:length(y), y = y))

  ggsave('fig-x.jpg', dpi = 4000, height = 3, width = 6.3)

  setwd(test.dir)
  
  model.data <- data.frame(mae = mae, val.mae = val.mae, test.mae = test.mae)
  write.csv(model.data, file = 'model-data.csv', row.names = FALSE)

  test.data <- data.set$test.data
  test.data$error <- data.set$test.data$keff - rowMeans(test.pred)
  write.csv(test.data, file = 'test-data.csv', row.names = FALSE)

  cat('Mean Training MAE = ', mean(mae) %>% sprintf('%.6f', .), '\n', sep = '')
  cat('Mean Validation MAE = ', mean(val.mae) %>% sprintf('%.6f', .), '\n', sep = '')
  cat('Mean Test MAE = ', mean(test.mae) %>% sprintf('%.6f', .), '\n-\n', sep = '')
  cat('Ensemble Training MAE = ', mean(abs(data.set$training.data$keff - rowMeans(training.pred))) %>% sprintf('%.6f', .), '\n', sep = '')
  cat('Ensemble Test MAE = ', mean(abs(data.set$test.data$keff - rowMeans(test.pred))) %>% sprintf('%.6f', .), '\n', sep = '')

  return(ensemble.model)

}
