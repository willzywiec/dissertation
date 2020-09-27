# nn.R
#
# William John Zywiec
# The George Washington University

NN <- function(data.set, batch.size, ensemble.size, epochs, layers, loss, lr, val.split, code.dir, test.dir) {

  library(ggplot2)
  library(keras)
  library(magrittr)
  library(scales)

  # build custom loss function
  if (loss == 'sse') {
    SSE <- function(y_true, y_pred) k_sum(k_pow(y_true - y_pred, 2))
    loss <- SSE
  }

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
    Plot('0', history)
  } else {
    Plot('0')
  }

  ensemble.model <- ensemble.history <- rep(list(0), length(model.files))

  if (length(model.files) < ensemble.size) {
    for (i in (length(model.files) + 1):ensemble.size) {
      ensemble.model[[i]] <- Model(layers, loss, lr)
      ensemble.history[[i]] <- Fit(data.set, ensemble.model[[i]], batch.size, 5 * epochs, val.split)
      Plot(i, ensemble.history[[i]])
      save_model_hdf5(ensemble.model[[i]], paste0(i, '.h5'))
    }
  } else {
    for (i in 1:ensemble.size) {
      Plot(i)
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
      remodel.files <- list.files(pattern = paste0(i, '-.+\\.h5$'))
      if (length(remodel.files) < epochs / 2) {
        ensemble.history[[i]] <- Fit(data.set, ensemble.model[[i]], batch.size, epochs / 2, val.split, remodel.dir, i)
        Plot(i, ensemble.history[[i]])
      } else {
        Plot(i)
      }
    }
  } else {
    for (i in 1:ensemble.size) {
      Plot(i)
    }
  }

  # test ensemble model
  mae <- val.mae <- test.mae <- ensemble.mae <- numeric()
  test.pred <- matrix(nrow = nrow(data.set$test.df), ncol = ensemble.size)

  for (i in 1:ensemble.size) {
    metrics <- read.csv(paste0(i, '.csv'))
    ensemble.model[[i]] <- load_model_hdf5(paste0(i, '-', metrics$epoch[which.min(metrics$mae + metrics$val.mae)], '.h5'), custom_objects = c(loss = loss))
    mae[i] <- metrics$mae[which.min(metrics$mae + metrics$val.mae)]
    val.mae[i] <- metrics$val.mae[which.min(metrics$mae + metrics$val.mae)]
    test.pred[ , i] <- ensemble.model[[i]] %>% predict(data.set$test.df)
    test.mae[i] <- mean(abs(data.set$test.data$keff - test.pred[ , i]))
    ensemble.mae[i] <- mean(abs(data.set$test.data$keff - rowMeans(test.pred, na.rm = TRUE)))
  }

  setwd(test.dir)

  ggplot(as.data.frame(ensemble.mae), aes(x = 1:length(ensemble.mae), y = ensemble.mae)) +
    ggtitle('test data') +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = pretty_breaks(), limits = c(1, ensemble.size)) +
    scale_y_continuous(breaks = c(2e-04, 2.5e-04, 3e-04, 3.5e-04), limits = c(2e-04, 3.5e-04), labels = function(x) format(x, scientific = TRUE)) +
    theme(
      legend.position = 'bottom',
      legend.spacing.x = unit(0.2, 'cm'),
      legend.text = element_text(size = 10),
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5)) +
    xlab('neural networks') +
    ylab(bquote(mean ~ absolute ~ error ~ (k[eff]))) +
    annotate(
      geom = 'text',
      x = which.min(ensemble.mae),
      y = min(ensemble.mae),
      vjust = 1.9,
      label = format(min(ensemble.mae), digits = 3, scientific = TRUE),
      family = 'serif',
      size = 3.5)

  ggsave('model.jpg', dpi = 1000) %>% suppressMessages()
  
  model.data <- data.frame(mae = mae, val.mae = val.mae, test.mae = test.mae)
  write.csv(model.data, file = 'model.csv', row.names = FALSE)

  test.data <- data.set$test.data
  test.data$error <- data.set$test.data$keff - rowMeans(test.pred)
  write.csv(test.data, file = 'test-data.csv', row.names = FALSE)

  cat('Mean Training MAE = ', mean(mae) %>% sprintf('%.6f', .), '\n', sep = '')
  cat('Mean Cross-Validation MAE = ', mean(val.mae) %>% sprintf('%.6f', .), '\n', sep = '')
  cat('Mean Test MAE = ', mean(test.mae) %>% sprintf('%.6f', .), '\n-\n', sep = '')
  cat('Ensemble Test MAE = ', mean(abs(data.set$test.data$keff - rowMeans(test.pred))) %>% sprintf('%.6f', .), '\n', sep = '')

  return(ensemble.model)

}
