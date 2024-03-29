# nn.R
#
# William John Zywiec
# The George Washington University

NN <- function(dataset, batch.size, ensemble.size, epochs, layers, loss, opt.alg, learning.rate, replot, val.split, source.dir, test.dir) {

  library(keras)
  library(magrittr)

  # load functions
  source(paste0(source.dir, '/model.R'))
  source(paste0(source.dir, '/fit.R'))
  source(paste0(source.dir, '/plot.R'))
  source(paste0(source.dir, '/test.R'))

  # build loss function
  if (loss == 'sse') loss <- SSE <- function(y_true, y_pred) k_sum(k_pow(y_true - y_pred, 2))

  model.dir <- paste0(test.dir, '/model')
  dir.create(model.dir, recursive = TRUE, showWarnings = FALSE)

  setwd(model.dir)

  model.files <- list.files(pattern = '\\.h5$')

  # build and train metamodel
  metamodel <- history <- rep(list(0), length(ensemble.size))

  if (length(model.files) < ensemble.size) {
    for (i in (length(model.files) + 1):ensemble.size) {
      metamodel[[i]] <- Model(dataset, layers, loss, opt.alg, learning.rate)
      history[[i]] <- Fit(dataset, metamodel[[i]], batch.size, epochs, val.split)
      Plot(i, history[[i]])
      save_model_hdf5(metamodel[[i]], paste0(i, '.h5'))
    }
  } else if (replot == TRUE) {
    for (i in 1:ensemble.size) Plot(i)
  }

  model.files <- list.files(pattern = '\\.h5$')

  for (i in 1:ensemble.size) metamodel[[i]] <- load_model_hdf5(model.files[i], custom_objects = c(loss = loss))

  # rebuild metamodel
  remodel.dir <- paste0(test.dir, '/remodel')
  dir.create(remodel.dir, showWarnings = FALSE)

  setwd(remodel.dir)

  remodel.files <- list.files(pattern = '\\.h5$')
  history <- list()

  if (length(remodel.files) < ensemble.size * epochs / 10) {
    for (i in 1:ensemble.size) {
      remodel.files <- list.files(pattern = paste0(i, '-.+\\.h5$'))
      if (length(remodel.files) < epochs / 10) {
        history[[i]] <- Fit(dataset, metamodel[[i]], batch.size, epochs / 10, val.split, remodel.dir, i)
        Plot(i, history[[i]])
      } else {
        Plot(i)
      }
    }
  } else if (replot == TRUE) {
    for (i in 1:ensemble.size) Plot(i)
  }

  # test metamodel
  mae <- val.mae <- numeric()

  for (i in 1:ensemble.size) {
    metrics <- read.csv(paste0(i, '.csv'))
    mae[i] <- metrics$mae[which.min(metrics$mae + metrics$val.mae)]
    val.mae[i] <- metrics$val.mae[which.min(metrics$mae + metrics$val.mae)]
    metamodel[[i]] <- load_model_hdf5(paste0(i, '-', metrics$epoch[which.min(metrics$mae + metrics$val.mae)], '.h5'), custom_objects = c(loss = loss))
  }

  wt <- Test(dataset, metamodel, mae, val.mae, test.dir)

  return(list(metamodel, wt))

}
