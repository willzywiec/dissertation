# risk.R
#
# William John Zywiec
# The George Washington University
#
# ...

Risk <- function(bn, ensemble.size, sample.size, risk.pool) {

  # load functions
  source(paste0(source.dir, '/predict.R'))

  setwd(test.dir)

  # load data
  if (file.exists('risk.csv')) {
    risk <- read.csv('risk.csv')
  } else {
    risk <- data.frame(risk = numeric())
  }

  # predict keff
  bn.data <- list()

  for (i in 1:risk.pool) {
    bn.data[[i]] <- Predict(bn, ensemble.size, sample.size)
    risk[nrow(risk) + 1, ] <- mean(bn.data[[i]]$keff > 1.0)
    if (file.exists('risk.csv')) {
      write.csv(bn.data[[i]], file = paste0('bn-', nrow(risk) + i, '.csv'), row.names = FALSE)
    } else {
      write.csv(bn.data[[i]], file = paste0('bn-', i, '.csv', row.names = FALSE))
    }
  }

  bn.data <<- bn.data

  if ((file.exists('risk.csv')) || (risk.pool > 0)) { 
    cat(paste0(mean(risk[ , 1]), '\n'))
  }

  setwd(test.dir)
  write.csv(risk, file = 'risk.csv', row.names = FALSE)

}
