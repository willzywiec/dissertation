# risk.R
#
# William John Zywiec
# The George Washington University
#
# ...

Risk <- function(bn, ensemble.size, sample.size, risk.pool, source.dir, training.dir) {

  # load functions
  source(paste0(source.dir, '/infer.R'))

  setwd(training.dir)

  if (file.exists('risk.csv')) {
    risk <- read.csv('risk.csv')
  } else {
    risk <- data.frame(risk = numeric())
  }

  if (risk.pool > 0) {

    bn.data <- list()

    for (i in 1:risk.pool) {
      bn.data[[i]] <- Infer(bn, ensemble.size, sample.size, training.dir)
      risk[nrow(risk) + 1, ] <- mean(bn.data[[i]]$keff > 1.0)
      if (file.exists('risk.csv')) {
        write.csv(bn.data[[i]], file = paste0('bn-', nrow(risk) + i, '.csv'), row.names = FALSE)
      } else {
        write.csv(bn.data[[i]], file = paste0('bn-', i, '.csv', row.names = FALSE))
      }
    }

    setwd(training.dir)
    write.csv(risk, file = 'risk.csv', row.names = FALSE)
    cat('Saved risk.csv\n')

    bn.data <<- bn.data

  }

  # write risk to file
  if ((file.exists('risk.csv')) || (risk.pool > 0)) { 
    cat(paste0(mean(risk[ , 1]), '\n'))
  }

}
