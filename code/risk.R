# risk.R
#
# William John Zywiec
# The George Washington University

Risk <- function(bn, data.set, ensemble.model, ensemble.size, sample.size, code.dir, main.dir) {

  source(paste0(code.dir, '/estimate.R'))

  risk.dir <- paste0(main.dir, '/risk-', dist, '-', risk.pool, sub('.', '', sample.size))
  dir.create(risk.dir, showWarnings = FALSE)

  setwd(risk.dir)

  # estimate risk
  if (file.exists('risk.csv')) {

    bn.data <- readRDS('bn-data.RData')
    risk <- read.csv('risk.csv', fileEncoding = 'UTF-8-BOM')
    cat('Risk = ', format(mean(risk$risk), digits = 3, scientific = TRUE), ' (n = ', nrow(risk), ')\n', sep = '')

  } else {

    bn.data <- list()
    risk <- numeric()

    for (i in 1:risk.pool) {
      bn.data[[i]] <- Estimate(bn, data.set, ensemble.model, ensemble.size, sample.size)
      risk[i] <- length(bn.data[[i]]$keff[bn.data[[i]]$keff >= 0.95]) / sample.size # USL = 0.95
      if (i == 1) {
        progress.bar <- txtProgressBar(min = 0, max = risk.pool, style = 3)
        setTxtProgressBar(progress.bar, i)
      } else if (i == risk.pool) {
        setTxtProgressBar(progress.bar, i)
        cat('\n', sep = '')
      } else {
        setTxtProgressBar(progress.bar, i)
      }
    }
  
    saveRDS(bn.data, file = 'bn-data.RData')
    write.csv(as.data.frame(risk, col.names = 'risk'), file = 'risk.csv', row.names = FALSE)
    cat('\nRisk = ', format(mean(risk), digits = 3, scientific = TRUE), ' (n = ', length(risk), ')\n', sep = '')

  }

  return(risk)

}
