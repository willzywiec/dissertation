# risk.R
#
# William John Zywiec
# The George Washington University
#
# ...

Risk <- function(ensemble.size, sample.size, risk.pool, source.directory, training.directory) {

	# load functions
	source(paste0(source.directory, '/infer.R'))

	# load risk
	setwd(training.directory)
	if (file.exists('risk.csv')) {
		risk <- read.csv('risk.csv')
	} else {
		risk <- data.frame(risk = numeric())
	}

	# assess risk
	if (risk.pool > 0) {

		bn.data <- list()

		for (i in 1:risk.pool) {
			bn.data[[i]] <- Infer(ensemble.size, sample.size, training.directory)
			risk[nrow(risk) + 1, ] <- mean(bn.data[[i]]$keff > 1.0)
			if (file.exists('risk.csv')) {
				write.csv(bn.data[[i]], file = paste0('bn_', nrow(risk) + i, '.csv'), row.names = FALSE)
			} else {
				write.csv(bn.data[[i]], file = paste0('bn_', i, '.csv', row.names = FALSE))
			}
		}

		setwd(training.directory)
		write.csv(risk, file = 'risk.csv', row.names = FALSE)
		cat('Saved risk.csv\n')

		bn.data <<- bn.data

	}

	# write risk to file
	if ((file.exists('risk.csv')) || (risk.pool > 0)) { 
		cat(paste0(mean(risk[ , 1]), '\n'))
	}

}
