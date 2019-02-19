# infer.R
#
# William John Zywiec
# The George Washington University
#
# ...

Infer <- function(ensemble.size, sample.size, training.directory) {

	# generate sample data
	bn.data <- cpdist(bn, nodes = c('mass', 'mod', 'rad', 'ref', 'dim'), evidence = TRUE, n = sample.size) %>% na.omit()
	bn.data[[1]] <- unlist(bn.data[[1]]) %>% as.character() %>% as.numeric() # mass
	bn.data[[2]] <- unlist(bn.data[[2]])									 # mod
	bn.data[[3]] <- unlist(bn.data[[3]]) %>% as.character() %>% as.numeric() # radius
	bn.data[[4]] <- unlist(bn.data[[4]])									 # ref
	bn.data[[5]] <- unlist(bn.data[[5]]) %>% as.character() %>% as.numeric() # dim

	# tabulate sample data
	conc <- numeric()

	for (i in 1:nrow(bn.data)) {
		if (bn.data[i, 1] == 0) {
			conc[i] <- 0
		} else {
			conc[i] <- (bn.data[i, 1] / (4/3 * pi * bn.data[i, 3]^3 * 0.001)) %>% round(2)
		}
	}

	bn.data$conc <- conc

	# scale sample data
	bn.df <- bn.data
	bn.df[[2]] <- bn.df[[2]] %>% as.factor() %>% as.numeric() # mod
	bn.df[[4]] <- bn.df[[4]] %>% as.factor() %>% as.numeric() # ref
	bn.df <- scale(bn.df, center = training.mean, scale = training.sd)

	# infer keff
	setwd(paste0(training.directory, '/hdf5'))
	keff <- data.frame(matrix(ncol = ensemble.size, nrow = sample.size))

	for (i in 1:ensemble.size) {
		keff[ , i] <- ensemble.model[[i]] %>% predict(bn.df)
		colnames(keff)[i] <- paste0('model_', i, '_', which.min(ensemble.history[[i]]$metrics$mean_absolute_error), '.h5')
	}

	keff.mean <- rowMeans(keff) %>% round(5)
	keff.mean[keff.mean < 0] <- 0
	bn.data$keff <- keff.mean
	return(bn.data)

}
