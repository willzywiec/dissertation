# tabulate.R
#
# William John Zywiec
# The George Washington University
#
# ...

Tabulate <- function() {

	output.files <- list.files(pattern = '\\.o$')

	# load training data
	if (file.exists('training_data.csv')) {

		training.data <- read.csv('training_data.csv', header = TRUE)

		if (nrow(training.data) >= length(output.files)) {
			Subset(na.omit(training.data))
			return(cat('Loaded training_data.csv\n'))
		}

	} 

	if (length(output.files) > 0) {

		mass <- rad <- dim <- ht <- vol <- conc <- hd <- keff <- sd <- numeric()
		form <- mod <- ref <- shape <- character()

		# tabulate training data
		for (i in 1:length(output.files)) {
			if (any(readLines(output.files[i]) %>% grep('final result', .))) {
				output <- readLines(output.files[i]) %>% grep('final result', ., value = TRUE) %>% strsplit('\\s+') %>% unlist()
				file.name <- gsub('\\.o', '', output.files[i]) %>% strsplit('_') %>% unlist()
				mass[i] <- as.numeric(file.name[1])
				form[i] <- file.name[2]
				mod[i] <- file.name[3]
				rad[i] <- as.numeric(file.name[4])
				ref[i] <- file.name[5]
				if (ref[i] == 'none') {
					dim[i] <- 0
					shape[i] <- file.name[6]
				} else {
					dim[i] <- as.numeric(file.name[6])
					shape[i] <- file.name[7]
				}
				if (shape[i] == 'sph') {
					ht[i] <- 2 * rad[i]
				} else if (ref[i] == 'none') {
					ht[i] <- as.numeric(file.name[7])
				} else {
					ht[i] <- as.numeric(file.name[8])
				}
				if (shape[i] == 'sph') {
					vol[i] <- 4/3 * pi * rad[i]^3
				} else if (shape[i] == 'rcc') {
					vol[i] <- pi * rad[i]^2 * ht[i]
				} else if (shape[i] == 'rpp') {
					vol[i] <- rad[i]^2 * ht[i]
				}
				conc[i] <- (mass[i] / (vol[i] * 0.001)) %>% round(2) # concentration (g/L)
				hd[i] <- ht[i] / (2 * rad[i]) # height:diameter
				keff[i] <- as.numeric(output[4]) %>% round(5)
				sd[i] <- as.numeric(output[5]) %>% round(5)
			}
		}

		training.data <- data.frame(
			mass = mass,
			form = form,
			mod = mod,
			rad = rad,
			ref = ref,
			dim = dim,
			shape = shape,
			ht = ht,
			vol = vol,
			conc = conc,
			hd = hd,
			keff = keff,
			sd = sd)

		# write training data to file
		write.csv(training.data, file = 'training_data.csv', row.names = FALSE)
		Subset(na.omit(training.data))
		return(cat('Saved training_data.csv\n'))

	}

}
