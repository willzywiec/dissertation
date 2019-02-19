# subset.R
#
# William John Zywiec
# The George Washington University
#
# ...

Subset <- function(training.data) {

	# load packages
	library(caret)

	if (nrow(training.data) > 1) {

		# partition data
		training.partition <- createDataPartition(training.data$keff, p = 0.8, list = FALSE)
		test.data <- training.data[-training.partition, ]
		training.data <- training.data[training.partition, ]

		test.data <<- test.data
		training.data <<- training.data

		# subset training data
		training.df <- training.data[c(-12, -13)]
		training.df$form <- training.df$form %>% as.factor() %>% as.numeric()
		training.df$mod <- training.df$mod %>% as.factor() %>% as.numeric()
		training.df$ref <- training.df$ref %>% as.factor() %>% as.numeric()
		training.df$shape <- training.df$shape %>% as.factor() %>% as.numeric()

		# subset test data
		test.df <- test.data[c(-12, -13)]
		test.df$form <- test.df$form %>% as.factor() %>% as.numeric()
		test.df$mod <- test.df$mod %>% as.factor() %>% as.numeric()
		test.df$ref <- test.df$ref %>% as.factor() %>% as.numeric()
		test.df$shape <- test.df$shape %>% as.factor() %>% as.numeric()

		# scale training and test data
		training.mean <<- apply(training.df, 2, mean)
		training.sd <<- apply(training.df, 2, sd)
		training.df <<- scale(training.df, center = training.mean, scale = training.sd)
		test.df <<- scale(test.df, center = training.mean, scale = training.sd)

	} else {

		training.data <<- training.data

	}

}
