# subset.R
#
# William John Zywiec
# The George Washington University
#
# ...

Subset <- function(data.set) {

  # load packages
  library(caret)

  data.set <- na.omit(data.set)
  data.set <<- data.set

  if (file.exists('test-data.csv') && file.exists('training-data.csv') && file.exists('test-df.csv') && file.exists('training-df.csv')) {

    test.data <<- read.csv('test-data.csv', header = TRUE)
    training.data <<- read.csv('training-data.csv', header = TRUE)

    test.df <<- as.matrix(read.csv('test-df.csv', header = TRUE))
    training.df <<- as.matrix(read.csv('training-df.csv', header = TRUE))

  } else if (nrow(data.set) > 1) {

    # test sph only
    data.set$shape <- NULL

    # one-hot encode categorical variables
    dummy <- dummyVars(~ ., data = data.set)
    training.data <- data.frame(predict(dummy, newdata = data.set))

    # partition data
    partition <- createDataPartition(training.data$keff, p = 0.8, list = FALSE)
    test.data <- training.data[-partition, ]
    training.data <- training.data[partition, ]

    test.data <<- test.data
    training.data <<- training.data

    write.csv(test.data, file = 'test-data.csv', row.names = FALSE)
    write.csv(training.data, file = 'training-data.csv', row.names = FALSE)

    # subset data
    test.df <- test.data[-c(25, 26)]
    # test.df <- test.data[-c(27, 28)]
    training.df <- training.data[-c(25, 26)]
    # training.df <- training.data[-c(27, 28)]

    # scale data
    num <- c(1, 9, 20, 21:24)
    # num <- c(1, 9, 20, 23:26)
    training.mean <<- apply(training.df[num], 2, mean)
    training.sd <<- apply(training.df[num], 2, sd)

    i <- 1 # counter

    while (i < length(num)) {
      test.df[num[i]] <- scale(test.df[num[i]], center = training.mean[i], scale = training.sd[i])
      training.df[num[i]] <- scale(training.df[num[i]], center = training.mean[i], scale = training.sd[i])
      i = i + 1
    }

    # convert data frames to matrices (Keras requirement)
    test.df <<- as.matrix(test.df)
    training.df <<- as.matrix(training.df)

    write.csv(test.df, file = 'test-df.csv', row.names = FALSE)
    write.csv(training.df, file = 'training-df.csv', row.names = FALSE)

  }

}
