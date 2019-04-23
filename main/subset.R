# subset.R
#
# William John Zywiec
# The George Washington University
#
# ...

Subset <- function(data.set) {

  # load packages
  library(caret)

  data.set <<- data.set

  if (nrow(data.set) > 1) {

    # one-hot encode categorical variables
    dummy <- dummyVars(~ ., data = data.set) # extra space between '~ .,'
    training.data <- data.frame(predict(dummy, newdata = data.set))

    # partition data
    training.partition <- createDataPartition(training.data$keff, p = 0.8, list = FALSE)
    test.data <- training.data[-training.partition, ]
    training.data <- training.data[training.partition, ]

    test.data <<- test.data
    training.data <<- training.data

    # subset data
    test.df <- test.data[c(-29, -30)]
    training.df <- training.data[c(-29, -30)]

    # scale data
    num <- c(1, 8, 21, 25:28)
    training.mean <<- apply(training.df[num], 2, mean)
    training.sd <<- apply(training.df[num], 2, sd)

    i <- 1 # counter

    while (i < length(num)) {
      training.df[num[i]] <- scale(training.df[num[i]], center = training.mean[i], scale = training.sd[i])
      test.df[num[i]] <- scale(test.df[num[i]], center = training.mean[i], scale = training.sd[i])
      i = i + 1
    }

    # convert data frames to matrices (Keras requirement)
    test.df <<- as.matrix(test.df)
    training.df <<- as.matrix(training.df)

  } else {

    training.data <<- training.data

  }

}
