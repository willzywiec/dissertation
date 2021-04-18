# scale.R
#
# William John Zywiec
# The George Washington University

Scale <- function(code, output) {

  library(caret)
  library(dplyr)

  if (nrow(output) > 0) {

    output$shape <- output$ht <- output$hd <- NULL

    # one-hot encode categorical variables
    dummy <- dummyVars(~ ., data = output, sep = '')
    training.data <- data.frame(predict(dummy, newdata = output))
    training.data <- subset(training.data, sd < 0.001)

    # partition data
    # test.data <- subset(training.data, mass > 100 & rad > 7.62 & rad < 45.72)
    # test.data <- sample_n(test.data, round(nrow(training.data) * 0.2))
    # training.data <- anti_join(training.data, test.data)
    test.data <- sample_n(training.data, round(nrow(training.data) * 0.2))
    training.data <- anti_join(training.data, test.data)

    # scale data
    index <- c(1, 9, 20:22) # mass, rad, thk, vol, conc

    training.mean <- apply(training.data[index], 2, mean)
    training.sd <- apply(training.data[index], 2, sd)

    training.df <- training.data[-c(23, 24)]
    test.df <- test.data[-c(23, 24)]

    for (i in 1:length(index)) {
      training.df[index[i]] <- scale(training.df[index[i]], center = training.mean[i], scale = training.sd[i])
      test.df[index[i]] <- scale(test.df[index[i]], center = training.mean[i], scale = training.sd[i])
    }

    # convert data frames to matrices (Keras requirement)
    training.df <- as.matrix(training.df)
    test.df <- as.matrix(test.df)

    dataset <- list(output, training.data, training.mean, training.sd, training.df, test.data, test.df)
    names(dataset) <- c('output', 'training.data', 'training.mean', 'training.sd', 'training.df', 'test.data', 'test.df')
    saveRDS(dataset, file = paste0(code, '-dataset.RData'))

    return(dataset)

  }

}
