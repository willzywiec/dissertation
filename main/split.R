# split.R
#
# William John Zywiec
# The George Washington University

Split <- function(output) {

  library(caret)

  if (nrow(output) > 0) {

    # comment the next three lines if using spherical and non-spherical geometry
    output$shape <- NULL
    output$ht <- NULL
    output$hd <- NULL

    # one-hot encode categorical variables
    dummy <- dummyVars(~ ., data = output)
    training.data <- data.frame(predict(dummy, newdata = output))

    # partition data
    partition <- createDataPartition(training.data$keff, p = 0.8, list = FALSE)
    test.data <- training.data[-partition, ]
    training.data <- training.data[partition, ]

    # scale data
    index <- c(1, 9, 20:22) # mass, rad, thk, vol, conc
    # index <- c(1, 9, 20, 23:26) # mass, rad, thk, ht, vol, conc, hd

    training.mean <- apply(training.data[index], 2, mean)
    training.sd <- apply(training.data[index], 2, sd)

    training.df <- training.data[-c(23, 24)]
    test.df <- test.data[-c(23, 24)]
    # training.df <- training.data[-c(27, 28)]
    # test.df <- test.data[-c(27, 28)]

    for (i in 1:length(index)) {
      training.df[index[i]] <- scale(training.df[index[i]], center = training.mean[i], scale = training.sd[i])
      test.df[index[i]] <- scale(test.df[index[i]], center = training.mean[i], scale = training.sd[i])
    }

    # convert data frames to matrices (Keras requirement)
    training.df <- as.matrix(training.df)
    test.df <- as.matrix(test.df)

    data.set <- list(output, training.data, training.mean, training.sd, training.df, test.data, test.df)
    names(data.set) <- c("output", "training.data", "training.mean", "training.sd", "training.df", "test.data", "test.df")
    saveRDS(data.set, file = "data-set.RData")

    return(data.set)

  }

}
