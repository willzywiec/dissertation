# test.R
#
# William John Zywiec
# The George Washington University

Test <- function(dataset, metamodel, mae, val.mae, test.dir) {

  library(keras)
  library(magrittr)

  setwd(test.dir)

  test.data <- dataset$test.data
  test.mae <- avg <- nm <- bfgs <- sa <- numeric()
  test.pred <- matrix(nrow = nrow(dataset$test.df), ncol = length(metamodel))
  nm.wt <- bfgs.wt <- sa.wt <- list()

  # build objective function
  Objective <- function(x) mean(abs(test.data$keff - rowSums(test.pred * x, na.rm = TRUE)))

  # minimize objective function
  for (i in 1:length(metamodel)) {

    test.pred[ , i] <- metamodel[[i]] %>% predict(dataset$test.df)
    test.mae[i] <- mean(abs(test.data$keff - test.pred[ , i]))

    avg[i] <- mean(abs(test.data$keff - rowMeans(test.pred, na.rm = TRUE)))

    nm.wt[[i]] <- optim(rep(0, i), Objective, method = 'Nelder-Mead', lower = 0)
    bfgs.wt[[i]] <- optim(rep(0, i), Objective, method = 'BFGS', lower = 0)
    sa.wt[[i]] <- optim(rep(0, i), Objective, method = 'SANN', lower = 0)

    nm[i] <- mean(abs(test.data$keff - rowSums(test.pred * nm.wt[[i]][[1]], na.rm = TRUE)))
    bfgs[i] <- mean(abs(test.data$keff - rowSums(test.pred * bfgs.wt[[i]][[1]], na.rm = TRUE)))
    sa[i] <- mean(abs(test.data$keff - rowSums(test.pred * sa.wt[[i]][[1]], na.rm = TRUE)))

    if (i == 1) {
      progress.bar <- txtProgressBar(min = 0, max = length(metamodel), style = 3)
      setTxtProgressBar(progress.bar, i)
      if (i == length(metamodel)) {
        cat('\n\n', sep = '')
      }
    } else if (i == length(metamodel)) {
      setTxtProgressBar(progress.bar, i)
      cat('\n\n', sep = '')
    } else {
      setTxtProgressBar(progress.bar, i)
    }

  }
  
  test.results <- data.frame(avg = avg, nm = nm, bfgs = bfgs, sa = sa)
  write.csv(test.results, file = 'test-results.csv', row.names = FALSE)

  cat('Mean Training MAE = ', mean(mae) %>% sprintf('%.6f', .), '\n', sep = '')
  cat('Mean Cross-Validation MAE = ', mean(val.mae) %>% sprintf('%.6f', .), '\n', sep = '')
  cat('Mean Test MAE = ', mean(test.mae) %>% sprintf('%.6f', .), '\n\n', sep = '')
  cat('Ensemble Test MAE = ', avg[length(metamodel)] %>% sprintf('%.9f', .), '\n', sep = '')
  cat('Ensemble Test MAE = ', nm[length(metamodel)] %>% sprintf('%.9f', .), ' (Nelder-Mead)\n', sep = '')
  cat('Ensemble Test MAE = ', bfgs[length(metamodel)] %>% sprintf('%.9f', .), ' (BFGS)\n', sep = '')
  cat('Ensemble Test MAE = ', sa[length(metamodel)] %>% sprintf('%.9f', .), ' (SA)\n', sep = '')

  test.min <- min(c(avg[which.min(avg)], nm[which.min(nm)], bfgs[which.min(bfgs)], sa[which.min(sa)]))

  if (test.min == nm[which.min(nm)]) {
    wt <- nm.wt[[which.min(nm)]]
  } else if (test.min == bfgs[which.min(bfgs)]) {
    wt <- bfgs.wt[[which.min(bfgs)]]
  } else if (test.min == sa[which.min(sa)]) {
    wt <- sa.wt[[which.min(sa)]]
  } else {
    wt <- 0
  }

  if (length(wt[[1]]) < length(metamodel) && wt[[1]][1] != 0) {
    cat('-\nTest MAE reaches a local minimum with ', length(wt[[1]]), ' neural networks\n\n', sep = '')
    cat('Ensemble Test MAE = ', avg[length(wt[[1]])] %>% sprintf('%.9f', .), '\n', sep = '')
    cat('Ensemble Test MAE = ', nm[length(wt[[1]])] %>% sprintf('%.9f', .), ' (Nelder-Mead)\n', sep = '')
    cat('Ensemble Test MAE = ', bfgs[length(wt[[1]])] %>% sprintf('%.9f', .), ' (BFGS)\n', sep = '')
    cat('Ensemble Test MAE = ', sa[length(wt[[1]])] %>% sprintf('%.9f', .), ' (SA)\n', sep = '')
  }

  training.data <- dataset$training.data

  # adjust predicted keff values
  Adjust <- function(x, dataset, training.data, test.data, test.pred, nm.wt, bfgs.wt, sa.wt) {

    training.pred <- matrix(nrow = nrow(dataset$training.df), ncol = x)

    if (x == 1) {

      training.pred[ , 1] <- metamodel[[1]] %>% predict(dataset$training.df)

      training.data$avg <- training.pred[ , 1]
      training.data$nm <- training.pred[ , 1] * nm.wt[[x]][[1]]
      training.data$bfgs <- training.pred[ , 1] * bfgs.wt[[x]][[1]]
      training.data$sa <- training.pred[ , 1] * sa.wt[[x]][[1]]

      test.data$avg <- test.pred[ , 1]
      test.data$nm <- test.pred[ , 1] * nm.wt[[x]][[1]]
      test.data$bfgs <- test.pred[ , 1] * bfgs.wt[[x]][[1]]
      test.data$sa <- test.pred[ , 1] * sa.wt[[x]][[1]]

    } else {

      for (i in 1:x) {
        training.pred[ , i] <- metamodel[[i]] %>% predict(dataset$training.df)
      }

      training.data$avg <- rowMeans(training.pred[ , 1:x])
      training.data$nm <- rowSums(training.pred[ , 1:x] * nm.wt[[x]][[1]])
      training.data$bfgs <- rowSums(training.pred[ , 1:x] * bfgs.wt[[x]][[1]])
      training.data$sa <- rowSums(training.pred[ , 1:x] * sa.wt[[x]][[1]])

      test.data$avg <- rowMeans(test.pred[ , 1:x])
      test.data$nm <- rowSums(test.pred[ , 1:x] * nm.wt[[x]][[1]])
      test.data$bfgs <- rowSums(test.pred[ , 1:x] * bfgs.wt[[x]][[1]])
      test.data$sa <- rowSums(test.pred[ , 1:x] * sa.wt[[x]][[1]])

    }

    write.csv(training.data, file = 'training-data.csv', row.names = FALSE)
    write.csv(test.data, file = 'test-data.csv', row.names = FALSE)

  }

  Adjust(length(wt[[1]]), dataset, training.data, test.data, test.pred, nm.wt, bfgs.wt, sa.wt)

  return(wt)

}
