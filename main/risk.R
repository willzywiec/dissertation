# risk.R
#
# William John Zywiec
# The George Washington University
#
# ...

Risk <- function(bn, data.set, ensemble.model, ensemble.size, sample.size) {

  # load packages
  library(parallel)

  cl <- makeCluster(10, type = 'SOCK')

  # sample conditional probability distributions
  bn.data <- cpdist(bn, nodes = c('mass', 'form', 'mod', 'rad', 'ref', 'dim', 'shape', 'ht'), evidence = TRUE, cluster = cl, n = sample.size) %>% na.omit()

  stopCluster(cl)

  bn.data[[1]] <- unlist(bn.data[[1]]) %>% as.character() %>% as.numeric() # mass
  bn.data[[2]] <- unlist(bn.data[[2]])                                     # form
  bn.data[[3]] <- unlist(bn.data[[3]])                                     # mod
  bn.data[[4]] <- unlist(bn.data[[4]]) %>% as.character() %>% as.numeric() # rad
  bn.data[[5]] <- unlist(bn.data[[5]])                                     # ref
  bn.data[[6]] <- unlist(bn.data[[6]]) %>% as.character() %>% as.numeric() # dim
  bn.data[[7]] <- unlist(bn.data[[7]])                                     # shape
  bn.data[[8]] <- unlist(bn.data[[8]]) %>% as.character() %>% as.numeric() # ht

  vol <- conc <- hd <- numeric()

  # set parameters
  for (i in 1:nrow(bn.data)) {

    # set density (g/cc)
    if (bn.data$form[i] == 'alpha') {
      density <- 19.86
    } else if (bn.data$form[i] == 'puo2') {
      density <- 11.5
    }

    # fix ht (cm) and calculate vol (cc)
    if (bn.data$shape[i] == 'sph') {
      bn.data$ht[i] <- 2 * bn.data$rad[i]
      vol[i] <- 4/3 * pi * bn.data$rad[i]^3
    } else if (bn.data$shape[i] == 'rcc') {
      vol[i] <- pi * bn.data$rad[i]^2 * bn.data$ht[i]
    }

    # fix mod, vol (cc), and rad (cm)
    if (vol[i] <= bn.data$mass[i] / density) {
      bn.data$mod[i] <- 'none'
      vol[i] <- bn.data$mass[i] / density
      if (bn.data$shape[i] == 'sph') {
        bn.data$rad[i] <- (3/4 * vol[i] / pi)^(1/3)
      } else if (bn.data$shape[i] == 'rcc') {
        bn.data$rad[i] <- (vol / bn.data$ht[i] / pi)^(1/2)
      }
    }

    # fix ref and dim (cm)
    if (bn.data$ref[i] == 'none' || bn.data$dim[i] == 0) {
      bn.data$ref[i] <- 'none'
      bn.data$dim[i] <- 0
    }

    # calculate conc (g/cc) and h/d
    if (vol[i] == 0) {
      conc[i] <- 0
      hd[i] <- 0
    } else {
      conc[i] <- bn.data$mass[i] / vol[i]
      hd[i] <- bn.data$ht[i] / (2 * bn.data$rad[i])
    }

  }

  bn.data$vol <- vol
  bn.data$conc <- conc
  bn.data$hd <- hd

  # load packages
  library(caret)

  # one-hot encode categorical variables
  dummy <- dummyVars(~ ., data = data.set$output[-c(12, 13)])
  # dummy <- dummyVars(~ ., data = data.set$output)
  bn.df <- data.frame(predict(dummy, newdata = bn.data))

  # scale data
  num <- c(1, 9, 20, 23:26)

  for (i in 1:length(num)) {
    bn.df[num[i]] <- scale(bn.df[num[i]], center = data.set$training.mean[i], scale = data.set$training.sd[i])
  }

  bn.df <- as.matrix(bn.df) # convert data frame to matrix (Keras requirement)

  # load packages
  library(keras)

  # predict keff
  keff <- ensemble.model[[1]] %>% predict(bn.df)
  keff[keff < 0] <- 0

  bn.data$keff <- keff

  bn.df <- cbind(bn.df, keff)
  bn.df <- subset(bn.df, keff >= 0.95)
  bn.df <- bn.df[ , -ncol(bn.df)]

  keff <- matrix(nrow = sample.size, ncol = ensemble.size)

  if (nrow(bn.df) > 0) {
    for (i in 1:ensemble.size) {
      keff[ , i] <- ensemble.model[[i]] %>% predict(bn.df)
    }
    keff <- rowMeans(keff)
    keff <- keff[keff >= 0.95]
    cat('Risk = ', nrow(keff) / sample.size, '\n', sep = '')
  } else {
    cat('Risk = 0\n')
  }

  return(bn.data)

}
