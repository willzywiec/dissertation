# predict.R
#
# William John Zywiec
# The George Washington University
#
# ...

Predict <- function(bn, ensemble.size, sample.size) {

  # sample conditional probability distributions
  bn.data <- cpdist(bn, nodes = c('mass', 'form', 'mod', 'rad', 'ref', 'dim', 'shape', 'ht'), evidence = TRUE, n = sample.size) %>% na.omit()
  bn.data[[1]] <- unlist(bn.data[[1]]) %>% as.character() %>% as.numeric() # mass
  bn.data[[2]] <- unlist(bn.data[[2]])                                     # form
  bn.data[[3]] <- unlist(bn.data[[3]])                                     # mod
  bn.data[[4]] <- unlist(bn.data[[4]]) %>% as.character() %>% as.numeric() # rad
  bn.data[[5]] <- unlist(bn.data[[5]])                                     # ref
  bn.data[[6]] <- unlist(bn.data[[6]]) %>% as.character() %>% as.numeric() # dim
  bn.data[[7]] <- unlist(bn.data[[7]])                                     # shape
  bn.data[[8]] <- unlist(bn.data[[8]]) %>% as.character() %>% as.numeric() # ht

  vol <- conc <- hd <- numeric()

  # set parameters and calculate vol, conc, and h/d
  for (i in 1:nrow(bn.data)) {

    # set density (g/cc)
    if (bn.data$form[i] == 'alpha') {
      density <- 19.86
    } else if (form == 'delta') {
      density <- 15.9
    } else if (form == 'puo2') {
      density <- 11.5
    }

    # calculate ht (cm) and vol (cc)
    if (bn.data$shape[i] == 'sph') {
      bn.data$ht[i] <- 2 * bn.data$rad[i]
      vol[i] <- (4/3 * pi * bn.data$rad[i]^3)
    } else if (bn.data$shape[i] == 'rcc') {
      vol[i] <- (pi * bn.data$rad[i]^2 * bn.data$ht[i])
    }

    # fix mod, vol (cc), and rad (cm)
    if (vol[i] <= bn.data$mass / density) {
      bn.data$mod[i] <- 'none'
      vol[i] <- bn.data$mass[i] / density
      if (bn.data$shape[i] == 'sph') {
        bn.data$rad[i] <- (3/4 * vol[i] / pi)^(1/3)
      } else if (bn.data$shape[i] == 'rcc') {
        bn.data$rad[i] <- (vol / bn.data$ht[i] / pi)^(1/2)
      }
    }

    # fix ref and dim (cm)
    if (bn.data$ref[i] == 'none' || dim == 0) {
      bn.data$ref[i] <- 'none'
      bn.data$dim[i] <- 0
    }

    conc[i] <- (bn.data$mass[i] / vol[i]) # conc (g/cc)
    hd[i] <- (bn.data$ht[i] / (2 * bn.data$rad[i])) # h/d

  }

  bn.data$vol <- vol
  bn.data$conc <- conc
  bn.data$hd <- hd

  # scale data
  bn.df <- bn.data
  bn.df[[2]] <- bn.df[[2]] %>% as.factor() %>% as.numeric() # form
  bn.df[[3]] <- bn.df[[3]] %>% as.factor() %>% as.numeric() # mod
  bn.df[[5]] <- bn.df[[5]] %>% as.factor() %>% as.numeric() # ref
  bn.df[[7]] <- bn.df[[7]] %>% as.factor() %>% as.numeric() # shape
  bn.df <- scale(bn.df, center = training.mean, scale = training.sd)

  # predict keff
  setwd(paste0(test.dir, '/hdf5'))
  keff <- data.frame(matrix(ncol = ensemble.size, nrow = sample.size))

  for (i in 1:ensemble.size) {
    keff[ , i] <- ensemble.model[[i]] %>% predict(bn.df)
    colnames(keff)[i] <- paste0('model-', i, '-', which.min(ensemble.history[[i]]$metrics$val_mean_absolute_error), '.h5')
  }

  keff.mean <- rowMeans(keff) %>% round(5)
  keff.mean[keff.mean < 0] <- 0
  bn.data$keff <- keff.mean

  return(bn.data)

}
