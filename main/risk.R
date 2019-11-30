# risk.R
#
# William John Zywiec
# The George Washington University
#
# ...

Risk <- function(bn, data.set, ensemble.model, ensemble.size, sample.size) {

  # load packages
  library(bnlearn)
  library(parallel)

  cluster <- makeCluster((detectCores() / 2), type = 'SOCK')

  # sample conditional probability distributions
  bn.data <- cpdist(
    bn,
    nodes = c('mass', 'form', 'mod', 'rad', 'ref', 'dim', 'shape', 'ht'),
    # evidence = TRUE,
    evidence = (as.integer(mass) > 100),
    batch = (sample.size / 10),
    cluster = cluster,
    n = sample.size) %>% na.omit()

  stopCluster(cluster)

  bn.data[[1]] <- unlist(bn.data[[1]]) %>% as.character() %>% as.numeric() # mass
  bn.data[[2]] <- unlist(bn.data[[2]])                                     # form
  bn.data[[3]] <- unlist(bn.data[[3]])                                     # mod
  bn.data[[4]] <- unlist(bn.data[[4]]) %>% as.character() %>% as.numeric() # rad
  bn.data[[5]] <- unlist(bn.data[[5]])                                     # ref
  bn.data[[6]] <- unlist(bn.data[[6]]) %>% as.character() %>% as.numeric() # dim
  bn.data[[7]] <- unlist(bn.data[[7]])                                     # shape
  bn.data[[8]] <- unlist(bn.data[[8]]) %>% as.character() %>% as.numeric() # ht

  vol <- conc <- hd <- numeric()

  # set density (g/cc)
  bn.data$form <- ifelse((bn.data$form == 'alpha'), 19.86, 11.5)

  # fix ht (cm) and calculate vol (cc)
  bn.data$ht <- ifelse((bn.data$shape == 'sph'), (2 * bn.data$rad), bn.data$ht)
  vol <- ifelse((bn.data$shape == 'sph'), (4/3 * pi * bn.data$rad^3), (pi * bn.data$rad^2 * bn.data$ht))

  # fix mod, vol (cc), and rad (cm)
  bn.data$mod <- ifelse((vol <= bn.data$mass / bn.data$form), 'none', as.character(bn.data$mod))
  vol <- ifelse((vol <= bn.data$mass / bn.data$form), (bn.data$mass / bn.data$form), vol)
  bn.data$rad <- ifelse((bn.data$shape == 'sph'), ((3/4 * vol / pi)^(1/3)), ((vol / bn.data$ht / pi)^(1/2)))

  # fix ref and dim (cm)
  bn.data$ref <- ifelse((bn.data$dim == 0), 'none', as.character(bn.data$ref))
  bn.data$dim <- ifelse((bn.data$ref == 'none'), 0, bn.data$dim)

  # calculate conc (g/cc) and h/d
  conc <- ifelse((vol == 0), 0, (bn.data$mass / vol))
  hd <- ifelse((vol == 0), 0, (bn.data$ht / (2 * bn.data$rad)))

  # reset form
  bn.data$form <- ifelse((bn.data$form == 19.86), 'alpha', 'puo2')

  bn.data$vol <- vol
  bn.data$conc <- conc
  bn.data$hd <- hd

  # load packages
  library(caret)

  # one-hot encode categorical variables
  dummy <- dummyVars(~ ., data = data.set$output[-c(12, 13)])
  bn.df <- data.frame(predict(dummy, newdata = bn.data))

  # scale data
  num <- c(1, 9, 20, 23:26)

  for (i in 1:length(num)) {
    bn.df[num[i]] <- scale(bn.df[num[i]], center = data.set$training.mean[i], scale = data.set$training.sd[i])
  }

  # convert data frame to matrix (Keras requirement)
  bn.df <- as.matrix(bn.df)

  # load packages
  library(keras)

  # predict keff
  keff <- ensemble.model[[1]] %>% predict(bn.df)

  bn.data$keff <- keff
  bn.data <- subset(bn.data, keff >= 0.5)

  bn.df <- cbind(bn.df, keff) %>% subset(keff >= 0.5)
  bn.df <- bn.df[ , -ncol(bn.df)]

  keff <- matrix(nrow = nrow(bn.df), ncol = ensemble.size)

  for (i in 1:ensemble.size) {
    keff[ , i] <- ensemble.model[[i]] %>% predict(bn.df)
  }

  bn.data$keff <- keff <- rowMeans(keff)

  cat('Risk = ', (length(keff[keff >= 0.9558]) / sample.size), '\n', sep = '')

  return(bn.data)

}
