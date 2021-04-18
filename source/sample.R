# sample.R
#
# William John Zywiec
# The George Washington University

Sample <- function(bn, dataset, metamodel, sample.size) {

  library(bnlearn)
  library(magrittr)
  library(parallel)

  cluster <- makeCluster((detectCores() / 2), type = 'SOCK')

  # sample conditional probability tables
  bn.data <- cpdist(
    bn,
    nodes = c('mass', 'form', 'mod', 'rad', 'ref', 'thk'),
    evidence = (as.integer(mass) > 120) & (as.integer(rad) > 7) & mod == 'ch2',
    # evidence = TRUE,
    batch = sample.size,
    cluster = cluster,
    n = sample.size) %>% na.omit()

  stopCluster(cluster)

  bn.data[[1]] <- unlist(bn.data[[1]]) %>% as.character() %>% as.numeric() # mass
  bn.data[[2]] <- unlist(bn.data[[2]])                                     # form
  bn.data[[3]] <- unlist(bn.data[[3]])                                     # mod
  bn.data[[4]] <- unlist(bn.data[[4]]) %>% as.character() %>% as.numeric() # rad
  bn.data[[5]] <- unlist(bn.data[[5]])                                     # ref
  bn.data[[6]] <- unlist(bn.data[[6]]) %>% as.character() %>% as.numeric() # thk

  # set Pu density (g/cc)
  pu.density <- ifelse((bn.data$form == 'alpha'), 19.86, 11.5)

  # calculate vol (cc)
  vol <- 4/3 * pi * bn.data$rad^3

  # fix mod, vol (cc), and rad (cm)
  bn.data$mod[vol <= bn.data$mass / pu.density] <- 'none'
  vol[vol <= bn.data$mass / pu.density] <- bn.data$mass[vol <= bn.data$mass / pu.density] / pu.density[vol <= bn.data$mass / pu.density]
  bn.data$rad <- (3/4 * vol / pi)^(1/3)

  # fix ref and thk (cm)
  bn.data$ref[bn.data$thk == 0] <- 'none'
  bn.data$thk[bn.data$ref == 'none'] <- 0

  # set conc (g/cc)
  conc <- ifelse((vol == 0), 0, (bn.data$mass / vol))

  # set form, vol (cc), and conc (g/cc)
  bn.data$form <- ifelse((pu.density == 19.86), 'alpha', 'puo2')
  bn.data$vol <- vol
  bn.data$conc <- conc

  library(caret)

  # one-hot encode categorical variables
  dummy <- dummyVars(~ ., data = bn.data, sep = '')
  bn.df <- data.frame(predict(dummy, newdata = bn.data))

  bn.df <- bn.df[head(names(dataset$training.data), -2)]

  # scale data
  bn.df$mass <- scale(bn.df$mass, center = dataset$training.mean[1], scale = dataset$training.sd[1])
  bn.df$rad <- scale(bn.df$rad, center = dataset$training.mean[2], scale = dataset$training.sd[2])
  bn.df$thk <- scale(bn.df$thk, center = dataset$training.mean[3], scale = dataset$training.sd[3])
  bn.df$vol <- scale(bn.df$vol, center = dataset$training.mean[4], scale = dataset$training.sd[4])
  bn.df$conc <- scale(bn.df$conc, center = dataset$training.mean[5], scale = dataset$training.sd[5])

  # convert data frame to matrix (Keras requirement)
  bn.df <- as.matrix(bn.df)

  library(keras)

  # predict keff values
  bn.data$keff <- metamodel[[1]][[1]] %>% predict(bn.df)

  bn.df <- cbind(bn.df, bn.data$keff) %>% subset(bn.data$keff > 0.9)
  bn.df <- bn.df[ , -ncol(bn.df)]

  bn.data <- subset(bn.data, keff > 0.9)

  if (typeof(metamodel[[2]]) == 'list') {
    keff <- matrix(nrow = nrow(bn.df), ncol = length(metamodel[[2]][[1]]))
    for (i in 1:length(metamodel[[2]][[1]])) keff[ , i] <- metamodel[[1]][[i]] %>% predict(bn.df)
    bn.data$keff <- rowSums(keff * metamodel[[2]][[1]])
  } else {
    keff <- matrix(nrow = nrow(bn.df), ncol = length(metamodel[[1]]))
    for (i in 1:length(metamodel[[1]])) keff[ , i] <- metamodel[[1]][[i]] %>% predict(bn.df)
    bn.data$keff <- rowMeans(keff)
  }

  return(bn.data)

}
