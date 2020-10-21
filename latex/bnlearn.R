library(bnlearn)
library(caret)
library(parallel)

# set variables
sample.size <- 20
cluster <- makeCluster((detectCores() / 4), type = 'SOCK')

# sample conditional probability tables
bn.data <- cpdist(
  bn,
  nodes = c('mass', 'form', 'mod', 'rad', 'ref', 'thk'),
  evidence = TRUE,
  batch = sample.size,
  cluster = cluster,
  n = sample.size) %>% na.omit()

stopCluster(cluster)

print(bn.data)
