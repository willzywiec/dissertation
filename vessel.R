# vessel.R
#
# William John Zywiec
# The George Washington University
#
# ...

# initialize environment
if (!is.null(dev.list())) dev.off()
rm(list = ls())
cat('\014')

# load packages
library(parallel)

# set source and training directories
source.directory <- 'C:/Users/Will/Dropbox/Dissertation/R'
training.directory <- 'D:/random_29jan19'

# set variables
ensemble.size <- 1
sample.size <- 1e+06
risk.pool <- 5

# load functions
source(paste0(source.directory, '/nn.R'))
source(paste0(source.directory, '/bn.R'))
source(paste0(source.directory, '/risk.R'))

# build neural network
# NN(ensemble.size, source.directory, training.directory)

# build Bayesian network
BN()

# assess risk
# Risk(ensemble.size, sample.size, risk.pool, source.directory, training.directory)
