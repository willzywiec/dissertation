# compare.R
#
# William John Zywiec
# The George Washington University

# load package
library(magrittr)

setwd('F:/main')

output <- read.csv('output.csv', fileEncoding = 'UTF-8-BOM') %>% na.omit()

setwd('F:/main/mcnp')



setwd('F:/main/mcnp-0')