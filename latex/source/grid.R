# grid.R
#
# William John Zywiec
# The George Washington University

# initialize environment
if (!is.null(dev.list())) dev.off()
rm(list = ls())
cat('\014')

# load function
source('E:/Dropbox/GWU/R/source/build.R')

build.dir <- 'F:/300/mcnp'

setwd(build.dir)

# set variables
mass <- seq(25, 4000, 25)
form <- c('alpha', 'puo2')
mod <- c('mgo', 'sepiolite', 'ch2', 'h2o', 'none')
rad <- seq(0, 18, 1) * 2.54
ref <- c('al', 'be', 'du', 'graphite', 'pb', 'mgo', 'ch2', 'ss304', 'h2o', 'none')
thk <- seq(0, 6, 1) * 2.54
shape <- 'sph'

# build input decks and run MCNP
df <- expand.grid(mass, form, mod, rad, ref, thk, shape, stringsAsFactors = FALSE)

for (i in 1:nrow(df)) {
  Build(df[i, 1], df[i, 2], df[i, 3], df[i, 4], df[i, 5], df[i, 6], df[i, 7])
}
