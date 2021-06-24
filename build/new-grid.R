# new-grid.R
#
# Will Zywiec

# initialize environment
if (!is.null(dev.list())) dev.off()
rm(list = ls())
cat('\014')

# load function
source('C:/Users/Will/Documents/GitHub/dissertation/build/build.R') # modify this path to point to build.R script

build.dir <- 'D:/mcnp-input' # modify this path to point to folder where input decks will be built

setwd(build.dir)

# variables
#
# mass: grams of fissile material (total weight; includes oxide weight if applicable)
# form: form of material
# mod: moderator
# rad: radius (cm)
# ref: reflector material
# thk: reflector thickness (cm)
# shape: sph or rcc

# set variables
# mass <- c(1000, 2000, 3000, 4000)
mass <- seq(1150, 4000, 25) # seq(25, 4000, 25)
# form <- c('alpha', 'puo2')
form <- c('alpha', 'delta', 'puo2', 'heu', 'uo2')
mod <- c('mgo', 'sepiolite', 'ch2', 'h2o', 'none')
rad <- seq(0, 18, 1) * 2.54
ref <- c('al', 'be', 'du', 'graphite', 'pb', 'mgo', 'ch2', 'ss304', 'h2o', 'none')
thk <- seq(0, 6, 1) * 2.54
shape <- 'sph'

# build input decks
df <- expand.grid(mass, form, mod, rad, ref, thk, shape, stringsAsFactors = FALSE)

bins <- split(df, mass)

for (i in 1:length(bins)) {
  for (j in 1:nrow(bins[[i]])) {
    new.dir <- paste0(build.dir, '/', mass[i])
    dir.create(new.dir, showWarnings = FALSE)
    setwd(new.dir)
    Build(
      bins[[i]][j, 1],
      bins[[i]][j, 2],
      bins[[i]][j, 3],
      bins[[i]][j, 4],
      bins[[i]][j, 5],
      bins[[i]][j, 6],
      bins[[i]][j, 7])
  }
}
