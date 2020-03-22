# generate.R
#
# William John Zywiec
# The George Washington University

# load functions
source("E:/Dropbox/GWU/R/misc/build.R")

# generate random uniform data
mass <- seq(50, 4000, 50)
form <- c("alpha", "puo2")
mod <- c("mgo", "sepiolite", "none")
rad <- seq(0, 18, 1) * 2.54
ref <- c("al", "be", "du", "graphite", "pb", "mgo", "ch2", "ss304", "h2o", "none")
thk <- seq(0, 6, 1) * 2.54
shape <- "sph"
# ht <- sample(seq(0.5, 36, 0.5) * 2.54, deck.size, replace = TRUE) # 36 in = 91.44 cm

file.dir <- "C:/Users/Will/Desktop/19mar20"

setwd(file.dir)
  
# build input decks and run MCNP
for (i in 1:40) {
  p <- i + 40
  dir.create(paste0(file.dir, "/", i))
  setwd(paste0(file.dir, "/", i))
  for (j in 1:length(form)) {
    for (k in 1:length(mod)) {
      for (l in 1:length(rad)) {
        for (m in 1:length(ref)) {
          for (n in 1:length(thk)) {
            for (o in 1:length(shape)) {
              Build(mass[i], form[j], mod[k], rad[l], ref[m], thk[n], shape[o])
              Build(mass[p], form[j], mod[k], rad[l], ref[m], thk[n], shape[o])
            }
          }
        }
      }
    }
  }
}
