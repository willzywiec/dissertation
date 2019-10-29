# generate.R
#
# William John Zywiec
# The George Washington University
#
# ...

Generate <- function() {

  # set deck size
  deck.size <- 1e+04
  
  if (file.exists('data-set.csv')) {
    if (nrow(data.set) < deck.size) {
      deck.size <- deck.size - nrow(data.set)
    }
  }

  # load functions
  source(paste0(source.dir, '/build.R'))
  source(paste0(source.dir, '/tabulate.R'))

  # tabulate data
  setwd(test.dir)
  Tabulate()

  # generate random uniform data
  mass <- sample(seq(10, 2000, 10), deck.size, replace = TRUE)
  form <- sample(c('alpha', 'puo2'), deck.size, replace = TRUE)
  mod <- sample(c('mgo', 'ch2', 'sepiolite', 'h2o', 'none'), deck.size, replace = TRUE)
  rad <- sample(seq(0.5, 18, 0.5) * 2.54, deck.size, replace = TRUE) # 18 in = 45.72 cm
  ref <- sample(c('al', 'be', 'du', 'graphite', 'pb', 'mgo', 'ch2', 'ss304', 'h2o', 'none'), deck.size, replace = TRUE)
  dim <- sample(seq(0.5, 18, 0.5) * 2.54, deck.size, replace = TRUE) # 18 in = 45.72 cm
  shape <- sample(c('sph', 'rcc'), deck.size, replace = TRUE)
  ht <- sample(seq(0.5, 36, 0.5) * 2.54, deck.size, replace = TRUE) # 36 in = 91.44 cm

  # build input decks and run MCNP
  for (i in 1:deck.size) {
    Build(mass[i], form[i], mod[i], rad[i], ref[i], dim[i], shape[i], ht[i])
  }

  # tabulate data
  Tabulate()

}
