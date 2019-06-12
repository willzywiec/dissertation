# generate.R
#
# William John Zywiec
# The George Washington University
#
# ...

Generate <- function(deck.size) {

  # generate random uniform data
  mass <- sample(seq(10, 2000, 10), deck.size, replace = TRUE)
  form <- sample(c('alpha', 'puo2'), deck.size, replace = TRUE)
  mod <- sample(c('al2o3', 'be', 'beo', 'graphite', 'mgo', 'ch2', 'sepiolite', 'sio2', 'h2o', 'none'), deck.size, replace = TRUE)
  rad <- sample(seq(0.5, 18, 0.5) * 2.54, deck.size, replace = TRUE) # 18 in = 45.72 cm
  ref <- sample(c('al', 'al2o3', 'be', 'beo', 'cs', 'cu', 'du', 'granite', 'graphite', 'fe', 'pb', 'mgo', 'mo', 'ni', 'nb', 'pt', 'ch2', 'ss304', 'ss304L', 'ss316', 'ss316L', 'ta', 'ti', 'w', 'v', 'h2o', 'none'), deck.size, replace = TRUE)
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
