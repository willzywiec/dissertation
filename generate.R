# generate.R
#
# William John Zywiec
# The George Washington University
#
# ...

Generate <- function(deck.size) {

  # generate random uniform data
  mass <- sample(seq(10, 2000, 10), deck.size, replace = TRUE)
  form <- sample(c('alpha', 'oxide'), deck.size, replace = TRUE)
  mod <- sample(c('ch2', 'sepiolite', 'h2o', 'none'), deck.size, replace = TRUE)
  rad <- sample(seq(0.5, 18, 0.5) * 2.54, deck.size, replace = TRUE) # 18 in = 45.72 cm
  ref <- sample(c('al', 'be', 'beo', 'cs', 'cu', 'c', 'ch2', 'ss304', 'ta', 'v', 'h2o', 'none'), deck.size, replace = TRUE)
  dim <- sample(seq(0.5, 18, 0.5) * 2.54, deck.size, replace = TRUE) # 18 in = 45.72 cm
  shape <- sample(c('sph', 'rcc', 'rpp'), deck.size, replace = TRUE)
  ht <- sample(seq(1, 36, 1) * 2.54, deck.size, replace = TRUE) # 36 in = 91.44 cm

  # build input decks and run MCNP
  for (i in 1:deck.size) {
    Build(mass[i], form[i], mod[i], rad[i], ref[i], dim[i], shape[i], ht[i])
  }

  # tabulate output files
  Tabulate()

}
