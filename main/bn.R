# bn.R
#
# William John Zywiec
# The George Washington University
#
# ...

BN <- function() {

  # load packages
  library(bnlearn)
  library(graph)
  library(igraph)

  # set arcs
  dag <- empty.graph(nodes = c('operation', 'condition', 'mass', 'form', 'mod', 'rad', 'ref', 'dim', 'shape', 'ht'))

  dag <- set.arc(dag, from = 'operation', to = 'condition')
  dag <- set.arc(dag, from = 'operation', to = 'mass')
  dag <- set.arc(dag, from = 'operation', to = 'form')
  dag <- set.arc(dag, from = 'operation', to = 'mod')
  dag <- set.arc(dag, from = 'operation', to = 'rad')
  dag <- set.arc(dag, from = 'operation', to = 'ref')
  dag <- set.arc(dag, from = 'operation', to = 'dim')
  dag <- set.arc(dag, from = 'operation', to = 'shape')
  dag <- set.arc(dag, from = 'operation', to = 'ht')

  dag <- set.arc(dag, from = 'condition', to = 'mass')
  dag <- set.arc(dag, from = 'condition', to = 'form')
  dag <- set.arc(dag, from = 'condition', to = 'mod')
  dag <- set.arc(dag, from = 'condition', to = 'rad')
  dag <- set.arc(dag, from = 'condition', to = 'ref')
  dag <- set.arc(dag, from = 'condition', to = 'dim')
  dag <- set.arc(dag, from = 'condition', to = 'shape')
  dag <- set.arc(dag, from = 'condition', to = 'ht')

  # build conditional probability tables
  operation <- c('large sample', 'machining', 'metallurgy', 'small sample', 'solution', 'waste')
  condition <- c('A', 'B', 'C', 'D', 'E', 'M', 'P')
  mass <- c(seq(10, 1000, 10), 0) # 100 + 1 bins
  form <- c('alpha', 'oxide', 'none')
  mod <- c('ch2', 'sepiolite', 'h2o', 'none')
  rad <- c(seq(0.2, 18, 0.2) * 2.54, 0) # 90 + 1 bins
  ref <- c('al', 'be', 'beo', 'cs', 'cu', 'graphite', 'ch2', 'ss304', 'ta', 'v', 'h2o', 'none')
  dim <- c(seq(0.2, 18, 0.2) * 2.54, 0) # 90 + 1 bins
  shape <- c('sph', 'rcc', 'none')
  ht <- c(seq(0.2, 36, 0.2) * 2.54, 0) # 180 + 1 bins

  operation.cpt <- matrix(c(
    3.61111e-01 ,  # large sample
    8.33333e-02 ,  # machining
    1.38889e-01 ,  # metallurgy
    2.77778e-01 ,  # small sample
    1.11111e-01 ,  # solution
    2.77778e-02 ), # waste
    ncol = 1, dimnames = list(operation, NULL))

  condition.cpt <- c(
  # A             B             C             D             E             M             P
    5.71420e-01 , 2.85714e-02 , 2.85714e-02 , 1.71429e-01 , 8.57143e-02 , 1.14286e-01 , 0           , # large sample
    8.33333e-02 , 0           , 0           , 0           , 0           , 9.16667e-01 , 0           , # machining
    6.15385e-01 , 7.69231e-02 , 0           , 0           , 3.07692e-01 , 0           , 0           , # metallurgy
    9.37500e-01 , 6.25000e-02 , 0           , 0           , 0           , 0           , 0           , # small sample
    1           , 0           , 0           , 0           , 0           , 0           , 0           , # solution
    0           , 0           , 0           , 0           , 0           , 0           , 1           ) # waste

  dim(condition.cpt) <- c(7, 6)
  dimnames(condition.cpt) <- list('condition' = condition, 'operation' = operation)

  mass.cpt <- load('mass_cpt.RData')
  dim(mass.cpt) <- c(1001, 7, 6)
  dimnames(mass.cpt) <- list('mass' = mass, 'condition' = condition, 'operation' = operation)

  rad.cpt <- load('rad_cpt.RData')
  dim(rad.cpt) <- c(181, 7, 6)
  dimnames(rad.cpt) <- list('rad' = rad, 'condition' = condition, 'operation' = operation)

  dim.cpt <- load('dim_cpt.RData')
  dim(dim.cpt) <- c(181, 7, 6)
  dimnames(dim.cpt) <- list('dim' = dim, 'condition' = condition, 'operation' = operation)

  ht.cpt <- load('ht_cpt.RData')
  dim(ht.cpt) <- c(361, 7, 6)
  dimnames(ht.cpt) <- list('ht' = ht, 'condition' = condition, 'operation' = operation)

  form.cpt <- c(
  # large sample
    5.60000e-01 , 4.40000e-01 , 0           , # A
    1           , 0           , 0           , # B
    0           , 1           , 0           , # C
    1           , 0           , 0           , # D
    1           , 0           , 0           , # E
    1           , 0           , 0           , # M
    0           , 0           , 1           , # P (NULL)
  # machining
    1           , 0           , 0           , # A
    0           , 0           , 1           , # B (NULL)
    0           , 0           , 1           , # C (NULL)
    0           , 0           , 1           , # D (NULL)
    0           , 0           , 1           , # E (NULL)
    1           , 0           , 0           , # M
    0           , 0           , 1           , # P (NULL)
  # metallurgy
    7.50000e-01 , 2.50000e-01 , 0           , # A
    1           , 0           , 0           , # B
    0           , 0           , 1           , # C (NULL)
    0           , 0           , 1           , # D (NULL)
    5.00000e-01 , 5.00000e-01 , 0           , # E
    0           , 0           , 1           , # M (NULL)
    0           , 0           , 1           , # P (NULL)
  # small sample
    1           , 0           , 0           , # A
    0           , 1           , 0           , # B
    0           , 0           , 1           , # C (NULL)
    0           , 0           , 1           , # D (NULL)
    0           , 0           , 1           , # E (NULL)
    0           , 0           , 1           , # M (NULL)
    0           , 0           , 1           , # P (NULL)
  # solution
    7.50000e-01 , 2.50000e-01 , 0           , # A
    0           , 0           , 1           , # B (NULL)
    0           , 0           , 1           , # C (NULL)
    0           , 0           , 1           , # D (NULL)
    0           , 0           , 1           , # E (NULL)
    0           , 0           , 1           , # M (NULL)
    0           , 0           , 1           , # P (NULL)
  # waste
    0           , 0           , 1           , # A (NULL)
    0           , 0           , 1           , # B (NULL)
    0           , 0           , 1           , # C (NULL)
    0           , 0           , 1           , # D (NULL)
    0           , 0           , 1           , # E (NULL)
    0           , 0           , 1           , # M (NULL)
    0           , 1           , 0           ) # P

  dim(form.cpt) <- c(3, 7, 6)
  dimnames(form.cpt) <- list('form' = form, 'condition' = condition, 'operation' = operation)

  mod.cpt <- c(
  # large sample
    2.38095e-02 , 3.57143e-02 , 2.38095e-02 , 9.16667e-01 , # A
    0           , 0           , 0           , 1           , # B
    6.25000e-02 , 9.37500e-02 , 6.25000e-02 , 7.81250e-01 , # C
    3.12500e-02 , 4.68750e-02 , 3.12500e-02 , 8.90625e-01 , # D
    0           , 0           , 0           , 1           , # E
    0           , 0           , 0           , 1           , # M
    0           , 0           , 0           , 1           , # P (NULL)
  # machining
    0           , 0           , 5.00000e-02 , 9.50000e-01 , # A
    0           , 0           , 0           , 1           , # B (NULL)
    0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 1           , # E (NULL)
    2.50000e-02 , 0           , 2.50000e-02 , 9.50000e-01 , # M
    0           , 0           , 0           , 1           , # P (NULL)
  # metallurgy
    0           , 0           , 0           , 1           , # A
    0           , 0           , 0           , 1           , # B
    0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 1           , # E
    0           , 0           , 0           , 1           , # M (NULL)
    0           , 0           , 0           , 1           , # P (NULL)
  # small sample
    2.50000e-02 , 0           , 2.50000e-02 , 9.25000e-01 , # A
    2.50000e-02 , 0           , 2.50000e-02 , 9.25000e-01 , # B
    0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 1           , # E (NULL)
    0           , 0           , 0           , 1           , # M (NULL)
    0           , 0           , 0           , 1           , # P (NULL)
  # solution
    0           , 0           , 1           , 0           , # A
    0           , 0           , 0           , 1           , # B (NULL)
    0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 1           , # E (NULL)
    0           , 0           , 0           , 1           , # M (NULL)
    0           , 0           , 0           , 1           , # P (NULL)
  # waste
    0           , 0           , 0           , 1           , # A (NULL)
    0           , 0           , 0           , 1           , # B (NULL)
    0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 1           , # E (NULL)
    0           , 0           , 0           , 1           , # M (NULL)
    2.50000e-01 , 5.00000e-01 , 0           , 2.50000e-01 ) # P

  dim(mod.cpt) <- c(4, 7, 6)
  dimnames(mod.cpt) <- list('mod' = mod, 'condition' = condition, 'operation' = operation)

  ref.cpt <- c(
  # large sample
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # A
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # B
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # C
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # D
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # E
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # M
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # P (NULL)
  # machining
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # A
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # B (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # E (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # M
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # P (NULL)
  # metallurgy
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # A
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # B
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # E
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # M (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # P (NULL)
  # small sample
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # A
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # B
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # E (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # M (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # P (NULL)
  # solution
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # A
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # B (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # E (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # M (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # P (NULL)
  # waste
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # A (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # B (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # E (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # M (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           ) # P

  dim(ref.cpt) <- c(12, 7, 6)
  dimnames(ref.cpt) <- list('ref' = ref, 'condition' = condition, 'operation' = operation)

  shape.cpt <- c(
  # large sample
    0           , 0           , 0           , # A
    0           , 0           , 0           , # B
    0           , 0           , 0           , # C
    0           , 0           , 0           , # D
    0           , 0           , 0           , # E
    0           , 0           , 0           , # M
    0           , 0           , 1           , # P (NULL)
  # machining
    0           , 0           , 0           , # A
    0           , 0           , 1           , # B (NULL)
    0           , 0           , 1           , # C (NULL)
    0           , 0           , 1           , # D (NULL)
    0           , 0           , 1           , # E (NULL)
    0           , 0           , 0           , # M
    0           , 0           , 1           , # P (NULL)
  # metallurgy
    0           , 0           , 0           , # A
    0           , 0           , 0           , # B
    0           , 0           , 1           , # C (NULL)
    0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , # E
    0           , 0           , 1           , # M (NULL)
    0           , 0           , 1           , # P (NULL)
  # small sample
    0           , 0           , 0           , # A
    0           , 0           , 0           , # B
    0           , 0           , 1           , # C (NULL)
    0           , 0           , 1           , # D (NULL)
    0           , 0           , 1           , # E (NULL)
    0           , 0           , 1           , # M (NULL)
    0           , 0           , 1           , # P (NULL)
  # solution
    0           , 0           , 0           , # A
    0           , 0           , 1           , # B (NULL)
    0           , 0           , 1           , # C (NULL)
    0           , 0           , 1           , # D (NULL)
    0           , 0           , 1           , # E (NULL)
    0           , 0           , 1           , # M (NULL)
    0           , 0           , 1           , # P (NULL)
  # waste
    0           , 0           , 1           , # A (NULL)
    0           , 0           , 1           , # B (NULL)
    0           , 0           , 1           , # C (NULL)
    0           , 0           , 1           , # D (NULL)
    0           , 0           , 1           , # E (NULL)
    0           , 0           , 1           , # M (NULL)
    0           , 0           , 0           ) # P

  dim(shape.cpt) <- c(3, 7, 6)
  dimnames(shape.cpt) <- list('shape' = shape, 'condition' = condition, 'operation' = operation)

  graphviz.plot(dag)
  dag

  # bn <- list(
  #   operation = operation.cpt,
  #   condition = condition.cpt,
  #   mass = mass.cpt,
  #   form = form.cpt,
  #   mod = mod.cpt,
  #   rad = rad.cpt,
  #   ref = ref.cpt,
  #   dim = dim.cpt,
  #   shape = shape.cpt,
  #   ht = ht.cpt)

  bn <- list(
    mass = mass.cpt,
    form = form.cpt,
    mod = mod.cpt,
    rad = rad.cpt,
    ref = ref.cpt,
    dim = dim.cpt,
    shape = shape.cpt,
    ht = ht.cpt)

  # check for errors
	for (i in 1:length(bn)) {
		for (j in 1:dim(bn[[i]])[2]) {
			for (k in 1:dim(bn[[i]])[3]) {
				if (sum(bn[[i]][ , j, k]) != 1) {
					cat(names(bn[i]), ' sum(bn[[', i, ']][ , ', j, ', ', k, ']) = ', sum(bn[[i]][ , j, k]), '\n', sep = '')
				}
			}
		}
	}

  # bn <<- custom.fit(dag, dist = bn)

  bn <<- custom.fit(dag, dist = c(list(operation = operation.cpt, condition = condition.cpt), bn))

}
