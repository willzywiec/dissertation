# bn.R
#
# William John Zywiec
# The George Washington University
#
# ...

BN <- function() {

  # load packages
  library(bnlearn)
  library(igraph)

  nodes <- c('op', 'ctrl', 'mass', 'form', 'mod', 'rad', 'ref', 'dim', 'shape', 'ht')

  # plot graph
  dag <- graph_from_data_frame(
    d = data.frame(
      from = c(rep(1, 9), rep(2, 8)),
      to = c(seq(2, 10), seq(3, 10))),
    vertices = data.frame(
      from = c(rep(1, 9), rep(2, 8)),
      to = c(seq(2, 10), seq(3, 10))))
  
  dag.layout <- matrix(
    data = c(0, 0, -3, -2.25, -1.5, -0.75, 0.75, 1.5, 2.25, 3, 3, 1.5, rep(0, 8)),
    nrow = 10,
    ncol = 2)

  plot(
    dag,
    edge.arrow.size = 0.3,
    edge.color = '#FC4E07',
    layout = dag.layout,
    rescale = FALSE,
    vertex.color = '#FD9268',
    vertex.frame.color = '#FC4E07',
    vertex.label.color = 'black',
    vertex.label.family = 'serif',
    vertex.size = 60,
    xlim = range(dag.layout[]),
    ylim = range(dag.layout[]))

  # build graph
  dag <- empty.graph(nodes = nodes)

  for (i in 2:length(nodes)) {
    dag <- set.arc(dag, 'operation', nodes[i])
    if (i > 2) {
      dag <- set.arc(dag, 'control', nodes[i])
    }
  }

  # build conditional probability tables
  op <- c('large sample', 'machining', 'metallurgy', 'small sample', 'solution', 'waste')
  ctrl <- c('A', 'B', 'C', 'D', 'E', 'M', 'P')
  mass <- c(seq(10, 1000, 10), 0) # 100 + 1 bins
  form <- c('alpha', 'oxide', 'none')
  mod <- c('ch2', 'sepiolite', 'h2o', 'none')
  rad <- c(seq(0.2, 18, 0.2) * 2.54, 0) # 90 + 1 bins
  ref <- c('al', 'be', 'beo', 'cs', 'cu', 'graphite', 'ch2', 'ss304', 'ta', 'v', 'h2o', 'none')
  dim <- c(seq(0.2, 18, 0.2) * 2.54, 0) # 90 + 1 bins
  shape <- c('sph', 'rcc', 'none')
  ht <- c(seq(0.2, 36, 0.2) * 2.54, 0) # 180 + 1 bins

  op.cpt <- matrix(c(
    3.61111e-01 ,  # large sample
    8.33333e-02 ,  # machining
    1.38889e-01 ,  # metallurgy
    2.77778e-01 ,  # small sample
    1.11111e-01 ,  # solution
    2.77778e-02 ), # waste
    ncol = 1, dimnames = list(operation, NULL))

  ctrl.cpt <- c(
  # A             B             C             D             E             M             P
    5.71420e-01 , 2.85714e-02 , 2.85714e-02 , 1.71429e-01 , 8.57143e-02 , 1.14286e-01 , 0           , # large sample
    8.33333e-02 , 0           , 0           , 0           , 0           , 9.16667e-01 , 0           , # machining
    6.15385e-01 , 7.69231e-02 , 0           , 0           , 3.07692e-01 , 0           , 0           , # metallurgy
    9.37500e-01 , 6.25000e-02 , 0           , 0           , 0           , 0           , 0           , # small sample
    1           , 0           , 0           , 0           , 0           , 0           , 0           , # solution
    0           , 0           , 0           , 0           , 0           , 0           , 1           ) # waste

  dim(control.cpt) <- c(7, 6)
  dimnames(control.cpt) <- list('control' = control, 'operation' = operation)

  mass.cpt <- readRDS('mass.RData')
  dim(mass.cpt) <- c(1001, 7, 6)
  dimnames(mass.cpt) <- list('mass' = mass, 'control' = control, 'operation' = operation)

  rad.cpt <- readRDS('rad.RData')
  dim(rad.cpt) <- c(181, 7, 6)
  dimnames(rad.cpt) <- list('rad' = rad, 'control' = control, 'operation' = operation)

  dim.cpt <- readRDS('dim.RData')
  dim(dim.cpt) <- c(181, 7, 6)
  dimnames(dim.cpt) <- list('dim' = dim, 'control' = control, 'operation' = operation)

  ht.cpt <- readRDS('ht.RData')
  dim(ht.cpt) <- c(361, 7, 6)
  dimnames(ht.cpt) <- list('ht' = ht, 'control' = control, 'operation' = operation)

  form.cpt <- c(
  # large sample
    0           , 4.40000e-01 , 0           , # A
    0           , 0           , 0           , # B
    0           , 1           , 0           , # C
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
    0           , 2.50000e-01 , 0           , # A
    0           , 0           , 0           , # B
    0           , 0           , 1           , # C (NULL)
    0           , 0           , 1           , # D (NULL)
    0           , 5.00000e-01 , 0           , # E
    0           , 0           , 1           , # M (NULL)
    0           , 0           , 1           , # P (NULL)
  # small sample
    0           , 0           , 0           , # A
    0           , 1           , 0           , # B
    0           , 0           , 1           , # C (NULL)
    0           , 0           , 1           , # D (NULL)
    0           , 0           , 1           , # E (NULL)
    0           , 0           , 1           , # M (NULL)
    0           , 0           , 1           , # P (NULL)
  # solution
    0           , 2.50000e-01 , 0           , # A
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
  dimnames(form.cpt) <- list('form' = form, 'control' = control, 'operation' = operation)

  mod.cpt <- c(
  # large sample
    0           , 2.38095e-02 , 3.57143e-02 , 2.38095e-02 , 9.16667e-01 , # A
    0           , 0           , 0           , 0           , 1           , # B
    0           , 6.25000e-02 , 9.37500e-02 , 6.25000e-02 , 7.81250e-01 , # C
    0           , 3.12500e-02 , 4.68750e-02 , 3.12500e-02 , 8.90625e-01 , # D
    0           , 0           , 0           , 0           , 1           , # E
    0           , 0           , 0           , 0           , 1           , # M
    0           , 0           , 0           , 0           , 1           , # P (NULL)
  # machining
    0           , 0           , 0           , 5.00000e-02 , 9.50000e-01 , # A
    0           , 0           , 0           , 0           , 1           , # B (NULL)
    0           , 0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 0           , 1           , # E (NULL)
    0           , 2.50000e-02 , 0           , 2.50000e-02 , 9.50000e-01 , # M
    0           , 0           , 0           , 0           , 1           , # P (NULL)
  # metallurgy
    0           , 0           , 0           , 0           , 1           , # A
    0           , 0           , 0           , 0           , 1           , # B
    0           , 0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 0           , 1           , # E
    0           , 0           , 0           , 0           , 1           , # M (NULL)
    0           , 0           , 0           , 0           , 1           , # P (NULL)
  # small sample
    0           , 2.50000e-02 , 0           , 2.50000e-02 , 9.25000e-01 , # A
    0           , 2.50000e-02 , 0           , 2.50000e-02 , 9.25000e-01 , # B
    0           , 0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 0           , 1           , # E (NULL)
    0           , 0           , 0           , 0           , 1           , # M (NULL)
    0           , 0           , 0           , 0           , 1           , # P (NULL)
  # solution
    0           , 0           , 0           , 1           , 0           , # A
    0           , 0           , 0           , 0           , 1           , # B (NULL)
    0           , 0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 0           , 1           , # E (NULL)
    0           , 0           , 0           , 0           , 1           , # M (NULL)
    0           , 0           , 0           , 0           , 1           , # P (NULL)
  # waste
    0           , 0           , 0           , 0           , 1           , # A (NULL)
    0           , 0           , 0           , 0           , 1           , # B (NULL)
    0           , 0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 0           , 1           , # E (NULL)
    0           , 0           , 0           , 0           , 1           , # M (NULL)
    0           , 2.50000e-01 , 5.00000e-01 , 0           , 2.50000e-01 ) # P

  dim(mod.cpt) <- c(5, 7, 6)
  dimnames(mod.cpt) <- list('mod' = mod, 'control' = control, 'operation' = operation)

  ref.cpt <- c(
  # large sample
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # A
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # B
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # C
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # D
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # E
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # M
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # P (NULL)
  # machining
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # A
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # B (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # E (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # M
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # P (NULL)
  # metallurgy
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # A
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # B
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # E
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # M (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # P (NULL)
  # small sample
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # A
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # B
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # E (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # M (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # P (NULL)
  # solution
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , # A
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # B (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # E (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # M (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # P (NULL)
  # waste
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # A (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # B (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # C (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # D (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # E (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           , # M (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           ) # P

  dim(ref.cpt) <- c(10, 7, 6)
  dimnames(ref.cpt) <- list('ref' = ref, 'control' = control, 'operation' = operation)

  shape.cpt <- c(
  # large sample
    0           , 0           , # A
    0           , 0           , # B
    0           , 0           , # C
    0           , 0           , # D
    0           , 0           , # E
    0           , 0           , # M
    0           , 1           , # P (NULL)
  # machining
    0           , 0           , # A
    0           , 1           , # B (NULL)
    0           , 1           , # C (NULL)
    0           , 1           , # D (NULL)
    0           , 1           , # E (NULL)
    0           , 0           , # M
    0           , 1           , # P (NULL)
  # metallurgy
    0           , 0           , # A
    0           , 0           , # B
    0           , 1           , # C (NULL)
    0           , 1           , # D (NULL)
    0           , 0           , # E
    0           , 1           , # M (NULL)
    0           , 1           , # P (NULL)
  # small sample
    0           , 0           , # A
    0           , 0           , # B
    0           , 1           , # C (NULL)
    0           , 1           , # D (NULL)
    0           , 1           , # E (NULL)
    0           , 1           , # M (NULL)
    0           , 1           , # P (NULL)
  # solution
    0           , 0           , # A
    0           , 1           , # B (NULL)
    0           , 1           , # C (NULL)
    0           , 1           , # D (NULL)
    0           , 1           , # E (NULL)
    0           , 1           , # M (NULL)
    0           , 1           , # P (NULL)
  # waste
    0           , 1           , # A (NULL)
    0           , 1           , # B (NULL)
    0           , 1           , # C (NULL)
    0           , 1           , # D (NULL)
    0           , 1           , # E (NULL)
    0           , 1           , # M (NULL)
    0           , 0           ) # P

  dim(shape.cpt) <- c(2, 7, 6)
  dimnames(shape.cpt) <- list('shape' = shape, 'control' = control, 'operation' = operation)

  # bn <- list(
  #   operation = operation.cpt,
  #   control = control.cpt,
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

  bn <<- custom.fit(dag, dist = c(list(operation = operation.cpt, control = control.cpt), bn))

}
