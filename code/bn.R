# bn.R
#
# William John Zywiec
# The George Washington University

BN <- function(dist, main.dir) {

  library(bnlearn)

  dist.dir <- paste0(main.dir, '/dist')
  dir.create(dist.dir, showWarnings = FALSE)
  
  setwd(dist.dir)

  # build graph
  nodes <- c('op', 'ctrl', 'mass', 'form', 'mod', 'rad', 'ref', 'thk')
  dag <- empty.graph(nodes = nodes)

  for (i in 2:length(nodes)) {
    dag <- set.arc(dag, 'op', nodes[i])
    if (i > 2) {
      dag <- set.arc(dag, 'ctrl', nodes[i])
    }
  }

  # build conditional probability tables
  op <- c('large sample', 'machining', 'metallurgy', 'small sample', 'solution', 'waste')
  ctrl <- c('A', 'B', 'C', 'D', 'E', 'M', 'P')
  mass <- seq(0, 4000, 1)
  form <- c('alpha', 'puo2')
  mod <- c('mgo', 'ch2', 'sepiolite', 'h2o', 'none')
  rad <- seq(0, 18, 0.25) * 2.54
  ref <- c('al', 'be', 'du', 'graphite', 'pb', 'mgo', 'ch2', 'ss304', 'h2o', 'none')
  thk <- seq(0, 2, 0.25) * 2.54

  op.cpt <- matrix(c(
    3.50e-01 ,  # large sample
    7.50e-02 ,  # machining
    1.25e-01 ,  # metallurgy
    3.00e-01 ,  # small sample
    1.00e-01 ,  # solution
    5.00e-02 ), # waste
    ncol = 1, dimnames = list(op, NULL))

  ctrl.cpt <- matrix(c(
    # A           B             C             D             E             M             P
    5.71429e-01 , 2.85714e-02 , 2.85714e-02 , 1.71428e-01 , 8.57142e-02 , 1.14286e-01 , 0           ,  # large sample
    1.53846e-01 , 0           , 0           , 0           , 0           , 8.46154e-01 , 0           ,  # machining
    6.15385e-01 , 7.69231e-02 , 0           , 0           , 3.07692e-01 , 0           , 0           ,  # metallurgy
    9.37500e-01 , 6.25000e-02 , 0           , 0           , 0           , 0           , 0           ,  # small sample
    1           , 0           , 0           , 0           , 0           , 0           , 0           ,  # solution
    5.00000e-01 , 0           , 0           , 0           , 0           , 0           , 5.00000e-01 ), # waste
    nrow = 7, ncol = 6, dimnames = list(ctrl, op))

  mass.cpt <- array(unlist(readRDS(paste0('mass-', dist, '.RData'))), dim = c(4001, 7, 6), dimnames = list('mass' = mass, 'ctrl' = ctrl, 'op' = op))
  rad.cpt <- array(unlist(readRDS(paste0('rad-', dist, '.RData'))), dim = c(73, 7, 6), dimnames = list('rad' = rad, 'ctrl' = ctrl, 'op' = op))
  thk.cpt <- array(unlist(readRDS(paste0('thk-', dist, '.RData'))), dim = c(9, 7, 6), dimnames = list('thk' = thk, 'ctrl' = ctrl, 'op' = op))

  form.cpt <- array(c(
  # large sample
    9.50000E-01 , 5.00000e-02 ,  # A
    1           , 0           ,  # B
    1           , 0           ,  # C
    1           , 0           ,  # D
    1           , 0           ,  # E
    1           , 0           ,  # M
    1           , 0           ,  # P (NULL)
  # machining
    1           , 0           ,  # A
    1           , 0           ,  # B (NULL)
    1           , 0           ,  # C (NULL)
    1           , 0           ,  # D (NULL)
    1           , 0           ,  # E (NULL)
    1           , 0           ,  # M
    1           , 0           ,  # P (NULL)
  # metallurgy
    9.50000E-01 , 5.00000e-02 ,  # A
    1           , 0           ,  # B
    1           , 0           ,  # C (NULL)
    1           , 0           ,  # D (NULL)
    5.00000e-01 , 5.00000e-01 ,  # E
    1           , 0           ,  # M (NULL)
    1           , 0           ,  # P (NULL)
  # small sample
    1           , 0           ,  # A
    1           , 0           ,  # B
    1           , 0           ,  # C (NULL)
    1           , 0           ,  # D (NULL)
    1           , 0           ,  # E (NULL)
    1           , 0           ,  # M (NULL)
    1           , 0           ,  # P (NULL)
  # solution
    9.50000E-01 , 5.00000e-02 ,  # A
    1           , 0           ,  # B (NULL)
    1           , 0           ,  # C (NULL)
    1           , 0           ,  # D (NULL)
    1           , 0           ,  # E (NULL)
    1           , 0           ,  # M (NULL)
    1           , 0           ,  # P (NULL)
  # waste
    0           , 1           ,  # A
    1           , 0           ,  # B (NULL)
    1           , 0           ,  # C (NULL)
    1           , 0           ,  # D (NULL)
    1           , 0           ,  # E (NULL)
    1           , 0           ,  # M (NULL)
    0           , 1           ), # P
    dim = c(2, 7, 6), dimnames = list('form' = form, 'ctrl' = ctrl, 'op' = op))

  mod.cpt <- array(c(
  # large sample
    8.72522E-02 , 1.62535E-01 , 2.12734E-04 , 0           , 7.50000E-01 ,  # A
    8.72522E-02 , 1.62535E-01 , 2.12734E-04 , 0           , 7.50000E-01 ,  # B
    8.72522E-02 , 1.62535E-01 , 2.12734E-04 , 0           , 7.50000E-01 ,  # C
    8.72522E-02 , 1.62535E-01 , 2.12734E-04 , 0           , 7.50000E-01 ,  # D
    1.74504E-02 , 3.25070E-02 , 4.25468E-05 , 0           , 9.50000E-01 ,  # E
    8.72522E-02 , 1.62535E-01 , 2.12734E-04 , 0           , 7.50000E-01 ,  # M
    0           , 0           , 0           , 0           , 1           ,  # P (NULL)
  # machining
    6.49960E-03 , 2.42690E-01 , 8.10672E-04 , 0           , 7.50000E-01 ,  # A
    0           , 0           , 0           , 0           , 1           ,  # B (NULL)
    0           , 0           , 0           , 0           , 1           ,  # C (NULL)
    0           , 0           , 0           , 0           , 1           ,  # D (NULL)
    0           , 0           , 0           , 0           , 1           ,  # E (NULL)
    6.49960E-03 , 2.42690E-01 , 8.10672E-04 , 0           , 7.50000E-01 ,  # M
    0           , 0           , 0           , 0           , 1           ,  # P (NULL)
  # metallurgy
    6.31463E-02 , 1.86759E-01 , 9.47006E-05 , 0           , 7.50000E-01 ,  # A
    6.31463E-02 , 1.86759E-01 , 9.47006E-05 , 0           , 7.50000E-01 ,  # B
    0           , 0           , 0           , 0           , 1           ,  # C (NULL)
    0           , 0           , 0           , 0           , 1           ,  # D (NULL)
    1.26293E-02 , 3.73518E-02 , 1.89401E-05 , 0           , 9.50000E-01 ,  # E
    0           , 0           , 0           , 0           , 1           ,  # M (NULL)
    0           , 0           , 0           , 0           , 1           ,  # P (NULL)
  # small sample
    1.66387E-02 , 2.33079E-01 , 2.82013E-04 , 0           , 7.50000E-01 ,  # A
    1.66387E-02 , 2.33079E-01 , 2.82013E-04 , 0           , 7.50000E-01 ,  # B
    0           , 0           , 0           , 0           , 1           ,  # C (NULL)
    0           , 0           , 0           , 0           , 1           ,  # D (NULL)
    0           , 0           , 0           , 0           , 1           ,  # E (NULL)
    0           , 0           , 0           , 0           , 1           ,  # M (NULL)
    0           , 0           , 0           , 0           , 1           ,  # P (NULL)
  # solution
    3.46069E-04 , 2.48339E-01 , 1.31506E-03 , 0           , 7.50000E-01 ,  # A
    0           , 0           , 0           , 0           , 1           ,  # B (NULL)
    0           , 0           , 0           , 0           , 1           ,  # C (NULL)
    0           , 0           , 0           , 0           , 1           ,  # D (NULL)
    0           , 0           , 0           , 0           , 1           ,  # E (NULL)
    0           , 0           , 0           , 0           , 1           ,  # M (NULL)
    0           , 0           , 0           , 0           , 1           ,  # P (NULL)
  # waste
    7.84143E-02 , 1.66521E-01 , 5.06462E-03 , 0           , 7.50000E-01 ,  # A
    0           , 0           , 0           , 0           , 1           ,  # B (NULL)
    0           , 0           , 0           , 0           , 1           ,  # C (NULL)
    0           , 0           , 0           , 0           , 1           ,  # D (NULL)
    0           , 0           , 0           , 0           , 1           ,  # E (NULL)
    0           , 0           , 0           , 0           , 1           ,  # M (NULL)
    7.84143E-02 , 1.66521E-01 , 5.06462E-03 , 0           , 7.50000E-01 ), # P
    dim = c(5, 7, 6), dimnames = list('mod' = mod, 'ctrl' = ctrl, 'op' = op))

  ref.cpt <- array(c(
  # large sample
    2.51452E-02 , 1.62768E-02 , 1.62768E-02 , 1.96831E-04 , 1.62768E-02 , 2.28734E-01 , 4.26090E-01 , 2.71004E-01 , 0           , 0           ,  # A
    2.51452E-02 , 1.62768E-02 , 1.62768E-02 , 1.96831E-04 , 1.62768E-02 , 2.28734E-01 , 4.26090E-01 , 2.71004E-01 , 0           , 0           ,  # B
    1.25726E-03 , 8.13842E-04 , 8.13842E-04 , 9.84155E-06 , 8.13842E-04 , 1.14367E-02 , 2.13045E-02 , 1.35502E-02 , 0           , 9.50000E-01 ,  # C
    2.51452E-02 , 1.62768E-02 , 1.62768E-02 , 1.96831E-04 , 1.62768E-02 , 2.28734E-01 , 4.26090E-01 , 2.71004E-01 , 0           , 0           ,  # D
    2.51452E-02 , 1.62768E-02 , 1.62768E-02 , 1.96831E-04 , 1.62768E-02 , 2.28734E-01 , 4.26090E-01 , 2.71004E-01 , 0           , 0           ,  # E
    2.51452E-02 , 1.62768E-02 , 1.62768E-02 , 1.96831E-04 , 1.62768E-02 , 2.28734E-01 , 4.26090E-01 , 2.71004E-01 , 0           , 0           ,  # M
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # P (NULL)
  # machining
    6.56559E-02 , 1.85162E-02 , 1.85162E-02 , 4.17659E-05 , 1.85162E-02 , 1.90870E-02 , 7.12693E-01 , 1.46974E-01 , 0           , 0           ,  # A
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # B (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # C (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # D (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # E (NULL)
    6.56559E-02 , 1.85162E-02 , 1.85162E-02 , 4.17659E-05 , 1.85162E-02 , 1.90870E-02 , 7.12693E-01 , 1.46974E-01 , 0           , 0           ,  # M
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # P (NULL)
  # metallurgy
    3.18113E-02 , 1.66920E-02 , 1.66920E-02 , 2.17439E-04 , 1.66920E-02 , 1.44988E-01 , 4.28811E-01 , 3.44097E-01 , 0           , 0           ,  # A
    3.18113E-02 , 1.66920E-02 , 1.66920E-02 , 2.17439E-04 , 1.66920E-02 , 1.44988E-01 , 4.28811E-01 , 3.44097E-01 , 0           , 0           ,  # B
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # C (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # D (NULL)
    3.18113E-02 , 1.66920E-02 , 1.66920E-02 , 2.17439E-04 , 1.66920E-02 , 1.44988E-01 , 4.28811E-01 , 3.44097E-01 , 0           , 0           ,  # E
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # M (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # P (NULL)
  # small sample
    4.79521E-02 , 1.32551E-02 , 1.32551E-02 , 5.13268E-04 , 1.32551E-02 , 5.14808E-02 , 7.21154E-01 , 1.39134E-01 , 0           , 0           ,  # A
    4.79521E-02 , 1.32551E-02 , 1.32551E-02 , 5.13268E-04 , 1.32551E-02 , 5.14808E-02 , 7.21154E-01 , 1.39134E-01 , 0           , 0           ,  # B
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # C (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # D (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # E (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # M (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # P (NULL)
  # solution
    8.46767E-02 , 4.01848E-02 , 4.01848E-02 , 0           , 4.01848E-02 , 9.40852E-04 , 6.75155E-01 , 1.18673E-01 , 0           , 0           ,  # A
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # B (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # C (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # D (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # E (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # M (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # P (NULL)
  # waste
    1.59817E-02 , 5.38644E-02 , 5.38644E-02 , 2.53678E-04 , 5.38644E-02 , 2.27803E-01 , 4.83765E-01 , 1.10604E-01 , 0           , 0           ,  # A
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # B (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # C (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # D (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # E (NULL)
    0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 0           , 1           ,  # M (NULL)
    1.59817E-02 , 5.38644E-02 , 5.38644E-02 , 2.53678E-04 , 5.38644E-02 , 2.27803E-01 , 4.83765E-01 , 1.10604E-01 , 0           , 0           ), # P
    dim = c(10, 7, 6), dimnames = list('ref' = ref, 'ctrl' = ctrl, 'op' = op))

  bn <- list(
    op = op.cpt,
    ctrl = ctrl.cpt,
    mass = mass.cpt,
    form = form.cpt,
    mod = mod.cpt,
    rad = rad.cpt,
    ref = ref.cpt,
    thk = thk.cpt)
  
  bn <- custom.fit(dag, dist = bn)

  return(bn)

}
