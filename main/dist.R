# dist.R
#
# William John Zywiec
# The George Washington University
#
# ...

# initialize environment
if (!is.null(dev.list())) dev.off()
rm(list = ls())
cat('\014')

# set precision
options(digits = 15)

# set test directory
test.dir <- 'C:/Users/Will/Desktop/test'
setwd(test.dir)

# 1 = large sample
# 2 = machining
# 3 = metallurgy
# 4 = small sample
# 5 = solution
# 6 = waste

mass.1 <- c(27, 10, 8, 0, 2, 8, 1, 1, 0, 0, 50, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
mass.2 <- c(49, 44, 1, 1, 0, 0, 0, 0, 0, 0, 0)
mass.3 <- c(149, 6, 215, 210, 9, 0, 0, 0, 0, 0, 0, 0, 0)
mass.4 <- c(54, 26, 1, 2, 1, 14, 63, 101, 0, 1, 4, 0, 0, 0, 0, 0, 0)
mass.5 <- c(10, 22, 0, 0, 0, 0)
mass.6 <- c(54, 115)

mass <- list(mass.1, mass.2, mass.3, mass.4, mass.5, mass.6)
mass.a <- mass.b <- mass.c <- mass.d <- mass.e <- mass.m <- mass.p <- list()

rad.1 <- c()
rad.2 <- c()
rad.3 <- c()
rad.4 <- c()
rad.5 <- c()
rad.6 <- c()

rad <- list(rad.1, rad.2, rad.3, rad.4, rad.5, rad.6)
rad.a <- rad.b <- rad.c <- rad.d <- rad.e <- rad.m <- rad.p <- list()

dim.1 <- c()
dim.2 <- c()
dim.3 <- c()
dim.4 <- c()
dim.5 <- c()
dim.6 <- c()

dim <- list(dim.1, dim.2, dim.3, dim.4, dim.5, dim.6)
dim.a <- dim.b <- dim.c <- dim.d <- dim.e <- dim.m <- dim.p <- list()

ht.1 <- c()
ht.2 <- c()
ht.3 <- c()
ht.4 <- c()
ht.5 <- c()
ht.6 <- c()

ht <- list(ht.1, ht.2, ht.3, ht.4, ht.5, ht.6)
ht.a <- ht.b <- ht.c <- ht.d <- ht.e <- ht.m <- ht.p <- list()

for (i in 1:6) {

  mass.a[[i]] <- mass[[i]][which(sapply(mass[[i]], function(x) x <= 65))]
  mass.b[[i]] <- mass[[i]][which(sapply(mass[[i]], function(x) x <= 220))]
  mass.c[[i]] <- mass[[i]][which(sapply(mass[[i]], function(x) x <= 1200))]
  mass.d[[i]] <- mass[[i]][which(sapply(mass[[i]], function(x) x <= 2500))]
  mass.e[[i]] <- mass[[i]][which(sapply(mass[[i]], function(x) x <= 2500))]
  mass.m[[i]] <- mass[[i]][which(sapply(mass[[i]], function(x) x <= 4000))]
  mass.p[[i]] <- mass[[i]][which(sapply(mass[[i]], function(x) x <= 300))]

  rad.a[[i]] <- rad[[i]]
  rad.b[[i]] <- rad[[i]][which(sapply(rad[[i]], function(x) x <= (3000 / pi)^(1/3)))] # 9.85 cm
  rad.c[[i]] <- rad[[i]][which(sapply(rad[[i]], function(x) x <= (1875 / pi)^(1/3)))] # 8.42 cm
  rad.d[[i]] <- rad[[i]][which(sapply(rad[[i]], function(x) x <= (750 / pi)^(1/3)))] # 6.20 cm
  rad.e[[i]] <- rad[[i]][which(sapply(rad[[i]], function(x) x <= (1875 / 19.86 / pi)^(1/3)))] # 3.11 cm
  rad.m[[i]] <- rad[[i]][which(sapply(rad[[i]], function(x) x <= (4000 * 2.54 / pi)^(1/2)))] # 56.87 cm
  rad.p[[i]] <- rad[[i]]

  dim.a[[i]] <- dim[[i]]
  dim.b[[i]] <- dim[[i]][which(sapply(dim[[i]], function(x) x <= (2 * 2.54)))] # 5.08 cm
  dim.c[[i]] <- dim[[i]][which(sapply(dim[[i]], function(x) x <= 0))] # 0 cm
  dim.d[[i]] <- dim[[i]][which(sapply(dim[[i]], function(x) x <= (0.25 * 2.54)))] # 0.635 cm
  dim.e[[i]] <- dim[[i]][which(sapply(dim[[i]], function(x) x <= (0.25 * 2.54)))] # 0.635 cm
  dim.m[[i]] <- dim[[i]][which(sapply(dim[[i]], function(x) x <= (0.3 * 2.54)))] # 0.762 cm
  dim.p[[i]] <- dim[[i]]

  ht.a[[i]] <- ht[[i]]
  ht.b[[i]] <- ht[[i]][which(sapply(ht[[i]], function(x) x <= (3000 / pi)^(1/3)))] # 9.85 cm
  ht.c[[i]] <- ht[[i]][which(sapply(ht[[i]], function(x) x <= (1875 / pi)^(1/3)))] # 8.42 cm
  ht.d[[i]] <- ht[[i]][which(sapply(ht[[i]], function(x) x <= (750 / pi)^(1/3)))] # 6.20 cm
  ht.e[[i]] <- ht[[i]][which(sapply(ht[[i]], function(x) x <= (1875 / 19.86 / pi)^(1/3)))] # 3.11 cm
  ht.m[[i]] <- ht[[i]][which(sapply(ht[[i]], function(x) x <= 2.54)] # 56.87 cm
  ht.p[[i]] <- ht[[i]]

}

mass <- rad <- dim <- ht <- list()

# mass
exp.a <- exp.b <- exp.c <- exp.d <- exp.e <- exp.m <- exp.p <- list()
pdf.a <- pdf.b <- pdf.c <- pdf.d <- pdf.e <- pdf.m <- pdf.p <- list()

for (i in 1:6) {

  rate.a <- 1 / mean(mass.a[[i]])
  rate.b <- 1 / mean(mass.b[[i]])
  rate.c <- 1 / mean(mass.c[[i]])
  rate.d <- 1 / mean(mass.d[[i]])
  rate.e <- 1 / mean(mass.e[[i]])
  rate.m <- 1 / mean(mass.m[[i]])
  rate.p <- 1 / mean(mass.p[[i]])
  # sd.p <- sd(mass.p[[i]])

  exp.a[[i]] <- dexp(seq(0, 1000, 1), rate = rate.a)
  exp.b[[i]] <- dexp(seq(0, 1000, 1), rate = rate.b)
  exp.c[[i]] <- dexp(seq(0, 1000, 1), rate = rate.c)
  exp.d[[i]] <- dexp(seq(0, 1000, 1), rate = rate.d)
  exp.e[[i]] <- dexp(seq(0, 1000, 1), rate = rate.e)
  exp.m[[i]] <- dexp(seq(0, 1000, 1), rate = rate.m)
  exp.p[[i]] <- dexp(seq(0, 1000, 1), rate = rate.p)
  # exp.p[[i]] <- dnorm(seq(0, 1000, 1), mean = mean.p, sd = sd.p)

  pdf.a[[i]] <- exp.a[[i]] / sum(exp.a[[i]])
  pdf.b[[i]] <- exp.b[[i]] / sum(exp.b[[i]])
  pdf.c[[i]] <- exp.c[[i]] / sum(exp.c[[i]])
  pdf.d[[i]] <- exp.d[[i]] / sum(exp.d[[i]])
  pdf.e[[i]] <- exp.e[[i]] / sum(exp.e[[i]])
  pdf.m[[i]] <- exp.m[[i]] / sum(exp.m[[i]])
  pdf.p[[i]] <- exp.p[[i]] / sum(exp.p[[i]])

  mass[[i]] <- list(pdf.a[[i]], pdf.b[[i]], pdf.c[[i]], pdf.d[[i]], pdf.e[[i]], pdf.m[[i]], pdf.p[[i]])

  for (j in 1:7) {
    if (i == 1 && j == 7) {
      mass[[i]][[j]] <- rep.int(0, 1001)
    } else if (i == 2) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 7) {
        mass[[i]][[j]] <- rep.int(0, 1001)
      }
    } else if (i == 3) {
      if (j == 3 || j == 4 || j == 6 || j == 7) {
        mass[[i]][[j]] <- rep.int(0, 1001)
      }
    } else if (i == 4) {
      if (j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
        mass[[i]][[j]] <- rep.int(0, 1001)
      }
    } else if (i == 5) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
        mass[[i]][[j]] <- rep.int(0, 1001)
      }
    } else if (i == 6) {
      if (j == 1 || j == 2 || j == 3 || j == 4 || j == 5 || j == 6) {
        mass[[i]][[j]] <- rep.int(0, 1001)
      }
    }
  }

}

saveRDS(mass, file = 'mass.RData')

# rad
exp.a <- exp.b <- exp.c <- exp.d <- exp.e <- exp.m <- exp.p <- list()
pdf.a <- pdf.b <- pdf.c <- pdf.d <- pdf.e <- pdf.m <- pdf.p <- list()

for (i in 1:6) {

  rate.a <- 1 / mean(rad.a[[i]])
  rate.b <- 1 / mean(rad.b[[i]])
  rate.c <- 1 / mean(rad.c[[i]])
  rate.d <- 1 / mean(rad.d[[i]])
  rate.e <- 1 / mean(rad.e[[i]])
  rate.m <- 1 / mean(rad.m[[i]])
  rate.p <- 1 / mean(rad.p[[i]])

  exp.a[[i]] <- dexp(seq(0, 1000, 1), rate = rate.a)
  exp.b[[i]] <- dexp(seq(0, 1000, 1), rate = rate.b)
  exp.c[[i]] <- dexp(seq(0, 1000, 1), rate = rate.c)
  exp.d[[i]] <- dexp(seq(0, 1000, 1), rate = rate.d)
  exp.e[[i]] <- dexp(seq(0, 1000, 1), rate = rate.e)
  exp.m[[i]] <- dexp(seq(0, 1000, 1), rate = rate.m)
  exp.p[[i]] <- dexp(seq(0, 1000, 1), rate = rate.p)

  pdf.a[[i]] <- exp.a[[i]] / sum(exp.a[[i]])
  pdf.b[[i]] <- exp.b[[i]] / sum(exp.b[[i]])
  pdf.c[[i]] <- exp.c[[i]] / sum(exp.c[[i]])
  pdf.d[[i]] <- exp.d[[i]] / sum(exp.d[[i]])
  pdf.e[[i]] <- exp.e[[i]] / sum(exp.e[[i]])
  pdf.m[[i]] <- exp.m[[i]] / sum(exp.m[[i]])
  pdf.p[[i]] <- exp.p[[i]] / sum(exp.p[[i]])

  rad[[i]] <- list(pdf.a[[i]], pdf.b[[i]], pdf.c[[i]], pdf.d[[i]], pdf.e[[i]], pdf.m[[i]], pdf.p[[i]])

  for (j in 1:7) {
    if (i == 1 && j == 7) {
      rad[[i]][[j]] <- rep.int(0, 1001)
    } else if (i == 2) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 7) {
        rad[[i]][[j]] <- rep.int(0, 1001)
      }
    } else if (i == 3) {
      if (j == 3 || j == 4 || j == 6 || j == 7) {
        rad[[i]][[j]] <- rep.int(0, 1001)
      }
    } else if (i == 4) {
      if (j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
        rad[[i]][[j]] <- rep.int(0, 1001)
      }
    } else if (i == 5) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
        rad[[i]][[j]] <- rep.int(0, 1001)
      }
    } else if (i == 6) {
      if (j == 1 || j == 2 || j == 3 || j == 4 || j == 5 || j == 6) {
        rad[[i]][[j]] <- rep.int(0, 1001)
      }
    }
  }

}

saveRDS(rad, file = 'rad.RData')

# dim
exp.a <- exp.b <- exp.c <- exp.d <- exp.e <- exp.m <- exp.p <- list()
pdf.a <- pdf.b <- pdf.c <- pdf.d <- pdf.e <- pdf.m <- pdf.p <- list()

for (i in 1:6) {

  rate.a <- 1 / mean(dim.a[[i]])
  rate.b <- 1 / mean(dim.b[[i]])
  rate.c <- 1 / mean(dim.c[[i]])
  rate.d <- 1 / mean(dim.d[[i]])
  rate.e <- 1 / mean(dim.e[[i]])
  rate.m <- 1 / mean(dim.m[[i]])
  rate.p <- 1 / mean(dim.p[[i]])

  exp.a[[i]] <- dexp(seq(0, 1000, 1), rate = rate.a)
  exp.b[[i]] <- dexp(seq(0, 1000, 1), rate = rate.b)
  exp.c[[i]] <- dexp(seq(0, 1000, 1), rate = rate.c)
  exp.d[[i]] <- dexp(seq(0, 1000, 1), rate = rate.d)
  exp.e[[i]] <- dexp(seq(0, 1000, 1), rate = rate.e)
  exp.m[[i]] <- dexp(seq(0, 1000, 1), rate = rate.m)
  exp.p[[i]] <- dexp(seq(0, 1000, 1), rate = rate.p)

  pdf.a[[i]] <- exp.a[[i]] / sum(exp.a[[i]])
  pdf.b[[i]] <- exp.b[[i]] / sum(exp.b[[i]])
  pdf.c[[i]] <- exp.c[[i]] / sum(exp.c[[i]])
  pdf.d[[i]] <- exp.d[[i]] / sum(exp.d[[i]])
  pdf.e[[i]] <- exp.e[[i]] / sum(exp.e[[i]])
  pdf.m[[i]] <- exp.m[[i]] / sum(exp.m[[i]])
  pdf.p[[i]] <- exp.p[[i]] / sum(exp.p[[i]])

  dim[[i]] <- list(pdf.a[[i]], pdf.b[[i]], pdf.c[[i]], pdf.d[[i]], pdf.e[[i]], pdf.m[[i]], pdf.p[[i]])

  for (j in 1:7) {
    if (i == 1 && j == 7) {
      dim[[i]][[j]] <- rep.int(0, 1001)
    } else if (i == 2) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 7) {
        dim[[i]][[j]] <- rep.int(0, 1001)
      }
    } else if (i == 3) {
      if (j == 3 || j == 4 || j == 6 || j == 7) {
        dim[[i]][[j]] <- rep.int(0, 1001)
      }
    } else if (i == 4) {
      if (j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
        dim[[i]][[j]] <- rep.int(0, 1001)
      }
    } else if (i == 5) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
        dim[[i]][[j]] <- rep.int(0, 1001)
      }
    } else if (i == 6) {
      if (j == 1 || j == 2 || j == 3 || j == 4 || j == 5 || j == 6) {
        dim[[i]][[j]] <- rep.int(0, 1001)
      }
    }
  }

}

saveRDS(dim, file = 'dim.RData')

# ht
exp.a <- exp.b <- exp.c <- exp.d <- exp.e <- exp.m <- exp.p <- list()
pdf.a <- pdf.b <- pdf.c <- pdf.d <- pdf.e <- pdf.m <- pdf.p <- list()

for (i in 1:6) {

  rate.a <- 1 / mean(ht.a[[i]])
  rate.b <- 1 / mean(ht.b[[i]])
  rate.c <- 1 / mean(ht.c[[i]])
  rate.d <- 1 / mean(ht.d[[i]])
  rate.e <- 1 / mean(ht.e[[i]])
  rate.m <- 1 / mean(ht.m[[i]])
  rate.p <- 1 / mean(ht.p[[i]])

  exp.a[[i]] <- dexp(seq(0, 1000, 1), rate = rate.a)
  exp.b[[i]] <- dexp(seq(0, 1000, 1), rate = rate.b)
  exp.c[[i]] <- dexp(seq(0, 1000, 1), rate = rate.c)
  exp.d[[i]] <- dexp(seq(0, 1000, 1), rate = rate.d)
  exp.e[[i]] <- dexp(seq(0, 1000, 1), rate = rate.e)
  exp.m[[i]] <- dexp(seq(0, 1000, 1), rate = rate.m)
  exp.p[[i]] <- dexp(seq(0, 1000, 1), rate = rate.p)

  pdf.a[[i]] <- exp.a[[i]] / sum(exp.a[[i]])
  pdf.b[[i]] <- exp.b[[i]] / sum(exp.b[[i]])
  pdf.c[[i]] <- exp.c[[i]] / sum(exp.c[[i]])
  pdf.d[[i]] <- exp.d[[i]] / sum(exp.d[[i]])
  pdf.e[[i]] <- exp.e[[i]] / sum(exp.e[[i]])
  pdf.m[[i]] <- exp.m[[i]] / sum(exp.m[[i]])
  pdf.p[[i]] <- exp.p[[i]] / sum(exp.p[[i]])

  ht[[i]] <- list(pdf.a[[i]], pdf.b[[i]], pdf.c[[i]], pdf.d[[i]], pdf.e[[i]], pdf.m[[i]], pdf.p[[i]])

  for (j in 1:7) {
    if (i == 1 && j == 7) {
      ht[[i]][[j]] <- rep.int(0, 1001)
    } else if (i == 2) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 7) {
        ht[[i]][[j]] <- rep.int(0, 1001)
      }
    } else if (i == 3) {
      if (j == 3 || j == 4 || j == 6 || j == 7) {
        ht[[i]][[j]] <- rep.int(0, 1001)
      }
    } else if (i == 4) {
      if (j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
        ht[[i]][[j]] <- rep.int(0, 1001)
      }
    } else if (i == 5) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
        ht[[i]][[j]] <- rep.int(0, 1001)
      }
    } else if (i == 6) {
      if (j == 1 || j == 2 || j == 3 || j == 4 || j == 5 || j == 6) {
        ht[[i]][[j]] <- rep.int(0, 1001)
      }
    }
  }

}

saveRDS(ht, file = 'ht.RData')
