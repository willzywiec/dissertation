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

# set working directory
setwd('E:/dist')

# 1 = large sample
# 2 = machining
# 3 = metallurgy
# 4 = small sample
# 5 = solution
# 6 = waste

dist.type <- 'normal'

# mass.1 <- c(rnorm(100, mean = 45.31, sd = 84.63), rexp(100, rate = 1/45.31))
# mass.1 <- mass.1[which(sapply(mass.1, function(x) x >= 0))]
# mass.2 <- c(rnorm(100, mean = 8.64, sd = 18.76), rexp(100, rate = 1/8.64))
# mass.2 <- mass.2[which(sapply(mass.2, function(x) x >= 0))]
# mass.3 <- c(rnorm(100, mean = 3.3, sd = 9.85), rexp(100, rate = 1/3.3))
# mass.3 <- mass.3[which(sapply(mass.3, function(x) x >= 0))]
# mass.4 <- c(rnorm(100, mean = 15.71, sd = 29.36), rexp(100, rate = 1/15.71))
# mass.4 <- mass.4[which(sapply(mass.4, function(x) x >= 0))]
# mass.5 <- c(rnorm(100, mean = 5.33, sd = 9.09), rexp(100, rate = 1/5.33))
# mass.5 <- mass.5[which(sapply(mass.5, function(x) x >= 0))]
# mass.6 <- c(rnorm(100, mean = 84.5, sd = 43.13), rexp(100, rate = 1/84.5))
# mass.6 <- mass.6[which(sapply(mass.6, function(x) x >= 0))]

mass.1 <- c(rnorm(100, mean = 62.5, sd = 74.9), rexp(100, rate = 1/62.5))
mass.1 <- mass.1[which(sapply(mass.1, function(x) x >= 0))]
mass.2 <- c(rnorm(100, mean = 28.9, sd = 42.6), rexp(100, rate = 1/28.9))
mass.2 <- mass.2[which(sapply(mass.2, function(x) x >= 0))]
mass.3 <- c(rnorm(100, mean = 49.7, sd = 62.6), rexp(100, rate = 1/49.7))
mass.3 <- mass.3[which(sapply(mass.3, function(x) x >= 0))]
mass.4 <- c(rnorm(100, mean = 59.9, sd = 54.1), rexp(100, rate = 1/59.9))
mass.4 <- mass.4[which(sapply(mass.4, function(x) x >= 0))]
mass.5 <- c(rnorm(100, mean = 8.9, sd = 12.6), rexp(100, rate = 1/8.9))
mass.5 <- mass.5[which(sapply(mass.5, function(x) x >= 0))]
mass.6 <- c(rnorm(100, mean = 84.5, sd = 43.2), rexp(100, rate = 1/84.5))
mass.6 <- mass.6[which(sapply(mass.6, function(x) x >= 0))]

mass <- list(mass.1, mass.2, mass.3, mass.4, mass.5, mass.6)
mass.a <- mass.b <- mass.c <- mass.d <- mass.e <- mass.m <- mass.p <- list()

rad.1 <- c(rnorm(100, mean = 7.62, sd = 8.09), rexp(100, rate = 1/7.62))
rad.1 <- rad.1[which(sapply(rad.1, function(x) x >= 0))]
rad.2 <- c(rnorm(100, mean = 7.62, sd = 8.67), rexp(100, rate = 1/7.62))
rad.2 <- rad.2[which(sapply(rad.2, function(x) x >= 0))]
rad.3 <- c(rnorm(100, mean = 7.62, sd = 8.23), rexp(100, rate = 1/7.62))
rad.3 <- rad.3[which(sapply(rad.3, function(x) x >= 0))]
rad.4 <- c(rnorm(100, mean = 7.62, sd = 7.37), rexp(100, rate = 1/7.62))
rad.4 <- rad.4[which(sapply(rad.4, function(x) x >= 0))]
rad.5 <- c(rnorm(100, mean = 7.62, sd = 8.56), rexp(100, rate = 1/7.62))
rad.5 <- rad.5[which(sapply(rad.5, function(x) x >= 0))]
rad.6 <- c(rnorm(100, mean = 7.62, sd = 6.09), rexp(100, rate = 1/7.62))
rad.6 <- rad.6[which(sapply(rad.6, function(x) x >= 0))]

rad <- list(rad.1, rad.2, rad.3, rad.4, rad.5, rad.6)
rad.a <- rad.b <- rad.c <- rad.d <- rad.e <- rad.m <- rad.p <- list()

dim.1 <- c(rnorm(100, mean = 0.635, sd = 1.48), rexp(100, rate = 1/0.635))
dim.1 <- dim.1[which(sapply(dim.1, function(x) x >= 0))]
dim.2 <- dim.3 <- dim.4 <- dim.5 <- dim.6 <- dim.1

dim <- list(dim.1, dim.2, dim.3, dim.4, dim.5, dim.6)
dim.a <- dim.b <- dim.c <- dim.d <- dim.e <- dim.m <- dim.p <- list()

ht.1 <- c(rnorm(100, mean = 11.27, sd = 11.97), rexp(100, rate = 1/11.27))
ht.1 <- ht.1[which(sapply(ht.1, function(x) x >= 0))]
ht.2 <- c(rnorm(100, mean = 11.27, sd = 12.83), rexp(100, rate = 1/11.27))
ht.2 <- ht.2[which(sapply(ht.2, function(x) x >= 0))]
ht.3 <- c(rnorm(100, mean = 11.27, sd = 12.17), rexp(100, rate = 1/11.27))
ht.3 <- ht.3[which(sapply(ht.3, function(x) x >= 0))]
ht.4 <- c(rnorm(100, mean = 11.27, sd = 10.89), rexp(100, rate = 1/11.27))
ht.4 <- ht.4[which(sapply(ht.4, function(x) x >= 0))]
ht.5 <- c(rnorm(100, mean = 11.27, sd = 12.65), rexp(100, rate = 1/11.27))
ht.5 <- ht.5[which(sapply(ht.5, function(x) x >= 0))]
ht.6 <- c(rnorm(100, mean = 11.27, sd = 9.01), rexp(100, rate = 1/11.27))
ht.6 <- ht.6[which(sapply(ht.6, function(x) x >= 0))]

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
  dim.c[[i]] <- rep(0, 200) # 0 cm
  dim.d[[i]] <- dim[[i]][which(sapply(dim[[i]], function(x) x <= (0.25 * 2.54)))] # 0.635 cm
  dim.e[[i]] <- dim[[i]][which(sapply(dim[[i]], function(x) x <= (0.25 * 2.54)))] # 0.635 cm
  dim.m[[i]] <- dim[[i]][which(sapply(dim[[i]], function(x) x <= (0.3 * 2.54)))] # 0.762 cm
  dim.p[[i]] <- dim[[i]]

  ht.a[[i]] <- ht[[i]]
  ht.b[[i]] <- ht[[i]][which(sapply(ht[[i]], function(x) x <= (3000 / pi)^(1/3)))] # 9.85 cm
  ht.c[[i]] <- ht[[i]][which(sapply(ht[[i]], function(x) x <= (1875 / pi)^(1/3)))] # 8.42 cm
  ht.d[[i]] <- ht[[i]][which(sapply(ht[[i]], function(x) x <= (750 / pi)^(1/3)))] # 6.20 cm
  ht.e[[i]] <- ht[[i]][which(sapply(ht[[i]], function(x) x <= (1875 / 19.86 / pi)^(1/3)))] # 3.11 cm
  ht.m[[i]] <- ht[[i]][which(sapply(ht[[i]], function(x) x <= 2.54))] # 56.87 cm
  ht.p[[i]] <- ht[[i]]

}

write.csv(mass.a[[1]], file = 'mass-a-1.csv', row.names = FALSE)
write.csv(mass.a[[2]], file = 'mass-a-2.csv', row.names = FALSE)
write.csv(mass.a[[3]], file = 'mass-a-3.csv', row.names = FALSE)
write.csv(mass.a[[4]], file = 'mass-a-4.csv', row.names = FALSE)
write.csv(mass.a[[5]], file = 'mass-a-5.csv', row.names = FALSE)
write.csv(mass.a[[6]], file = 'mass-a-6.csv', row.names = FALSE)

mass <- rad <- dim <- ht <- list()

# mass
dist.a <- dist.b <- dist.c <- dist.d <- dist.e <- dist.m <- dist.p <- list()
pdf.a <- pdf.b <- pdf.c <- pdf.d <- pdf.e <- pdf.m <- pdf.p <- list()

for (i in 1:6) {

  if (dist.type == 'exponential') {
    dist.a[[i]] <- dexp(seq(0, 4000, 1), rate = 1 / mean(mass.a[[i]]))
    dist.b[[i]] <- dexp(seq(0, 4000, 1), rate = 1 / mean(mass.b[[i]]))
    dist.c[[i]] <- dexp(seq(0, 4000, 1), rate = 1 / mean(mass.c[[i]]))
    dist.d[[i]] <- dexp(seq(0, 4000, 1), rate = 1 / mean(mass.d[[i]]))
    dist.e[[i]] <- dexp(seq(0, 4000, 1), rate = 1 / mean(mass.e[[i]]))
    dist.m[[i]] <- dexp(seq(0, 4000, 1), rate = 1 / mean(mass.m[[i]]))
    dist.p[[i]] <- dexp(seq(0, 4000, 1), rate = 1 / mean(mass.p[[i]]))
  } else if (dist.type == 'gamma') {
    dist.a[[i]] <- dgamma(seq(0, 4000, 1), rate = 1 / mean(mass.a[[i]]))
    dist.b[[i]] <- dgamma(seq(0, 4000, 1), rate = 1 / mean(mass.b[[i]]))
    dist.c[[i]] <- dgamma(seq(0, 4000, 1), rate = 1 / mean(mass.c[[i]]))
    dist.d[[i]] <- dgamma(seq(0, 4000, 1), rate = 1 / mean(mass.d[[i]]))
    dist.e[[i]] <- dgamma(seq(0, 4000, 1), rate = 1 / mean(mass.e[[i]]))
    dist.m[[i]] <- dgamma(seq(0, 4000, 1), rate = 1 / mean(mass.m[[i]]))
    dist.p[[i]] <- dgamma(seq(0, 4000, 1), rate = 1 / mean(mass.p[[i]]))
  } else if (dist.type == 'normal') {
    dist.a[[i]] <- dnorm(seq(0, 4000, 1), mean = mean(mass.a[[i]]), sd = sd(mass.a[[i]]))
    dist.b[[i]] <- dnorm(seq(0, 4000, 1), mean = mean(mass.b[[i]]), sd = sd(mass.b[[i]]))
    dist.c[[i]] <- dnorm(seq(0, 4000, 1), mean = mean(mass.c[[i]]), sd = sd(mass.c[[i]]))
    dist.d[[i]] <- dnorm(seq(0, 4000, 1), mean = mean(mass.d[[i]]), sd = sd(mass.d[[i]]))
    dist.e[[i]] <- dnorm(seq(0, 4000, 1), mean = mean(mass.e[[i]]), sd = sd(mass.e[[i]]))
    dist.m[[i]] <- dnorm(seq(0, 4000, 1), mean = mean(mass.m[[i]]), sd = sd(mass.m[[i]]))
    dist.p[[i]] <- dnorm(seq(0, 4000, 1), mean = mean(mass.p[[i]]), sd = sd(mass.p[[i]]))
  }

  # normalize probability density functions
  pdf.a[[i]] <- dist.a[[i]] / sum(dist.a[[i]])
  pdf.b[[i]] <- dist.b[[i]] / sum(dist.b[[i]])
  pdf.c[[i]] <- dist.c[[i]] / sum(dist.c[[i]])
  pdf.d[[i]] <- dist.d[[i]] / sum(dist.d[[i]])
  pdf.e[[i]] <- dist.e[[i]] / sum(dist.e[[i]])
  pdf.m[[i]] <- dist.m[[i]] / sum(dist.m[[i]])
  pdf.p[[i]] <- dist.p[[i]] / sum(dist.p[[i]])

  mass[[i]] <- list(pdf.a[[i]], pdf.b[[i]], pdf.c[[i]], pdf.d[[i]], pdf.e[[i]], pdf.m[[i]], pdf.p[[i]])

  for (j in 1:7) {
    if (i == 1 && j == 7) {
      mass[[i]][[j]] <- c(1, rep.int(0, 4000))
    } else if (i == 2) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 7) {
        mass[[i]][[j]] <- c(1, rep.int(0, 4000))
      }
    } else if (i == 3) {
      if (j == 3 || j == 4 || j == 6 || j == 7) {
        mass[[i]][[j]] <- c(1, rep.int(0, 4000))
      }
    } else if (i == 4) {
      if (j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
        mass[[i]][[j]] <- c(1, rep.int(0, 4000))
      }
    } else if (i == 5) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
        mass[[i]][[j]] <- c(1, rep.int(0, 4000))
      }
    } else if (i == 6) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 6) {
        mass[[i]][[j]] <- c(1, rep.int(0, 4000))
      }
    }
  }

}

saveRDS(mass, file = 'mass.RData')

# rad
dist.a <- dist.b <- dist.c <- dist.d <- dist.e <- dist.m <- dist.p <- list()
pdf.a <- pdf.b <- pdf.c <- pdf.d <- pdf.e <- pdf.m <- pdf.p <- list()

for (i in 1:6) {

  if (dist.type == 'exponential') {
    dist.a[[i]] <- dexp(seq(0, 45.72, 0.635), rate = 1 / mean(rad.a[[i]]))
    dist.b[[i]] <- dexp(seq(0, 45.72, 0.635), rate = 1 / mean(rad.b[[i]]))
    dist.c[[i]] <- dexp(seq(0, 45.72, 0.635), rate = 1 / mean(rad.c[[i]]))
    dist.d[[i]] <- dexp(seq(0, 45.72, 0.635), rate = 1 / mean(rad.d[[i]]))
    dist.e[[i]] <- dexp(seq(0, 45.72, 0.635), rate = 1 / mean(rad.e[[i]]))
    dist.m[[i]] <- dexp(seq(0, 45.72, 0.635), rate = 1 / mean(rad.m[[i]]))
    dist.p[[i]] <- dexp(seq(0, 45.72, 0.635), rate = 1 / mean(rad.p[[i]]))
  } else if (dist.type == 'gamma') {
    dist.a[[i]] <- dgamma(seq(0, 45.72, 0.635), rate = 1 / mean(rad.a[[i]]))
    dist.b[[i]] <- dgamma(seq(0, 45.72, 0.635), rate = 1 / mean(rad.b[[i]]))
    dist.c[[i]] <- dgamma(seq(0, 45.72, 0.635), rate = 1 / mean(rad.c[[i]]))
    dist.d[[i]] <- dgamma(seq(0, 45.72, 0.635), rate = 1 / mean(rad.d[[i]]))
    dist.e[[i]] <- dgamma(seq(0, 45.72, 0.635), rate = 1 / mean(rad.e[[i]]))
    dist.m[[i]] <- dgamma(seq(0, 45.72, 0.635), rate = 1 / mean(rad.m[[i]]))
    dist.p[[i]] <- dgamma(seq(0, 45.72, 0.635), rate = 1 / mean(rad.p[[i]]))
  } else if (dist.type == 'normal') {
    dist.a[[i]] <- dnorm(seq(0, 45.72, 0.635), mean = mean(rad.a[[i]]), sd = sd(rad.a[[i]]))
    dist.b[[i]] <- dnorm(seq(0, 45.72, 0.635), mean = mean(rad.b[[i]]), sd = sd(rad.b[[i]]))
    dist.c[[i]] <- dnorm(seq(0, 45.72, 0.635), mean = mean(rad.c[[i]]), sd = sd(rad.c[[i]]))
    dist.d[[i]] <- dnorm(seq(0, 45.72, 0.635), mean = mean(rad.d[[i]]), sd = sd(rad.d[[i]]))
    dist.e[[i]] <- dnorm(seq(0, 45.72, 0.635), mean = mean(rad.e[[i]]), sd = sd(rad.e[[i]]))
    dist.m[[i]] <- dnorm(seq(0, 45.72, 0.635), mean = mean(rad.m[[i]]), sd = sd(rad.m[[i]]))
    dist.p[[i]] <- dnorm(seq(0, 45.72, 0.635), mean = mean(rad.p[[i]]), sd = sd(rad.p[[i]]))
  }

  pdf.a[[i]] <- dist.a[[i]] / sum(dist.a[[i]])
  pdf.b[[i]] <- dist.b[[i]] / sum(dist.b[[i]])
  pdf.c[[i]] <- dist.c[[i]] / sum(dist.c[[i]])
  pdf.d[[i]] <- dist.d[[i]] / sum(dist.d[[i]])
  pdf.e[[i]] <- dist.e[[i]] / sum(dist.e[[i]])
  pdf.m[[i]] <- dist.m[[i]] / sum(dist.m[[i]])
  pdf.p[[i]] <- dist.p[[i]] / sum(dist.p[[i]])

  rad[[i]] <- list(pdf.a[[i]], pdf.b[[i]], pdf.c[[i]], pdf.d[[i]], pdf.e[[i]], pdf.m[[i]], pdf.p[[i]])

  for (j in 1:7) {
    if (i == 1 && j == 7) {
      rad[[i]][[j]] <- c(1, rep.int(0, 72))
    } else if (i == 2) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 7) {
        rad[[i]][[j]] <- c(1, rep.int(0, 72))
      }
    } else if (i == 3) {
      if (j == 3 || j == 4 || j == 6 || j == 7) {
        rad[[i]][[j]] <- c(1, rep.int(0, 72))
      }
    } else if (i == 4) {
      if (j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
        rad[[i]][[j]] <- c(1, rep.int(0, 72))
      }
    } else if (i == 5) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
        rad[[i]][[j]] <- c(1, rep.int(0, 72))
      }
    } else if (i == 6) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 6) {
        rad[[i]][[j]] <- c(1, rep.int(0, 72))
      }
    }
  }

}

saveRDS(rad, file = 'rad.RData')

# dim
dist.a <- dist.b <- dist.c <- dist.d <- dist.e <- dist.m <- dist.p <- list()
pdf.a <- pdf.b <- pdf.c <- pdf.d <- pdf.e <- pdf.m <- pdf.p <- list()

for (i in 1:6) {

  if (dist.type == 'exponential') {
    dist.a[[i]] <- dexp(seq(0, 45.72, 0.635), rate = 1 / mean(dim.a[[i]]))
    dist.b[[i]] <- dexp(seq(0, 45.72, 0.635), rate = 1 / mean(dim.b[[i]]))
    dist.c[[i]] <- dexp(seq(0, 45.72, 0.635), rate = 1 / mean(dim.c[[i]]))
    dist.d[[i]] <- dexp(seq(0, 45.72, 0.635), rate = 1 / mean(dim.d[[i]]))
    dist.e[[i]] <- dexp(seq(0, 45.72, 0.635), rate = 1 / mean(dim.e[[i]]))
    dist.m[[i]] <- dexp(seq(0, 45.72, 0.635), rate = 1 / mean(dim.m[[i]]))
    dist.p[[i]] <- dexp(seq(0, 45.72, 0.635), rate = 1 / mean(dim.p[[i]]))
  } else if (dist.type == 'gamma') {
    dist.a[[i]] <- dgamma(seq(0, 45.72, 0.635), rate = 1 / mean(dim.a[[i]]))
    dist.b[[i]] <- dgamma(seq(0, 45.72, 0.635), rate = 1 / mean(dim.b[[i]]))
    dist.c[[i]] <- dgamma(seq(0, 45.72, 0.635), rate = 1 / mean(dim.c[[i]]))
    dist.d[[i]] <- dgamma(seq(0, 45.72, 0.635), rate = 1 / mean(dim.d[[i]]))
    dist.e[[i]] <- dgamma(seq(0, 45.72, 0.635), rate = 1 / mean(dim.e[[i]]))
    dist.m[[i]] <- dgamma(seq(0, 45.72, 0.635), rate = 1 / mean(dim.m[[i]]))
    dist.p[[i]] <- dgamma(seq(0, 45.72, 0.635), rate = 1 / mean(dim.p[[i]]))
  } else if (dist.type == 'normal') {
    dist.a[[i]] <- dnorm(seq(0, 45.72, 0.635), mean = mean(dim.a[[i]]), sd = sd(dim.a[[i]]))
    dist.b[[i]] <- dnorm(seq(0, 45.72, 0.635), mean = mean(dim.b[[i]]), sd = sd(dim.b[[i]]))
    dist.c[[i]] <- dnorm(seq(0, 45.72, 0.635), mean = mean(dim.c[[i]]), sd = 0.01) # sd = 0 doen't work
    dist.d[[i]] <- dnorm(seq(0, 45.72, 0.635), mean = mean(dim.d[[i]]), sd = sd(dim.d[[i]]))
    dist.e[[i]] <- dnorm(seq(0, 45.72, 0.635), mean = mean(dim.e[[i]]), sd = sd(dim.e[[i]]))
    dist.m[[i]] <- dnorm(seq(0, 45.72, 0.635), mean = mean(dim.m[[i]]), sd = sd(dim.m[[i]]))
    dist.p[[i]] <- dnorm(seq(0, 45.72, 0.635), mean = mean(dim.p[[i]]), sd = sd(dim.p[[i]]))
  }
  
  pdf.a[[i]] <- dist.a[[i]] / sum(dist.a[[i]])
  pdf.b[[i]] <- dist.b[[i]] / sum(dist.b[[i]])
  pdf.c[[i]] <- dist.c[[i]] / sum(dist.c[[i]])
  pdf.d[[i]] <- dist.d[[i]] / sum(dist.d[[i]])
  pdf.e[[i]] <- dist.e[[i]] / sum(dist.e[[i]])
  pdf.m[[i]] <- dist.m[[i]] / sum(dist.m[[i]])
  pdf.p[[i]] <- dist.p[[i]] / sum(dist.p[[i]])

  dim[[i]] <- list(pdf.a[[i]], pdf.b[[i]], pdf.c[[i]], pdf.d[[i]], pdf.e[[i]], pdf.m[[i]], pdf.p[[i]])

  for (j in 1:7) {
    if (i == 1 && j == 7) {
      dim[[i]][[j]] <- c(1, rep.int(0, 72))
    } else if (i == 2) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 7) {
        dim[[i]][[j]] <- c(1, rep.int(0, 72))
      }
    } else if (i == 3) {
      if (j == 3 || j == 4 || j == 6 || j == 7) {
        dim[[i]][[j]] <- c(1, rep.int(0, 72))
      }
    } else if (i == 4) {
      if (j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
        dim[[i]][[j]] <- c(1, rep.int(0, 72))
      }
    } else if (i == 5) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
        dim[[i]][[j]] <- c(1, rep.int(0, 72))
      }
    } else if (i == 6) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 6) {
        dim[[i]][[j]] <- c(1, rep.int(0, 72))
      }
    }
  }

}

saveRDS(dim, file = 'dim.RData')

# ht
dist.a <- dist.b <- dist.c <- dist.d <- dist.e <- dist.m <- dist.p <- list()
pdf.a <- pdf.b <- pdf.c <- pdf.d <- pdf.e <- pdf.m <- pdf.p <- list()

for (i in 1:6) {

  if (dist.type == 'exponential') {
    dist.a[[i]] <- dexp(seq(0, 91.44, 0.635), rate = 1 / mean(ht.a[[i]]))
    dist.b[[i]] <- dexp(seq(0, 91.44, 0.635), rate = 1 / mean(ht.b[[i]]))
    dist.c[[i]] <- dexp(seq(0, 91.44, 0.635), rate = 1 / mean(ht.c[[i]]))
    dist.d[[i]] <- dexp(seq(0, 91.44, 0.635), rate = 1 / mean(ht.d[[i]]))
    dist.e[[i]] <- dexp(seq(0, 91.44, 0.635), rate = 1 / mean(ht.e[[i]]))
    dist.m[[i]] <- dexp(seq(0, 91.44, 0.635), rate = 1 / mean(ht.m[[i]]))
    dist.p[[i]] <- dexp(seq(0, 91.44, 0.635), rate = 1 / mean(ht.p[[i]]))
  } else if (dist.type == 'gamma') {
    dist.a[[i]] <- dgamma(seq(0, 91.44, 0.635), rate = 1 / mean(ht.a[[i]]))
    dist.b[[i]] <- dgamma(seq(0, 91.44, 0.635), rate = 1 / mean(ht.b[[i]]))
    dist.c[[i]] <- dgamma(seq(0, 91.44, 0.635), rate = 1 / mean(ht.c[[i]]))
    dist.d[[i]] <- dgamma(seq(0, 91.44, 0.635), rate = 1 / mean(ht.d[[i]]))
    dist.e[[i]] <- dgamma(seq(0, 91.44, 0.635), rate = 1 / mean(ht.e[[i]]))
    dist.m[[i]] <- dgamma(seq(0, 91.44, 0.635), rate = 1 / mean(ht.m[[i]]))
    dist.p[[i]] <- dgamma(seq(0, 91.44, 0.635), rate = 1 / mean(ht.p[[i]]))
  } else if (dist.type == 'normal') {
    dist.a[[i]] <- dnorm(seq(0, 91.44, 0.635), mean = mean(ht.a[[i]]), sd = sd(ht.a[[i]]))
    dist.b[[i]] <- dnorm(seq(0, 91.44, 0.635), mean = mean(ht.b[[i]]), sd = sd(ht.b[[i]]))
    dist.c[[i]] <- dnorm(seq(0, 91.44, 0.635), mean = mean(ht.c[[i]]), sd = sd(ht.c[[i]]))
    dist.d[[i]] <- dnorm(seq(0, 91.44, 0.635), mean = mean(ht.d[[i]]), sd = sd(ht.d[[i]]))
    dist.e[[i]] <- dnorm(seq(0, 91.44, 0.635), mean = mean(ht.e[[i]]), sd = sd(ht.e[[i]]))
    dist.m[[i]] <- dnorm(seq(0, 91.44, 0.635), mean = mean(ht.m[[i]]), sd = sd(ht.m[[i]]))
    dist.p[[i]] <- dnorm(seq(0, 91.44, 0.635), mean = mean(ht.p[[i]]), sd = sd(ht.p[[i]]))
  }
  
  pdf.a[[i]] <- dist.a[[i]] / sum(dist.a[[i]])
  pdf.b[[i]] <- dist.b[[i]] / sum(dist.b[[i]])
  pdf.c[[i]] <- dist.c[[i]] / sum(dist.c[[i]])
  pdf.d[[i]] <- dist.d[[i]] / sum(dist.d[[i]])
  pdf.e[[i]] <- dist.e[[i]] / sum(dist.e[[i]])
  pdf.m[[i]] <- dist.m[[i]] / sum(dist.m[[i]])
  pdf.p[[i]] <- dist.p[[i]] / sum(dist.p[[i]])

  ht[[i]] <- list(pdf.a[[i]], pdf.b[[i]], pdf.c[[i]], pdf.d[[i]], pdf.e[[i]], pdf.m[[i]], pdf.p[[i]])

  for (j in 1:7) {
    if (i == 1 && j == 7) {
      ht[[i]][[j]] <- c(1, rep.int(0, 144))
    } else if (i == 2) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 7) {
        ht[[i]][[j]] <- c(1, rep.int(0, 144))
      }
    } else if (i == 3) {
      if (j == 3 || j == 4 || j == 6 || j == 7) {
        ht[[i]][[j]] <- c(1, rep.int(0, 144))
      }
    } else if (i == 4) {
      if (j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
        ht[[i]][[j]] <- c(1, rep.int(0, 144))
      }
    } else if (i == 5) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
        ht[[i]][[j]] <- c(1, rep.int(0, 144))
      }
    } else if (i == 6) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 6) {
        ht[[i]][[j]] <- c(1, rep.int(0, 144))
      }
    }
  }

}

saveRDS(ht, file = 'ht.RData')
