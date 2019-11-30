# dist.R
#
# William John Zywiec
# The George Washington University
#
# ...

# initialize environment
# if (!is.null(dev.list())) dev.off()
# rm(list = ls())
# cat('\014')

# load packages
library(EnvStats)
library(fitdistrplus)

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

# set probability distribution
dist.type <- 'gamma'

if (file.exists('mass.csv') && file.exists('rad.csv') && file.exists('dim.csv') && file.exists('ht.csv')) {
  
  mass <- as.list(read.csv('mass.csv'))
  rad <- as.list(read.csv('rad.csv'))
  dim <- as.list(read.csv('dim.csv'))
  ht <- as.list(read.csv('ht.csv'))
  
} else {

  mass.1 <- rnorm(500, mean = 62.5, sd = 74.9)
  mass.2 <- rnorm(500, mean = 28.9, sd = 42.6)
  mass.3 <- rnorm(500, mean = 49.7, sd = 62.6)
  mass.4 <- rnorm(500, mean = 59.9, sd = 54.1)
  mass.5 <- rnorm(500, mean = 8.9,  sd = 12.6)
  mass.6 <- rnorm(500, mean = 84.5, sd = 43.2)

  mass <- list(mass.1, mass.2, mass.3, mass.4, mass.5, mass.6)
  write.csv(mass, file = 'mass.csv', row.names = FALSE)

  rad.1 <- rnorm(500, mean = 7.62, sd = 8.09)
  rad.2 <- rnorm(500, mean = 7.62, sd = 8.67)
  rad.3 <- rnorm(500, mean = 7.62, sd = 8.23)
  rad.4 <- rnorm(500, mean = 7.62, sd = 7.37)
  rad.5 <- rnorm(500, mean = 7.62, sd = 8.56)
  rad.6 <- rnorm(500, mean = 7.62, sd = 6.09)

  rad <- list(rad.1, rad.2, rad.3, rad.4, rad.5, rad.6)
  write.csv(rad, file = 'rad.csv', row.names = FALSE)

  dim.1 <- rnorm(500, mean = 0.635, sd = 1.48)
  dim.2 <- rnorm(500, mean = 0.635, sd = 1.48)
  dim.3 <- rnorm(500, mean = 0.635, sd = 1.48)
  dim.4 <- rnorm(500, mean = 0.635, sd = 1.48)
  dim.5 <- rnorm(500, mean = 0.635, sd = 1.48)
  dim.6 <- rnorm(500, mean = 0.635, sd = 1.48)

  dim <- list(dim.1, dim.2, dim.3, dim.4, dim.5, dim.6)
  write.csv(dim, file = 'dim.csv', row.names = FALSE)

  ht.1 <- rnorm(500, mean = 11.27, sd = 11.97)
  ht.2 <- rnorm(500, mean = 11.27, sd = 12.83)
  ht.3 <- rnorm(500, mean = 11.27, sd = 12.17)
  ht.4 <- rnorm(500, mean = 11.27, sd = 10.89)
  ht.5 <- rnorm(500, mean = 11.27, sd = 12.65)
  ht.6 <- rnorm(500, mean = 11.27, sd = 9.01)

  ht <- list(ht.1, ht.2, ht.3, ht.4, ht.5, ht.6)
  write.csv(ht, file = 'ht.csv', row.names = FALSE)
  
}

for (i in 1:6) {
  mass[[i]] <- mass[[i]][which(sapply(mass[[i]], function(x) x >= 0))]
  rad[[i]] <- rad[[i]][which(sapply(rad[[i]], function(x) x >= 0))]
  dim[[i]] <- dim[[i]][which(sapply(dim[[i]], function(x) x >= 0))]
  ht[[i]] <- ht[[i]][which(sapply(ht[[i]], function(x) x >= 0))]
}

mass.a <- mass.b <- mass.c <- mass.d <- mass.e <- mass.m <- mass.p <- list()
rad.a <- rad.b <- rad.c <- rad.d <- rad.e <- rad.m <- rad.p <- list()
dim.a <- dim.b <- dim.c <- dim.d <- dim.e <- dim.m <- dim.p <- list()
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
  dim.c[[i]] <- dim[[i]][which(sapply(dim[[i]], function(x) x <= 0.1))] # 0.1 cm
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

for (i in 1:length(mass.a)) {
  write.csv(mass.a[[i]], file = paste0('mass-a-', i, '.csv'), row.names = FALSE)
}

mass.x <- list(mass.a, mass.b, mass.c, mass.d, mass.e, mass.m, mass.p)
rad.x <- list(rad.a, rad.b, rad.c, rad.d, rad.e, rad.m, rad.p)
dim.x <- list(dim.a, dim.b, dim.c, dim.d, dim.e, dim.m, dim.p)
ht.x <- list(ht.a, ht.b, ht.c, ht.d, ht.e, ht.m, ht.p)

mass <- rad <- dim <- ht <- list()

for (i in 1:6) {

  mass.fit <- rad.fit <- dim.fit <- ht.fit <- list()
  mass.dist <- rad.dist <- dim.dist <- ht.dist <- list()
  mass.pdf <- rad.pdf <- dim.pdf <- ht.pdf <- list()
  mass.gof <- rad.gof <- dim.gof <- ht.gof <- list()

  for (j in 1:7) {
    
    if (dist.type == 'gamma') {

      mass.fit[[j]] <- fitdist(mass.x[[j]][[i]], distr = 'gamma', method = 'mle')
      mass.dist[[j]] <- dgamma(seq(0, 4000, 1), rate = mass.fit[[j]]$estimate[[2]], shape = mass.fit[[j]]$estimate[[1]])

      rad.fit[[j]] <- fitdist(rad.x[[j]][[i]], distr = 'gamma', method = 'mle')
      rad.dist[[j]] <- dgamma(seq(0, 45.72, 0.635), rate = rad.fit[[j]]$estimate[[2]], shape = rad.fit[[j]]$estimate[[1]])

      dim.fit[[j]] <- fitdist(dim.x[[j]][[i]], distr = 'gamma', method = 'mle')
      # dim.dist[[j]] <- dgamma(seq(0, 45.72, 0.635), rate = dim.fit[[j]]$estimate[[2]], shape = dim.fit[[j]]$estimate[[1]])
      dim.dist[[j]] <- dgamma(seq(0, 5.08, 0.635), rate = dim.fit[[j]]$estimate[[2]], shape = dim.fit[[j]]$estimate[[1]])

      ht.fit[[j]] <- fitdist(ht.x[[j]][[i]], distr = 'gamma', method = 'mle')
      ht.dist[[j]] <- dgamma(seq(0, 91.44, 0.635), rate = ht.fit[[j]]$estimate[[2]], shape = ht.fit[[j]]$estimate[[1]])

    } else if (dist.type == 'gev') {

      mass.fit[[j]] <- fgev(mass.x[[j]][[i]], method = 'BFGS', std.err = FALSE)
      mass.dist[[j]] <- dgev(seq(0, 4000, 1), loc = mass.fit[[j]]$estimate[[1]], scale = mass.fit[[j]]$estimate[[2]], shape = mass.fit[[j]]$estimate[[3]])
      
      rad.fit[[j]] <- fgev(rad.x[[j]][[i]], method = 'BFGS', std.err = FALSE)
      rad.dist[[j]] <- dgev(seq(0, 45.72, 0.635), loc = rad.fit[[j]]$estimate[[1]], scale = rad.fit[[j]]$estimate[[2]], shape = rad.fit[[j]]$estimate[[3]])
      
      dim.fit[[j]] <- fgev(dim.x[[j]][[i]], method = 'BFGS', std.err = FALSE)
      # dim.dist[[j]] <- dgev(seq(0, 45.72, 0.635), loc = dim.fit[[j]]$estimate[[1]], scale = dim.fit[[j]]$estimate[[2]], shape = dim.fit[[j]]$estimate[[3]])
      dim.dist[[j]] <- dgev(seq(0, 5.08, 0.635), loc = dim.fit[[j]]$estimate[[1]], scale = dim.fit[[j]]$estimate[[2]], shape = dim.fit[[j]]$estimate[[3]])
      
      ht.fit[[j]] <- fgev(ht.x[[j]][[i]], method = 'BFGS', std.err = FALSE)
      ht.dist[[j]] <- dgev(seq(0, 91.44, 0.635), loc = ht.fit[[j]]$estimate[[1]], scale = ht.fit[[j]]$estimate[[2]], shape = ht.fit[[j]]$estimate[[3]])
      
    } else if (dist.type == 'log-normal') {
      
      mass.fit[[j]] <- fitdist(mass.x[[j]][[i]], distr = 'lnorm', method = 'mle')
      mass.dist[[j]] <- dlnorm(seq(0, 4000, 1), meanlog = mass.fit[[j]]$estimate[[1]], sdlog = mass.fit[[j]]$estimate[[2]])
      
      rad.fit[[j]] <- fitdist(rad.x[[j]][[i]], distr = 'lnorm', method = 'mle')
      rad.dist[[j]] <- dlnorm(seq(0, 45.72, 0.635), meanlog = rad.fit[[j]]$estimate[[1]], sdlog = rad.fit[[j]]$estimate[[2]])
      
      dim.fit[[j]] <- fitdist(dim.x[[j]][[i]], distr = 'lnorm', method = 'mle')
      # dim.dist[[j]] <- dlnorm(seq(0, 45.72, 0.635), meanlog = dim.fit[[j]]$estimate[[1]], sdlog = dim.fit[[j]]$estimate[[2]])
      dim.dist[[j]] <- dlnorm(seq(0, 5.08, 0.635), meanlog = dim.fit[[j]]$estimate[[1]], sdlog = dim.fit[[j]]$estimate[[2]])
      
      ht.fit[[j]] <- fitdist(ht.x[[j]][[i]], distr = 'lnorm', method = 'mle')
      ht.dist[[j]] <- dlnorm(seq(0, 91.44, 0.635), meanlog = ht.fit[[j]]$estimate[[1]], sdlog = ht.fit[[j]]$estimate[[2]])

    } else if (dist.type == 'normal') {
      
      mass.fit[[j]] <- fitdist(mass.x[[j]][[i]], distr = 'norm', method = 'mle')
      mass.dist[[j]] <- dnorm(seq(0, 4000, 1), mean = mass.fit[[j]]$estimate[[1]], sd = mass.fit[[j]]$estimate[[2]])
      
      rad.fit[[j]] <- fitdist(rad.x[[j]][[i]], distr = 'norm', method = 'mle')
      rad.dist[[j]] <- dnorm(seq(0, 45.72, 0.635), mean = rad.fit[[j]]$estimate[[1]], sd = rad.fit[[j]]$estimate[[2]])
      
      dim.fit[[j]] <- fitdist(dim.x[[j]][[i]], distr = 'norm', method = 'mle')
      # dim.dist[[j]] <- dnorm(seq(0, 45.72, 0.635), mean = dim.fit[[j]]$estimate[[1]], sd = dim.fit[[j]]$estimate[[2]])
      dim.dist[[j]] <- dnorm(seq(0, 5.08, 0.635), mean = dim.fit[[j]]$estimate[[1]], sd = dim.fit[[j]]$estimate[[2]])
      
      ht.fit[[j]] <- fitdist(ht.x[[j]][[i]], distr = 'norm', method = 'mle')
      ht.dist[[j]] <- dnorm(seq(0, 91.44, 0.635), mean = ht.fit[[j]]$estimate[[1]], sd = ht.fit[[j]]$estimate[[2]])
      
    } else if (dist.type == 'pareto') {
      
      mass.fit[[j]] <- pareto.fit(mass.x[[j]][[i]])
      mass.dist[[j]] <- dpareto(seq(0, 4000, 1), shape = mass.fit[[j]]$estimate$lambda, scale = mass.fit[[j]]$estimate$sigma)
      
      rad.fit[[j]] <- pareto.fit(rad.x[[j]][[i]])
      rad.dist[[j]] <- dpareto(seq(0, 45.72, 0.635), shape = rad.fit[[j]]$estimate$lambda, scale = rad.fit[[j]]$estimate$sigma)
      
      dim.fit[[j]] <- pareto.fit(dim.x[[j]][[i]])
      # dim.dist[[j]] <- dpareto(seq(0, 45.72, 0.635), shape = dim.fit[[j]]$estimate$lambda, scale = dim.fit[[j]]$estimate$sigma)
      dim.dist[[j]] <- dpareto(seq(0, 5.08, 0.635), shape = dim.fit[[j]]$estimate$lambda, scale = dim.fit[[j]]$estimate$sigma)
      
      ht.fit[[j]] <- pareto.fit(ht.x[[j]][[i]])
      ht.dist[[j]] <- dpareto(seq(0, 91.44, 0.635), shape = ht.fit[[j]]$estimate$lambda, scale = ht.fit[[j]]$estimate$sigma)
      
    } else if (dist.type == 'weibull') {
      
      mass.fit[[j]] <- fitdist(mass.x[[j]][[i]], distr = 'weibull', method = 'mle')
      mass.dist[[j]] <- dweibull(seq(0, 4000, 1), shape = mass.fit[[j]]$estimate[[1]], scale = mass.fit[[j]]$estimate[[2]])
      
      rad.fit[[j]] <- fitdist(rad.x[[j]][[i]], distr = 'weibull', method = 'mle')
      rad.dist[[j]] <- dweibull(seq(0, 45.72, 0.635), shape = rad.fit[[j]]$estimate[[1]], scale = rad.fit[[j]]$estimate[[2]])
      
      dim.fit[[j]] <- fitdist(dim.x[[j]][[i]], distr = 'weibull', method = 'mle')
      # dim.dist[[j]] <- dweibull(seq(0, 45.72, 0.635), shape = dim.fit[[j]]$estimate[[1]], scale = dim.fit[[j]]$estimate[[2]])
      dim.dist[[j]] <- dweibull(seq(0, 5.08, 0.635), shape = dim.fit[[j]]$estimate[[1]], scale = dim.fit[[j]]$estimate[[2]])
      
      ht.fit[[j]] <- fitdist(ht.x[[j]][[i]], distr = 'weibull', method = 'mle')
      ht.dist[[j]] <- dweibull(seq(0, 91.44, 0.635), shape = ht.fit[[j]]$estimate[[1]], scale = ht.fit[[j]]$estimate[[2]])
      
    }

    # normalize probability distributions
    mass.pdf[[j]] <- mass.dist[[j]] / sum(mass.dist[[j]])
    rad.pdf[[j]] <- rad.dist[[j]] / sum(rad.dist[[j]])
    dim.pdf[[j]] <- dim.dist[[j]] / sum(dim.dist[[j]])
    ht.pdf[[j]] <- ht.dist[[j]] / sum(ht.dist[[j]])

  }
  
  mass.ks <- mass.cvm <- mass.ad <- 0
  rad.ks <- rad.cvm <- rad.ad <- 0
  dim.ks <- dim.cvm <- dim.ad <- 0
  ht.ks <- ht.cvm <- ht.ad <- 0
  
  if (dist.type == 'gev') {
    
    for (j in 1:7) {
    
      # mass.ks <- mass.ks + gofTest(mass.dist[[j]], distribution = 'gev', test = 'ks')$statistic[[1]]
      # mass.cvm <- mass.cvm + gofTest(mass.dist[[j]], distribution = 'gev', test = 'cvm')$statistic[[1]]
      # mass.ad <- mass.ad + gofTest(mass.dist[[j]], distribution = 'gev', test = 'ad')$statistic[[1]]
    
      # rad.ks <- rad.ks + gofTest(rad.dist[[j]], distribution = 'gev', test = 'ks')$statistic[[1]]
      # rad.cvm <- rad.cvm + gofTest(rad.dist[[j]], distribution = 'gev', test = 'cvm')$statistic[[1]]
      # rad.ad <- rad.ad + gofTest(rad.dist[[j]], distribution = 'gev', test = 'ad')$statistic[[1]]
    
      # dim.ks <- dim.ks + gofTest(dim.dist[[j]], distribution = 'gev', test = 'ks')$statistic[[1]]
      # dim.cvm <- dim.cvm + gofTest(dim.dist[[j]], distribution = 'gev', test = 'cvm')$statistic[[1]]
      # dim.ad <- dim.ad + gofTest(dim.dist[[j]], distribution = 'gev', test = 'ad')$statistic[[1]]
    
      # ht.ks <- ht.ks + gofTest(ht.dist[[j]], distribution = 'gev', test = 'ks')$statistic[[1]]
      # ht.cvm <- ht.cvm + gofTest(ht.dist[[j]], distribution = 'gev', test = 'cvm')$statistic[[1]]
      # ht.ad <- ht.ad + gofTest(ht.dist[[j]], distribution = 'gev', test = 'ad')$statistic[[1]]
      
    }
    
  } else {
    
    for (j in 1:7) {
      
      mass.ks <- mass.ks + gofstat(mass.fit[[j]])$ks
      mass.cvm <- mass.cvm + gofstat(mass.fit[[j]])$cvm
      mass.ad <- mass.ad + gofstat(mass.fit[[j]])$ad
      
      rad.ks <- rad.ks + gofstat(rad.fit[[j]])$ks
      rad.cvm <- rad.cvm + gofstat(rad.fit[[j]])$cvm
      rad.ad <- rad.ad + gofstat(rad.fit[[j]])$ad
      
      dim.ks <- dim.ks + gofstat(dim.fit[[j]])$ks
      dim.cvm <- dim.cvm + gofstat(dim.fit[[j]])$cvm
      dim.ad <- dim.ad + gofstat(dim.fit[[j]])$ad
      
      ht.ks <- ht.ks + gofstat(ht.fit[[j]])$ks
      ht.cvm <- ht.cvm + gofstat(ht.fit[[j]])$cvm
      ht.ad <- ht.ad + gofstat(ht.fit[[j]])$ad
      
    }
    
  }
  
  ks <- (mass.ks + rad.ks + dim.ks + ht.ks) / 28
  cvm <- (mass.cvm + rad.cvm + dim.cvm + ht.cvm) / 28
  ad <- (mass.ad + rad.ad + dim.ad + ht.ad) / 28
  
  mass[[i]] <- mass.pdf
  rad[[i]] <- rad.pdf
  dim[[i]] <- dim.pdf
  ht[[i]] <- ht.pdf

  for (j in 1:7) {
    if (i == 1 && j == 7) {
      mass[[i]][[j]] <- c(1, rep.int(0, 4000))
      rad[[i]][[j]] <- c(1, rep.int(0, 72))
      # dim[[i]][[j]] <- c(1, rep.int(0, 72))
      dim[[i]][[j]] <- c(1, rep.int(0, 8))
      ht[[i]][[j]] <- c(1, rep.int(0, 144))
    } else if (i == 2) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 7) {
        mass[[i]][[j]] <- c(1, rep.int(0, 4000))
        rad[[i]][[j]] <- c(1, rep.int(0, 72))
        # dim[[i]][[j]] <- c(1, rep.int(0, 72))
        dim[[i]][[j]] <- c(1, rep.int(0, 8))
        ht[[i]][[j]] <- c(1, rep.int(0, 144))
      }
    } else if (i == 3) {
      if (j == 3 || j == 4 || j == 6 || j == 7) {
        mass[[i]][[j]] <- c(1, rep.int(0, 4000))
        rad[[i]][[j]] <- c(1, rep.int(0, 72))
        # dim[[i]][[j]] <- c(1, rep.int(0, 72))
        dim[[i]][[j]] <- c(1, rep.int(0, 8))
        ht[[i]][[j]] <- c(1, rep.int(0, 144))
      }
    } else if (i == 4) {
      if (j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
        mass[[i]][[j]] <- c(1, rep.int(0, 4000))
        rad[[i]][[j]] <- c(1, rep.int(0, 72))
        # dim[[i]][[j]] <- c(1, rep.int(0, 72))
        dim[[i]][[j]] <- c(1, rep.int(0, 8))
        ht[[i]][[j]] <- c(1, rep.int(0, 144))
      }
    } else if (i == 5) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
        mass[[i]][[j]] <- c(1, rep.int(0, 4000))
        rad[[i]][[j]] <- c(1, rep.int(0, 72))
        # dim[[i]][[j]] <- c(1, rep.int(0, 72))
        dim[[i]][[j]] <- c(1, rep.int(0, 8))
        ht[[i]][[j]] <- c(1, rep.int(0, 144))
      }
    } else if (i == 6) {
      if (j == 2 || j == 3 || j == 4 || j == 5 || j == 6) {
        mass[[i]][[j]] <- c(1, rep.int(0, 4000))
        rad[[i]][[j]] <- c(1, rep.int(0, 72))
        # dim[[i]][[j]] <- c(1, rep.int(0, 72))
        dim[[i]][[j]] <- c(1, rep.int(0, 8))
        ht[[i]][[j]] <- c(1, rep.int(0, 144))
      }
    }
  }

}

saveRDS(mass, file = paste0('mass-', dist.type, '.RData'))
saveRDS(rad, file = paste0('rad-', dist.type, '.RData'))
saveRDS(dim, file = paste0('dim-', dist.type, '.RData'))
saveRDS(ht, file = paste0('ht-', dist.type, '.RData'))
