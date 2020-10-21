# dist.R
#
# William John Zywiec
# The George Washington University

Dist <- function(dist.type) {

  library(EnvStats)
  library(evd)
  library(fitdistrplus)
  library(ggplot2)
  # library(ParetoPosStable)

  # set precision
  options(digits = 15)

  setwd('F:/300/dist')

  # 1 = large sample
  # 2 = machining
  # 3 = metallurgy
  # 4 = small sample
  # 5 = solution
  # 6 = waste
  
  if (file.exists('mass.csv') && file.exists('rad.csv') && file.exists('thk.csv') && file.exists('ht.csv')) {
  
    mass <- as.list(read.csv('mass.csv'))
    rad <- as.list(read.csv('rad.csv'))
    thk <- as.list(read.csv('thk.csv'))
    ht <- as.list(read.csv('ht.csv'))
  
  } else {

    mass.1 <- rnorm(500, mean = 62.5, sd = 74.9)
    mass.2 <- rnorm(500, mean = 28.9, sd = 42.6)
    mass.3 <- rnorm(500, mean = 49.7, sd = 62.6)
    mass.4 <- rnorm(500, mean = 59.9, sd = 54.1)
    mass.5 <- rnorm(500, mean = 8.9,  sd = 12.6)
    mass.6 <- rnorm(500, mean = 84.5, sd = 43.2)

    mass <- list(mass.1, mass.2, mass.3, mass.4, mass.5, mass.6)
    mass.df <- as.data.frame(mass)
    colnames(mass.df) <- c('large sample', 'machining', 'metallurgy', 'small sample', 'solution', 'waste')
    write.csv(mass.df, file = 'mass.csv', row.names = FALSE)

    rad.1 <- rnorm(500, mean = 7.62, sd = 8.09)
    rad.2 <- rnorm(500, mean = 7.62, sd = 8.67)
    rad.3 <- rnorm(500, mean = 7.62, sd = 8.23)
    rad.4 <- rnorm(500, mean = 7.62, sd = 7.37)
    rad.5 <- rnorm(500, mean = 7.62, sd = 8.56)
    rad.6 <- rnorm(500, mean = 7.62, sd = 6.09)

    rad <- list(rad.1, rad.2, rad.3, rad.4, rad.5, rad.6)
    rad.df <- as.data.frame(rad)
    colnames(rad.df) <- c('large sample', 'machining', 'metallurgy', 'small sample', 'solution', 'waste')
    write.csv(rad.df, file = 'rad.csv', row.names = FALSE)

    thk.1 <- rnorm(500, mean = 0.635, sd = 1.48)
    thk.2 <- rnorm(500, mean = 0.635, sd = 1.48)
    thk.3 <- rnorm(500, mean = 0.635, sd = 1.48)
    thk.4 <- rnorm(500, mean = 0.635, sd = 1.48)
    thk.5 <- rnorm(500, mean = 0.635, sd = 1.48)
    thk.6 <- rnorm(500, mean = 0.635, sd = 1.48)

    thk <- list(thk.1, thk.2, thk.3, thk.4, thk.5, thk.6)
    thk.df <- as.data.frame(thk)
    colnames(thk.df) <- c('large sample', 'machining', 'metallurgy', 'small sample', 'solution', 'waste')
    write.csv(thk.df, file = 'thk.csv', row.names = FALSE)

    ht.1 <- rnorm(500, mean = 11.27, sd = 11.97)
    ht.2 <- rnorm(500, mean = 11.27, sd = 12.83)
    ht.3 <- rnorm(500, mean = 11.27, sd = 12.17)
    ht.4 <- rnorm(500, mean = 11.27, sd = 10.89)
    ht.5 <- rnorm(500, mean = 11.27, sd = 12.65)
    ht.6 <- rnorm(500, mean = 11.27, sd = 9.01)

    ht <- list(ht.1, ht.2, ht.3, ht.4, ht.5, ht.6)
    ht.df <- as.data.frame(ht)
    colnames(ht.df) <- c('large sample', 'machining', 'metallurgy', 'small sample', 'solution', 'waste')
    write.csv(ht.df, file = 'ht.csv', row.names = FALSE)
  
  }

  for (i in 1:6) {
    mass[[i]] <- mass[[i]][which(sapply(mass[[i]], function(x) x >= 0))]
    rad[[i]] <- rad[[i]][which(sapply(rad[[i]], function(x) x >= 0))]
    thk[[i]] <- thk[[i]][which(sapply(thk[[i]], function(x) x >= 0))]
    ht[[i]] <- ht[[i]][which(sapply(ht[[i]], function(x) x >= 0))]
  }

  mass.a <- mass.b <- mass.c <- mass.d <- mass.e <- mass.m <- mass.p <- list()
  rad.a <- rad.b <- rad.c <- rad.d <- rad.e <- rad.m <- rad.p <- list()
  thk.a <- thk.b <- thk.c <- thk.d <- thk.e <- thk.m <- thk.p <- list()
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

    thk.a[[i]] <- thk[[i]]
    thk.b[[i]] <- thk[[i]][which(sapply(thk[[i]], function(x) x <= (2 * 2.54)))] # 5.08 cm
    thk.c[[i]] <- thk[[i]][which(sapply(thk[[i]], function(x) x <= 0.1))] # 0.1 cm
    thk.d[[i]] <- thk[[i]][which(sapply(thk[[i]], function(x) x <= (0.25 * 2.54)))] # 0.635 cm
    thk.e[[i]] <- thk[[i]][which(sapply(thk[[i]], function(x) x <= (0.25 * 2.54)))] # 0.635 cm
    thk.m[[i]] <- thk[[i]][which(sapply(thk[[i]], function(x) x <= (0.3 * 2.54)))] # 0.762 cm
    thk.p[[i]] <- thk[[i]]

    ht.a[[i]] <- ht[[i]]
    ht.b[[i]] <- ht[[i]][which(sapply(ht[[i]], function(x) x <= (3000 / pi)^(1/3)))] # 9.85 cm
    ht.c[[i]] <- ht[[i]][which(sapply(ht[[i]], function(x) x <= (1875 / pi)^(1/3)))] # 8.42 cm
    ht.d[[i]] <- ht[[i]][which(sapply(ht[[i]], function(x) x <= (750 / pi)^(1/3)))] # 6.20 cm
    ht.e[[i]] <- ht[[i]][which(sapply(ht[[i]], function(x) x <= (1875 / 19.86 / pi)^(1/3)))] # 3.11 cm
    ht.m[[i]] <- ht[[i]][which(sapply(ht[[i]], function(x) x <= 2.54))] # 56.87 cm
    ht.p[[i]] <- ht[[i]]

  }

  for (i in 1:length(mass.a)) {
    write.csv(mass.a[[i]], file = paste0('mass-a-', i, '-', dist.type, '.csv'), row.names = FALSE)
  }

  mass.ctrl <- list(mass.a, mass.b, mass.c, mass.d, mass.e, mass.m, mass.p)
  rad.ctrl <- list(rad.a, rad.b, rad.c, rad.d, rad.e, rad.m, rad.p)
  thk.ctrl <- list(thk.a, thk.b, thk.c, thk.d, thk.e, thk.m, thk.p)
  ht.ctrl <- list(ht.a, ht.b, ht.c, ht.d, ht.e, ht.m, ht.p)

  op.1 <- op.2 <- op.3 <- op.4 <- op.5 <- op.6 <- list()

  op.list <- list(op.1, op.2, op.3, op.4, op.5, op.6)

  mass.ks <- mass.cvm <- mass.ad <- op.list
  rad.ks <- rad.cvm <- rad.ad <- op.list
  thk.ks <- thk.cvm <- thk.ad <- op.list
  ht.ks <- ht.cvm <- ht.ad <- op.list

  mass <- rad <- thk <- ht <- list()

  for (i in 1:6) {

    mass.fit <- rad.fit <- thk.fit <- ht.fit <- list()
    mass.dist <- rad.dist <- thk.dist <- ht.dist <- list()
    mass.pdf <- rad.pdf <- thk.pdf <- ht.pdf <- list()
    # mass.gof <- rad.gof <- thk.gof <- ht.gof <- list()

    for (j in 1:7) {
    
      if (dist.type == 'gamma') {

        mass.fit[[j]] <- fitdist(mass.ctrl[[j]][[i]], distr = 'gamma', method = 'mle')
        mass.dist[[j]] <- dgamma(seq(0, 4000, 1), rate = mass.fit[[j]]$estimate[[2]], shape = mass.fit[[j]]$estimate[[1]])

        rad.fit[[j]] <- fitdist(rad.ctrl[[j]][[i]], distr = 'gamma', method = 'mle')
        rad.dist[[j]] <- dgamma(seq(0, 45.72, 0.635), rate = rad.fit[[j]]$estimate[[2]], shape = rad.fit[[j]]$estimate[[1]])

        thk.fit[[j]] <- fitdist(thk.ctrl[[j]][[i]], distr = 'gamma', method = 'mle')
        # thk.dist[[j]] <- dgamma(seq(0, 45.72, 0.635), rate = thk.fit[[j]]$estimate[[2]], shape = thk.fit[[j]]$estimate[[1]])
        thk.dist[[j]] <- dgamma(seq(0, 5.08, 0.635), rate = thk.fit[[j]]$estimate[[2]], shape = thk.fit[[j]]$estimate[[1]])

        ht.fit[[j]] <- fitdist(ht.ctrl[[j]][[i]], distr = 'gamma', method = 'mle')
        ht.dist[[j]] <- dgamma(seq(0, 91.44, 0.635), rate = ht.fit[[j]]$estimate[[2]], shape = ht.fit[[j]]$estimate[[1]])

      } else if (dist.type == 'gev') {

        mass.fit[[j]] <- fgev(mass.ctrl[[j]][[i]], method = 'BFGS', std.err = FALSE)
        mass.dist[[j]] <- dgev(seq(0, 4000, 1), loc = mass.fit[[j]]$estimate[[1]], scale = mass.fit[[j]]$estimate[[2]], shape = mass.fit[[j]]$estimate[[3]])
      
        rad.fit[[j]] <- fgev(rad.ctrl[[j]][[i]], method = 'BFGS', std.err = FALSE)
        rad.dist[[j]] <- dgev(seq(0, 45.72, 0.635), loc = rad.fit[[j]]$estimate[[1]], scale = rad.fit[[j]]$estimate[[2]], shape = rad.fit[[j]]$estimate[[3]])
      
        thk.fit[[j]] <- fgev(thk.ctrl[[j]][[i]], method = 'BFGS', std.err = FALSE)
        # thk.dist[[j]] <- dgev(seq(0, 45.72, 0.635), loc = thk.fit[[j]]$estimate[[1]], scale = thk.fit[[j]]$estimate[[2]], shape = thk.fit[[j]]$estimate[[3]])
        thk.dist[[j]] <- dgev(seq(0, 5.08, 0.635), loc = thk.fit[[j]]$estimate[[1]], scale = thk.fit[[j]]$estimate[[2]], shape = thk.fit[[j]]$estimate[[3]])
      
        ht.fit[[j]] <- fgev(ht.ctrl[[j]][[i]], method = 'BFGS', std.err = FALSE)
        ht.dist[[j]] <- dgev(seq(0, 91.44, 0.635), loc = ht.fit[[j]]$estimate[[1]], scale = ht.fit[[j]]$estimate[[2]], shape = ht.fit[[j]]$estimate[[3]])
      
      } else if (dist.type == 'log-normal') {
      
        mass.fit[[j]] <- fitdist(mass.ctrl[[j]][[i]], distr = 'lnorm', method = 'mle')
        mass.dist[[j]] <- dlnorm(seq(0, 4000, 1), meanlog = mass.fit[[j]]$estimate[[1]], sdlog = mass.fit[[j]]$estimate[[2]])
      
        rad.fit[[j]] <- fitdist(rad.ctrl[[j]][[i]], distr = 'lnorm', method = 'mle')
        rad.dist[[j]] <- dlnorm(seq(0, 45.72, 0.635), meanlog = rad.fit[[j]]$estimate[[1]], sdlog = rad.fit[[j]]$estimate[[2]])
      
        thk.fit[[j]] <- fitdist(thk.ctrl[[j]][[i]], distr = 'lnorm', method = 'mle')
        # thk.dist[[j]] <- dlnorm(seq(0, 45.72, 0.635), meanlog = thk.fit[[j]]$estimate[[1]], sdlog = thk.fit[[j]]$estimate[[2]])
        thk.dist[[j]] <- dlnorm(seq(0, 5.08, 0.635), meanlog = thk.fit[[j]]$estimate[[1]], sdlog = thk.fit[[j]]$estimate[[2]])
      
        ht.fit[[j]] <- fitdist(ht.ctrl[[j]][[i]], distr = 'lnorm', method = 'mle')
        ht.dist[[j]] <- dlnorm(seq(0, 91.44, 0.635), meanlog = ht.fit[[j]]$estimate[[1]], sdlog = ht.fit[[j]]$estimate[[2]])

      } else if (dist.type == 'normal') {
      
        mass.fit[[j]] <- fitdist(mass.ctrl[[j]][[i]], distr = 'norm', method = 'mle')
        mass.dist[[j]] <- dnorm(seq(0, 4000, 1), mean = mass.fit[[j]]$estimate[[1]], sd = mass.fit[[j]]$estimate[[2]])
      
        rad.fit[[j]] <- fitdist(rad.ctrl[[j]][[i]], distr = 'norm', method = 'mle')
        rad.dist[[j]] <- dnorm(seq(0, 45.72, 0.635), mean = rad.fit[[j]]$estimate[[1]], sd = rad.fit[[j]]$estimate[[2]])
      
        thk.fit[[j]] <- fitdist(thk.ctrl[[j]][[i]], distr = 'norm', method = 'mle')
        # thk.dist[[j]] <- dnorm(seq(0, 45.72, 0.635), mean = thk.fit[[j]]$estimate[[1]], sd = thk.fit[[j]]$estimate[[2]])
        thk.dist[[j]] <- dnorm(seq(0, 5.08, 0.635), mean = thk.fit[[j]]$estimate[[1]], sd = thk.fit[[j]]$estimate[[2]])
      
        ht.fit[[j]] <- fitdist(ht.ctrl[[j]][[i]], distr = 'norm', method = 'mle')
        ht.dist[[j]] <- dnorm(seq(0, 91.44, 0.635), mean = ht.fit[[j]]$estimate[[1]], sd = ht.fit[[j]]$estimate[[2]])
      
      } else if (dist.type == 'pareto') {
      
        mass.fit[[j]] <- pareto.fit(mass.ctrl[[j]][[i]])
        mass.dist[[j]] <- dpareto(seq(0, 4000, 1), shape = mass.fit[[j]]$estimate$lambda, scale = mass.fit[[j]]$estimate$sigma)
      
        rad.fit[[j]] <- pareto.fit(rad.ctrl[[j]][[i]])
        rad.dist[[j]] <- dpareto(seq(0, 45.72, 0.635), shape = rad.fit[[j]]$estimate$lambda, scale = rad.fit[[j]]$estimate$sigma)
      
        thk.fit[[j]] <- pareto.fit(thk.ctrl[[j]][[i]])
        # thk.dist[[j]] <- dpareto(seq(0, 45.72, 0.635), shape = thk.fit[[j]]$estimate$lambda, scale = thk.fit[[j]]$estimate$sigma)
        thk.dist[[j]] <- dpareto(seq(0, 5.08, 0.635), shape = thk.fit[[j]]$estimate$lambda, scale = thk.fit[[j]]$estimate$sigma)
      
        ht.fit[[j]] <- pareto.fit(ht.ctrl[[j]][[i]])
        ht.dist[[j]] <- dpareto(seq(0, 91.44, 0.635), shape = ht.fit[[j]]$estimate$lambda, scale = ht.fit[[j]]$estimate$sigma)
      
      } else if (dist.type == 'weibull') {
      
        mass.fit[[j]] <- fitdist(mass.ctrl[[j]][[i]], distr = 'weibull', method = 'mle')
        mass.dist[[j]] <- dweibull(seq(0, 4000, 1), shape = mass.fit[[j]]$estimate[[1]], scale = mass.fit[[j]]$estimate[[2]])
      
        rad.fit[[j]] <- fitdist(rad.ctrl[[j]][[i]], distr = 'weibull', method = 'mle')
        rad.dist[[j]] <- dweibull(seq(0, 45.72, 0.635), shape = rad.fit[[j]]$estimate[[1]], scale = rad.fit[[j]]$estimate[[2]])
      
        thk.fit[[j]] <- fitdist(thk.ctrl[[j]][[i]], distr = 'weibull', method = 'mle')
        # thk.dist[[j]] <- dweibull(seq(0, 45.72, 0.635), shape = thk.fit[[j]]$estimate[[1]], scale = thk.fit[[j]]$estimate[[2]])
        thk.dist[[j]] <- dweibull(seq(0, 5.08, 0.635), shape = thk.fit[[j]]$estimate[[1]], scale = thk.fit[[j]]$estimate[[2]])
      
        ht.fit[[j]] <- fitdist(ht.ctrl[[j]][[i]], distr = 'weibull', method = 'mle')
        ht.dist[[j]] <- dweibull(seq(0, 91.44, 0.635), shape = ht.fit[[j]]$estimate[[1]], scale = ht.fit[[j]]$estimate[[2]])
      
      }

      # normalize probability distributions
      mass.pdf[[j]] <- mass.dist[[j]] / sum(mass.dist[[j]])
      rad.pdf[[j]] <- rad.dist[[j]] / sum(rad.dist[[j]])
      thk.pdf[[j]] <- thk.dist[[j]] / sum(thk.dist[[j]])
      ht.pdf[[j]] <- ht.dist[[j]] / sum(ht.dist[[j]])

    }
  
    if (dist.type == 'gev') {
    
      for (j in 1:7) {
    
        # mass.ks[[i]][[j]] <- gofTest(mass.dist[[j]], distribution = 'gev', test = 'ks')$statistic[[1]]
        # mass.cvm[[i]][[j]] <- gofTest(mass.dist[[j]], distribution = 'gev', test = 'cvm')$statistic[[1]]
        # mass.ad[[i]][[j]] <- gofTest(mass.dist[[j]], distribution = 'gev', test = 'ad')$statistic[[1]]
    
        # rad.ks[[i]][[j]] <- gofTest(rad.dist[[j]], distribution = 'gev', test = 'ks')$statistic[[1]]
        # rad.cvm[[i]][[j]] <- gofTest(rad.dist[[j]], distribution = 'gev', test = 'cvm')$statistic[[1]]
        # rad.ad[[i]][[j]] <- gofTest(rad.dist[[j]], distribution = 'gev', test = 'ad')$statistic[[1]]
    
        # thk.ks[[i]][[j]] <- gofTest(thk.dist[[j]], distribution = 'gev', test = 'ks')$statistic[[1]]
        # thk.cvm[[i]][[j]] <- gofTest(thk.dist[[j]], distribution = 'gev', test = 'cvm')$statistic[[1]]
        # thk.ad[[i]][[j]] <- gofTest(thk.dist[[j]], distribution = 'gev', test = 'ad')$statistic[[1]]
    
        # ht.ks[[i]][[j]] <- gofTest(ht.dist[[j]], distribution = 'gev', test = 'ks')$statistic[[1]]
        # ht.cvm[[i]][[j]] <- gofTest(ht.dist[[j]], distribution = 'gev', test = 'cvm')$statistic[[1]]
        # ht.ad[[i]][[j]] <- gofTest(ht.dist[[j]], distribution = 'gev', test = 'ad')$statistic[[1]]
      
      }
    
    } else {
    
      for (j in 1:7) {
      
        mass.ks[[i]][[j]] <- gofstat(mass.fit[[j]])$ks
        mass.cvm[[i]][[j]] <- gofstat(mass.fit[[j]])$cvm
        mass.ad[[i]][[j]] <- gofstat(mass.fit[[j]])$ad
      
        rad.ks[[i]][[j]] <- gofstat(rad.fit[[j]])$ks
        rad.cvm[[i]][[j]] <- gofstat(rad.fit[[j]])$cvm
        rad.ad[[i]][[j]] <- gofstat(rad.fit[[j]])$ad
      
        thk.ks[[i]][[j]] <- gofstat(thk.fit[[j]])$ks
        thk.cvm[[i]][[j]] <- gofstat(thk.fit[[j]])$cvm
        thk.ad[[i]][[j]] <- gofstat(thk.fit[[j]])$ad
      
        ht.ks[[i]][[j]] <- gofstat(ht.fit[[j]])$ks
        ht.cvm[[i]][[j]] <- gofstat(ht.fit[[j]])$cvm
        ht.ad[[i]][[j]] <- gofstat(ht.fit[[j]])$ad
      
      }
    
    }
  
    mass[[i]] <- mass.pdf
    rad[[i]] <- rad.pdf
    thk[[i]] <- thk.pdf
    ht[[i]] <- ht.pdf

    for (j in 1:7) {
      if (i == 1 && j == 7) {
        mass[[i]][[j]] <- c(1, rep.int(0, 4000))
        rad[[i]][[j]] <- c(1, rep.int(0, 72))
        # thk[[i]][[j]] <- c(1, rep.int(0, 72))
        thk[[i]][[j]] <- c(1, rep.int(0, 8))
        ht[[i]][[j]] <- c(1, rep.int(0, 144))
      } else if (i == 2) {
        if (j == 2 || j == 3 || j == 4 || j == 5 || j == 7) {
          mass[[i]][[j]] <- c(1, rep.int(0, 4000))
          rad[[i]][[j]] <- c(1, rep.int(0, 72))
          # thk[[i]][[j]] <- c(1, rep.int(0, 72))
          thk[[i]][[j]] <- c(1, rep.int(0, 8))
          ht[[i]][[j]] <- c(1, rep.int(0, 144))
        }
      } else if (i == 3) {
        if (j == 3 || j == 4 || j == 6 || j == 7) {
          mass[[i]][[j]] <- c(1, rep.int(0, 4000))
          rad[[i]][[j]] <- c(1, rep.int(0, 72))
          # thk[[i]][[j]] <- c(1, rep.int(0, 72))
          thk[[i]][[j]] <- c(1, rep.int(0, 8))
          ht[[i]][[j]] <- c(1, rep.int(0, 144))
        }
      } else if (i == 4) {
        if (j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
          mass[[i]][[j]] <- c(1, rep.int(0, 4000))
          rad[[i]][[j]] <- c(1, rep.int(0, 72))
          # thk[[i]][[j]] <- c(1, rep.int(0, 72))
          thk[[i]][[j]] <- c(1, rep.int(0, 8))
          ht[[i]][[j]] <- c(1, rep.int(0, 144))
        }
      } else if (i == 5) {
        if (j == 2 || j == 3 || j == 4 || j == 5 || j == 6 || j == 7) {
          mass[[i]][[j]] <- c(1, rep.int(0, 4000))
          rad[[i]][[j]] <- c(1, rep.int(0, 72))
          # thk[[i]][[j]] <- c(1, rep.int(0, 72))
          thk[[i]][[j]] <- c(1, rep.int(0, 8))
          ht[[i]][[j]] <- c(1, rep.int(0, 144))
        }
      } else if (i == 6) {
        if (j == 2 || j == 3 || j == 4 || j == 5 || j == 6) {
          mass[[i]][[j]] <- c(1, rep.int(0, 4000))
          rad[[i]][[j]] <- c(1, rep.int(0, 72))
          # thk[[i]][[j]] <- c(1, rep.int(0, 72))
          thk[[i]][[j]] <- c(1, rep.int(0, 8))
          ht[[i]][[j]] <- c(1, rep.int(0, 144))
        }
      }
    }

  }

  ks <- c(unlist(mass.ks), unlist(rad.ks), unlist(thk.ks))
  cvm <- c(unlist(mass.cvm), unlist(rad.cvm), unlist(thk.cvm))
  ad <- c(unlist(mass.ad), unlist(rad.ad), unlist(thk.ad))

  test.statistics <- data.frame(ks = ks, cvm = cvm, ad = ad)

  saveRDS(mass, file = paste0('mass-', dist.type, '.RData'))
  saveRDS(rad, file = paste0('rad-', dist.type, '.RData'))
  saveRDS(thk, file = paste0('thk-', dist.type, '.RData'))
  saveRDS(ht, file = paste0('ht-', dist.type, '.RData'))

  return(test.statistics)

}

a <- Dist('gamma')
b <- Dist('normal')
c <- Dist('log-normal')
d <- Dist('weibull')
