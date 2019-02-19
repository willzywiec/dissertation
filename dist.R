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

large.sample <- c(27, 10, 8, 0, 2, 8, 1, 1, 0, 0, 50, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
machining <- c(49, 44, 1, 1, 0, 0, 0, 0, 0, 0, 0)
metallurgy <- c(149, 6, 215, 210, 9, 0, 0, 0, 0, 0, 0, 0, 0)
small.sample <- c(54, 26, 1, 2, 1, 14, 63, 101, 0, 1, 4, 0, 0, 0, 0, 0, 0)
solution <- c(10, 22, 0, 0, 0, 0)
waste <- 54

operation <- list(large.sample, machining, metallurgy, small.sample, solution, waste)

condition.a <- condition.b <- condition.c <- condition.d <- condition.e <- condition.m <- condition.p <- list()
rate.a <- rate.b <- rate.c <- rate.d <- rate.e <- rate.m <- rate.p <- list()
exp.a <- exp.b <- exp.c <- exp.d <- exp.e <- exp.m <- exp.p <- list()
pdf.a <- pdf.b <- pdf.c <- pdf.d <- pdf.e <- pdf.m <- pdf.p <- list()

for (i in 1:length(operation)) {
	
	condition.a[[i]] <- operation[[i]][which(sapply(operation[[i]], function(x) x <= 65))]
	condition.b[[i]] <- operation[[i]][which(sapply(operation[[i]], function(x) x <= 220))]
	condition.c[[i]] <- operation[[i]][which(sapply(operation[[i]], function(x) x <= 1200))]
	condition.d[[i]] <- operation[[i]][which(sapply(operation[[i]], function(x) x <= 2500))]
	condition.e[[i]] <- operation[[i]][which(sapply(operation[[i]], function(x) x <= 2500))]
	condition.m[[i]] <- operation[[i]][which(sapply(operation[[i]], function(x) x <= 4000))]
	condition.p[[i]] <- operation[[i]][which(sapply(operation[[i]], function(x) x <= 300))]
	
	if (i < 6) {
		
		rate.a[[i]] <- 1 / mean(condition.a[[i]])
		rate.b[[i]] <- 1 / mean(condition.b[[i]])
		rate.c[[i]] <- 1 / mean(condition.c[[i]])
		rate.d[[i]] <- 1 / mean(condition.d[[i]])
		rate.e[[i]] <- 1 / mean(condition.e[[i]])
		rate.m[[i]] <- 1 / mean(condition.m[[i]])
		rate.p[[i]] <- 1 / mean(condition.p[[i]])
		exp.a[[i]] <- dexp(seq(1, 1000, 10), rate = rate.a[[i]])
		exp.b[[i]] <- dexp(seq(1, 1000, 10), rate = rate.b[[i]])
		exp.c[[i]] <- dexp(seq(1, 1000, 10), rate = rate.c[[i]])
		exp.d[[i]] <- dexp(seq(1, 1000, 10), rate = rate.d[[i]])
		exp.e[[i]] <- dexp(seq(1, 1000, 10), rate = rate.e[[i]])
		exp.m[[i]] <- dexp(seq(1, 1000, 10), rate = rate.m[[i]])
		exp.p[[i]] <- dexp(seq(1, 1000, 10), rate = rate.p[[i]])
		pdf.a[[i]] <- exp.a[[i]] / sum(exp.a[[i]])
		pdf.b[[i]] <- exp.b[[i]] / sum(exp.b[[i]])
		pdf.c[[i]] <- exp.c[[i]] / sum(exp.c[[i]])
		pdf.d[[i]] <- exp.d[[i]] / sum(exp.d[[i]])
		pdf.e[[i]] <- exp.e[[i]] / sum(exp.e[[i]])
		pdf.m[[i]] <- exp.m[[i]] / sum(exp.m[[i]])
		pdf.p[[i]] <- exp.p[[i]] / sum(exp.p[[i]])
		
	} else {
		
		mean.a <- mean(condition.a[[i]])
		mean.b <- mean(condition.b[[i]])
		mean.c <- mean(condition.c[[i]])
		mean.d <- mean(condition.d[[i]])
		mean.e <- mean(condition.e[[i]])
		mean.m <- mean(condition.m[[i]])
		mean.p <- mean(condition.p[[i]])
		exp.a[[i]] <- dnorm(seq(1, 1000, 10), mean = mean.a, sd = 25)
		exp.b[[i]] <- dnorm(seq(1, 1000, 10), mean = mean.b, sd = 25)
		exp.c[[i]] <- dnorm(seq(1, 1000, 10), mean = mean.c, sd = 25)
		exp.d[[i]] <- dnorm(seq(1, 1000, 10), mean = mean.d, sd = 25)
		exp.e[[i]] <- dnorm(seq(1, 1000, 10), mean = mean.e, sd = 25)
		exp.m[[i]] <- dnorm(seq(1, 1000, 10), mean = mean.m, sd = 25)
		exp.p[[i]] <- dnorm(seq(1, 1000, 10), mean = mean.p, sd = 25)
		pdf.a[[i]] <- exp.a[[i]] / sum(exp.a[[i]])
		pdf.b[[i]] <- exp.b[[i]] / sum(exp.b[[i]])
		pdf.c[[i]] <- exp.c[[i]] / sum(exp.c[[i]])
		pdf.d[[i]] <- exp.d[[i]] / sum(exp.d[[i]])
		pdf.e[[i]] <- exp.e[[i]] / sum(exp.e[[i]])
		pdf.m[[i]] <- exp.m[[i]] / sum(exp.m[[i]])
		pdf.p[[i]] <- exp.p[[i]] / sum(exp.p[[i]])
		
	}
	
}

exp <- list(exp.a, exp.b, exp.c, exp.d, exp.e, exp.m, exp.p)
pdf <- list(pdf.a, pdf.b, pdf.c, pdf.d, pdf.e, pdf.m, pdf.p)

main <- c('Large Sample', 'Machining', 'Metallurgy', 'Small Sample', 'Solution', 'Waste')
top <- c('A', 'B', 'C', 'D', 'E', 'M', 'P')

for (i in 1:length(pdf)) {
	x11(); dev.off()
	par(mfrow = c(3, 2))
	for (j in 1:length(operation)) {
		plot(pdf[[i]][[j]], col = 'green', main = paste0(main[j], ' (', top[i], ')'), pch = 20, xlab = '', ylab = '')
		points(exp[[i]][[j]], pch = 20, xlab = '', ylab = '')
	}
}

pdf.str <- pdf

for (i in 1:length(pdf)) {
	for (j in 1:length(operation)) {
		pdf.str[[i]][[j]] <- ''
		for (k in 1:100) {
			if (k > 1) {
				if (pdf[[i]][[j]][k] > 1e-100) {
					pdf.str[[i]][[j]] <- paste0(pdf.str[[i]][[j]], '  , ', formatC(pdf[[i]][[j]][k], format = 'e', digits = 5))
				} else {
					pdf.str[[i]][[j]] <- paste0(pdf.str[[i]][[j]], ' , ', formatC(pdf[[i]][[j]][k], format = 'e', digits = 5))
				}
			} else {
				pdf.str[[i]][[j]] <- formatC(pdf[[i]][[j]][k], format = 'e', digits = 5)
			}
		}
	}
}
