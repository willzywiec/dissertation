> 
> library(bnlearn)
> library(caret)
> library(parallel)
> 
> # set variables
> sample.size <- 10
> cluster <- makeCluster((detectCores() / 4), type = 'SOCK')
> 
> # sample conditional probability tables
> bn.data <- cpdist(
+     bn,
+     nodes = c('mass', 'form', 'mod', 'rad', 'ref', 'thk'),
+     evidence = TRUE,
+     batch = sample.size,
+     cluster = cluster,
+     n = sample.size) %>% na.omit()
> 
> stopCluster(cluster)
> 
> print(bn.data)
   mass  form  mod    rad   ref   thk
1    30 alpha none  5.715   ch2 1.905
2     7 alpha none  13.97 ss304 0.635
3    48 alpha none   6.35   ch2 0.635
4    80 alpha none  1.905   ch2 0.635
5    57 alpha none   6.35 ss304 1.905
6    61  puo2 none   2.54 ss304  1.27
7    46  puo2 none  8.255    du  2.54
8    22 alpha none  10.16   mgo 1.905
9    33 alpha none  0.635   ch2 0.635
10    9 alpha none 17.145   ch2  1.27
> 