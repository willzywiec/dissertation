# gRain.R
#
# William John Zywiec
# The George Washington University

library(gRain)

geq <- 300

grain.finding <- numeric()

for (i in 1:length(geq:3999)) {
  grain.bn <- as.grain(bn)
  grain.finding[i] <- setFinding(grain.bn, nodes = 'mass', states = as.character(i + geq)) %>% pFinding()
}

cat(formatC(sum(grain.finding), format = 'e', digits = 4))

formatC(sum(grain.finding[0:100]), format = 'e', digits = 4)
formatC(sum(grain.finding[100:200]), format = 'e', digits = 4)
formatC(sum(grain.finding[200:300]), format = 'e', digits = 4)
formatC(sum(grain.finding[300:400]), format = 'e', digits = 4)
