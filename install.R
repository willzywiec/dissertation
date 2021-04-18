# install.R
#
# William John Zywiec
# The George Washington University

install.packages('BiocManager')
library(BiocManager)
BiocManager::install('graph')
BiocManager::install('RBGL')
BiocManager::install('Rgraphviz')

install.packages('bnlearn')
install.packages('caret')
install.packages('dplyr')
install.packages('EnvStats')
install.packages('evd')
install.packages('fitdistrplus')
# install.packages('forcats')
install.packages('ggplot2')
install.packages('gRain')
install.packages('igraph')
install.packages('knitr')
install.packages('magrittr')
# install.packages('metR')
install.packages('parallel')
# install.packages('ParetoPosStable')
# install.packages('reshape')
install.packages('reticulate')
install.packages('rmarkdown')
install.packages('scales')
install.packages('snow')
install.packages('xfun')

install.packages('keras')
library(keras)
install_keras(tensorflow = 'gpu')
