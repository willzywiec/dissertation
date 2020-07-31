# Dissertation

These scripts combine a Bayesian network with an ensemble of neural networks for the purpose of estimating process criticality accident risk. 🤯

**dist.R** first needs to be run for the distribution fit called in **source.Rmd** (e.g., "gamma"). Once that's done, everything can be run from **source.Rmd**.

I built 1.6 million MCNP input decks with **generate.R** and **build.R** and ran them on Quartz (a supercomputer at Lawrence Livermore National Laboratory). The rest of the scripts were run on a desktop computer with an AMD Ryzen 7 1700 3.0 GHz 8-core CPU and Nvidia GeForce GTX 1080 8 GB GDDR5X graphics card.

## Prerequisites
R  
Rtools  
MCNP6.2  
ENDF/B-VII.1 Nuclear Data (the current release of ENDF/B-VIII has problems)  

## R Packages
BiocManager::install('graph')  
BiocManager::install('RBGL')  
BiocManager::install('Rgraphviz')  
bnlearn  
caret  
dplyr  
EnvStats  
evd  
fitdistrplus  
ggplot2  
gRain  
igraph  
keras  
knitr  
magrittr  
parallel  
ParetoPosStable (optional)
scales  
