# Dissertation

These scripts estimate process criticality accident risk using a Bayesian network and a neural network metamodel. 🤯

**dist.R** first needs to be run for the distribution fit called in **source.Rmd** (e.g., "gamma"). Once that's done, everything can be run from **source.Rmd**.

I built 1.6 million MCNP input decks with **generate.R** and **build.R** and ran them on Quartz, a supercomputer at LLNL.
The rest of the scripts were run on a desktop computer (AMD Ryzen 7 1700 3.0 GHz 8-core CPU with an NVIDIA GeForce GTX 1080 GPU).

## Prerequisites
R  
Rtools  
MCNP6.2  
ENDF/B-VII.1 nuclear data (the current release of ENDF/B-VIII has problems)  

## R Packages Needed
BiocManager::install('graph')  
BiocManager::install('RBGL')  
BiocManager::install('Rgraphviz')  
bnlearn  
caret  
dplyr  
EnvStats  
evd  
fitdistrplus  
forcats (optional)  
ggplot2  
gRain  
igraph  
keras  
knitr  
magrittr  
metR (optional)  
parallel  
ParetoPosStable (optional)  
reshape (optional)  
scales  
