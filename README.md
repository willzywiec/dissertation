# Dissertation

These scripts combine a Bayesian network with an ensemble of deep feedforward neural networks, for the purpose of estimating process criticality accident risk.

**dist.R** needs to be modified and/or run for the distribution fit called in **source.Rmd** (e.g., "gamma"). Once that's done, everything can be run from **source.Rmd**.

I ran these scripts on an AMD Ryzen 7 1700 3.0 GHz 8-core CPU and an Nvidia GeForce GTX 1080 8 GB GDDR5X graphics card. For a more complete description of my build, please e-mail me.

## Prerequisites
MCNP6.2  
ENDF/B-VII.1 Nuclear Data

## R Packages
bnlearn  
caret  
EnvStats  
fitdistrplus  
ggplot2  
igraph  
keras  
magrittr  
parallel  
scales  
