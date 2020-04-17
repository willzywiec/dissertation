# Dissertation

These scripts combine a Bayesian network with an ensemble of neural networks for the purpose of estimating process criticality accident risk.

**dist.R** first needs to be run for the distribution fit called in **source.Rmd** (e.g., "gamma"). Once that's done, everything can be run from **source.Rmd**.

I ran these scripts on an AMD Ryzen 7 1700 3.0 GHz 8-core CPU and an Nvidia GeForce GTX 1080 8 GB GDDR5X graphics card. For a more complete description of my build, please e-mail me.

I built all MCNP input decks with **generate.R** and **build.R** and ran them on a supercomputer at Lawrence Livermore National Laboratory.

## Prerequisites
Rtools  
MCNP6.2  
ENDF/B-VII.1 Nuclear Data

## R Packages
BiocManager --> graph  
bnlearn  
caret  
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
scales  
