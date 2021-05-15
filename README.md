# Dissertation

This repository contains a PDF of my dissertation, "Analysis of Process Criticality Accident Risk Using a Metamodel-Driven Bayesian Network", as well as generalized copies of several scripts that I wrote, which I'm in the process of refactoring to run from a single directory using PyTorch.  
  
While I was working on this project, breakers in my apartment regularly blew and power outages occurred, so I maintained the source code in the cloud with a hardware backup and wrote several Python and R scripts to automate a lot of saving/reloading processes that are no longer necessary.

**notebook.Rmd** in the **source** folder builds a coupled Bayesian network and neural network metamodel and estimates process criticality accident risk. 🤯  

## Prerequisites
Python 3.7+ (if running MCNP input decks on Linux using LSF/Slurm)  
TensorFlow 2.1+  
CUDA 10+ (optional)  
R  
Rtools  
MCNP6.2 (or equivalent)  
ENDF/B-VII.1 nuclear data (the current release of ENDF/B-VIII has problems)  

**install.R** installs all necessary R packages.

MCNP input decks were built using **grid.R** (**build** folder) and run using **volley.py** (**linux** folder), which is configured to run on Linux using LSF/Slurm.
I'm planning on uploading a compiled **output.csv** file at some point, which would allow users to skip this step.  

**dist.R** in the **dist** folder also needs to be run for the distribution fit called in **notebook.Rmd** (e.g., 'gamma'). Once that's done, everything can be run from **notebook.Rmd**.  

With the exception of the MCNP input decks, which were run on Quartz (a supercomputer at LLNL), everything else was run on a desktop computer (AMD Ryzen 7 1700 3.0 GHz 8-core CPU with an NVIDIA GeForce GTX 1080 GPU).  

All scripts were tested on 4/24/2021 using Python 3.8.5, TensorFlow 2.4, and CUDA 11.3.  

## R Packages Needed
BiocManager  
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
reticulate  
rmarkdown  
scales  
snow  
xfun  
