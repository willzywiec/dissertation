# Dissertation

This repository contains a PDF of my dissertation, "Analysis of Process Criticality Accident Risk Using a Metamodel-Driven Bayesian Network", as well as modified copies of several scripts that I wrote, which I'm in the process of refactoring to run using PyTorch.  
  
While I was working on this project, breakers in my apartment regularly blew, so I maintained the source code in the cloud with a hardware backup and wrote several Python and R scripts to automate a lot of saving/reloading processes that are no longer necessary.  

Running **source/notebook.Rmd** as-is will generate approximately 85 GB of data, which is saved to the **test** folder. To avoid saving this data directly to your main hard drive, I recommend moving the **test** folder to an external hard drive and modifying the **test.dir** path on line 51 of **source/notebook.Rmd**.

## Prerequisites
Python 3.7+  
TensorFlow 2.1+  
CUDA 10+  
R 4+  
Rtools  
MCNP6.2 (optional)  
ENDF/B-VII.1 nuclear data (optional)  

## Running the Code
**install.R** installs all necessary R packages.

MCNP input decks were built using **build/grid.R** and run using **linux/volley.py**, which is configured to run on Linux using Slurm. I'm in the process of configuring Git LFS to upload and version a 130 MB **data/mcnp-output.csv** file, which is a pre-configured dataset that consists of output from 1,542,397 MCNP simulations that were run on Quartz (a supercomputer at LLNL). With the exception of the MCNP simulations, everything was run on a desktop computer (AMD Ryzen 7 1700 3.0 GHz 8-core CPU with an NVIDIA GeForce GTX 1080 GPU).  

**dist/dist.R** needs to be run for the distribution fit called in **source/notebook.Rmd** (e.g., 'gamma'). Once that's done, everything can be run from **source/notebook.Rmd**.  
  
Running **source/notebook.Rmd** builds a coupled Bayesian network and neural network metamodel and estimates process criticality accident risk. 🤯  
  
All scripts were tested on 4/24/2021 using R 4.0.2, Python 3.8.5, TensorFlow 2.4, and CUDA 11.3.  

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
