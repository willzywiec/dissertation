# Dissertation

**notebook.Rmd** in the **source** directory builds a coupled Bayesian network and neural network metamodel and estimates process criticality accident risk. 🤯  

## Prerequisites
Python 3.7+ (if running MCNP input decks on Linux and LSF/Slurm)  
R  
Rtools  
MCNP6.2 (or equivalent)  
ENDF/B-VII.1 nuclear data (the current release of ENDF/B-VIII has problems)  

**install.R** installs all necessary R packages.

MCNP input decks were built with **grid.R** in the **build** directory and run with **volley.py** in the **linux** directory, which is configured to run on Linux and LSF/Slurm.
I'm planning on uploading a compiled **output.csv** file at some point, which would allow users to skip this step.  

**dist.R** in the **dist** directory also needs to be run for the distribution fit called in **notebook.Rmd** (e.g., 'gamma'). Once that's done, everything can be run from **notebook.Rmd**.  

With the exception of the MCNP input decks, which were run on Quartz (a supercomputer at LLNL), everything else was run on a desktop computer (AMD Ryzen 7 1700 3.0 GHz 8-core CPU with an NVIDIA GeForce GTX 1080 GPU).  

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
