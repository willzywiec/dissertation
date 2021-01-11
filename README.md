# Dissertation

These scripts build a coupled Bayesian network and neural network metamodel, which models fissionable material operations in a nuclear facility and estimates process criticality accident risk. 🤯  

MCNP input decks are built with **generate.R** and **build.R** and run with **volley.py** (and **single.py**), which are configured to run on Linux and LSF/Slurm. I'm planning on uploading a compiled **output.csv** file at some point, which would allow users to skip this step.  

**dist.R** also needs to be run for the distribution fit called in **source.Rmd** (e.g., 'gamma'). Once that's done, everything can be run from **source.Rmd**.  

With the exception of the MCNP input decks, which were run on Quartz (a supercomputer at LLNL), everything else was run on a desktop computer (AMD Ryzen 7 1700 3.0 GHz 8-core CPU with an NVIDIA GeForce GTX 1080 GPU).  

## Prerequisites
Python 3.7+ (if running MCNP input decks on Linux and LSF/Slurm)  
R  
Rtools  
MCNP6.2 (or equivalent)  
ENDF/B-VII.1 nuclear data (the current release of ENDF/B-VIII has problems)  

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
scales  
snow  
