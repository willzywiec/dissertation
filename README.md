# Dissertation

This repository contains a PDF of my dissertation, "Analysis of Process Criticality Accident Risk Using a Metamodel-Driven Bayesian Network", as well as modified copies of several scripts that I wrote, which I'm in the process of refactoring to run using PyTorch.  
  
While I was working on this project, breakers in my apartment regularly popped open, so I maintained the source code in the cloud with a hardware backup and wrote several Python and R scripts to automate a lot of saving/reloading processes that are no longer necessary.

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

MCNP input decks were built using **build/grid.R** and run using **linux/volley.py**, which is configured to run on Linux using Slurm. I'm in the process of configuring Git LFS to upload and version a 130 MB **data/mcnp-output.csv** file, which is a pre-configured dataset that consists of output from 1,542,397 MCNP simulations that were run on Quartz, a supercomputer at LLNL.  

With the exception of the MCNP simulations, everything was run on a desktop computer that I built specifically to work on this project. The specs of this desktop computer are provided below.  

**Deep Learning Desktop Computer**  
AMD Ryzen 7 1700 3.0 GHz 8-core CPU  
Asus ROG Strix B350-i Motherboard  
Corsair Vengeance LPX 16 GB (2 x 8 GB) DDR4 3000 MHz RAM (later upgraded to 32 GB (2 x 16 GB))  
DAN A4-SFGX v2 Mini ITX Case  
Noctua NH-L9a + AM4 Kit Cooler (I also bought a Cryorig C7, but it sat a bit higher than I liked)  
Noctua NT-H1 Thermal Compound  
Nvidia GeForce GTX 1080 8 GB GDDR5X GPU  
Samsung 960 EVO 500 GB M.2-2280 SSD  

I've been running this rig for four years, and it's my favorite computer that I've ever built. I have to give a lot of props to DAN cases (https://www.dan-cases.com/). They make a superior product in a small form factor, and my temperatures were always within tolerance, despite pushing this computer to the limit for days and weeks at a time.  

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
