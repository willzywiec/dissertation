# Dissertation

This repository contains a PDF of my dissertation, "Analysis of Process Criticality Accident Risk Using a Metamodel-Driven Bayesian Network", as well as slightly modified copies of several R scripts I wrote between 2017 and 2021. This is very much a long term work-in-progress, which has morphed into a CRAN package I currently maintain (https://www.github.com/willzywiec/criticality).  

These scripts build a deep neural network metamodel coupled with a Bayesian network, which can be used to estimate the risk of bad things happening (e.g., P(keff) >= 1 🤯). When combined with a database of ~1.5 million MCNP simulations, these scripts generate approximately 85 GB of data, which are saved to the **test** folder.  

**Fun story**: I started working on my PhD in August of 2017, which coincided with the beginning of the (now ongoing) worldwide GPU shortage. At the time, purchasing a GTX 1080 for $500-600 seemed a little expensive, just to work on a dissertation, so I decided to rent a GPU-enabled AWS server to run a few tests. To my horror, I blew past $150 after three days of training a toy metamodel. Luckily, Best Buy allowed me to reserve a GTX 1080 for in-store pickup, which is something they no longer do (Boo!). I spent around $1700 to build a deep learning desktop computer and it's been a coin-saving workhorse ever since. About 2/3 of the way through my dissertation, I had to upgrade the RAM to 32 GB to accommodate some of the larger datasets and metamodels I was building. Given everything that's happened since then, my only regret is not purchasing 3 or 4 more GTX 1080s when I had the chance.  

## Deep Learning Desktop Computer
AMD Ryzen 7 1700 3.0 GHz 8-core CPU  
Asus ROG Strix B350-i Motherboard  
Corsair Vengeance LPX 32 GB (2 x 16 GB) DDR4 3200 MHz RAM  
DAN A4-SFGX v2 Mini ITX Case  
Noctua NH-L9a + AM4 Kit Cooler (I also bought a Cryorig C7, but it sat a bit higher than I liked)  
Noctua NT-H1 Thermal Compound  
Nvidia GeForce GTX 1080 8 GB GDDR5X Founders Edition GPU  
Samsung 960 EVO 500 GB M.2-2280 SSD  

## Prerequisites
Python 3.7+  
TensorFlow 2.1+  
CUDA 10+  
R 4+  
Rtools  
MCNP6.2 (optional)  
ENDF/B-VII.1 nuclear data (optional)  
32+ GB RAM (I initially started this project with 16 GB RAM and eventually ran into memory issues)  

## Running the Code
**install.R** installs all necessary R packages.

MCNP input decks were built using **build/grid.R** and run using **linux/copy.py** and **linux/volley.py**, which are configured to run on Linux using Slurm. The data I used in my dissertation consisted of a 130 MB **mcnp-output.csv** file, which contains output from 1,542,397 MCNP simulations that were run on the Quartz supercomputer at LLNL. I haven't posted this .csv file or the simulation output because of potential export control issues. With the exception of the MCNP simulations, everything was run on the desktop computer I built specifically to work on this project.  

**dist/dist.R** needs to be run for the truncated probability distribution fits called in **source/notebook.Rmd** (e.g., 'gamma'). Once that's done, everything else can be run from **source/notebook.Rmd**.    
  
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
