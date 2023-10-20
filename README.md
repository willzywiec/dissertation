# Dissertation

This repository contains slightly modified copies of several R scripts I wrote between 2017 and 2021, which appear in my dissertation. This is a long term work-in-progress, which morphed into a CRAN package (https://www.github.com/willzywiec/criticality).  

These scripts build a deep neural network metamodel of Monte Carlo radiation transport simulations, which are used to estimate process criticality accident risk [P(keff) >= 1 🤯]. The default configuration of these scripts generate approximately 85 GB of data, which are saved to the **test** folder. The hardware and software I used are provided below.  

GWU dissertations don't allow an epigraph, but the one I would've used is provided below.

"Risk comes from not knowing what you're doing." - Warren Buffett  

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
32+ GB RAM (I initially started this project with 16 GB RAM and eventually ran into memory issues while running R)  

## Running the Code
**install.R** installs all necessary R packages.

MCNP input decks were built using **build/grid.R** and run using **linux/copy.py** and **linux/volley.py**, which are configured to run on Red Hat Linux using Slurm. The data I used in my dissertation consisted of a 130 MB **mcnp-output.csv** file, which contains output from 1,542,397 MCNP simulations that were run on the Quartz supercomputer at LLNL. I haven't posted the .csv file or simulation output, but an example of the required .csv file format is shown below. With the exception of the MCNP simulations, everything was run on a desktop computer I built for this project.  

![excel-screenshot](https://github.com/willzywiec/dissertation/assets/30445407/b9c383c8-bd33-432e-8bec-06a833e3d3f8)

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

## Closing Remarks
I recently compiled an updated PDF of my dissertation using MiKTeX and the **latexmk dissertation.tex** command. I didn't include the LaTeX source code here, but if I forget how it's done or someone else stumbles upon this thread, it's generally easier to install LaTeXTools and compile PDFs directly in Sublime Text using **package control**.  
