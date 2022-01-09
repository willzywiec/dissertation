# Dissertation

This repository contains a PDF of my dissertation, "Analysis of Process Criticality Accident Risk Using a Metamodel-Driven Bayesian Network", as well as modified copies of several scripts that are included in the appendix. This is very much a long term work-in-progress, which I'm actively maintaining in a separate repository.  

Running **source/notebook.Rmd** as-is builds a deep neural network metamodel coupled with a Bayesian network. It's entire purpose in life is to convert nuclear facility data into keff values and estimate process criticality accident risk or P(keff) >= 0.95 to 1 🤯. These scripts generate approximately 85 GB of data, which are saved to the **test** folder. To avoid saving this data directly to your computer, I recommend moving the **test** folder to an external hard drive and modifying the **test.dir** path on line 51 of **source/notebook.Rmd**.  

**Fun story**: I started working on my PhD in 2017 at the beginning of the worldwide GPU shortage. At the time, a lot of software (e.g., PyTorch) and cloud-based platforms (e.g., SageMaker) were brand new and starting to take off. I bought some time on AWS, ran tests, and quickly determined that buying a GPU would save me (tens of) thousands of dollars. I ended up purchasing and building a desktop computer around a GTX 1080 and it's been a steady workhorse ever since. About 2/3 of the way through my dissertation, I had to upgrade the RAM to 32 GB to accommodate some of the larger datasets and models I was using. Given everything that's happened since then, my only regret is not purchasing 3 or 4 more GTX 1080s.  

## Deep Learning Desktop Computer
AMD Ryzen 7 1700 3.0 GHz 8-core CPU  
Asus ROG Strix B350-i Motherboard  
Corsair Vengeance LPX 32 GB (2 x 16 GB) DDR4 3200 MHz RAM  
DAN A4-SFGX v2 Mini ITX Case  
Noctua NH-L9a + AM4 Kit Cooler (I also bought a Cryorig C7, but it sat a bit higher than I liked)  
Noctua NT-H1 Thermal Compound  
Nvidia GeForce GTX 1080 8 GB GDDR5X GPU  
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

MCNP input decks were built using **build/grid.R** and run using **linux/copy.py** and **linux/volley.py**, which are configured to run on Linux using Slurm. The data I used in my dissertation consisted of a 130 MB **mcnp-output.csv** file, which contains output from 1,542,397 MCNP simulations that were run on the Quartz supercomputer at LLNL. I haven't posted this .csv file or the simulation output because of export control issues. With the exception of the MCNP simulations, everything was run on the desktop computer that I built specifically to work on this project.  

**dist/dist.R** needs to be run for the truncated probability distribution fits called in **source/notebook.Rmd** (e.g., 'gamma'). Once that's done, everything can be run from **source/notebook.Rmd**.    
  
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
