# moss-and-vascular-plants
Repository that contains all the files to reproduce the results presented in the manuscript "Major land plant lineages respond differently to climate warming". 

In this study we compare the notional elevation shift of mosses and vascular plants using BDM data.

## Content of the repository

1. **Data-geo**: Background data to produce the map with the locations of the study plots.
2. **Data-raw:** This folder contains the data files used in the presented study. These files were exported from the BDM data-base as described in `R/Data-preparation.R`.
3. **Figures:** This folder contains the figures that are used in the manuscript. These figures are produced by the R-Scripts in the folder `R`.
4. **R**: This folder contains the following R-Scripts to conduct the analyses. 
   - [Data-preparation-and-descriptive-statistics.R](R/Data-preparation-and-descriptive-statistics.R): Selects the relevant BDM-Data and prepares them in a structure that is suitable for all further analyses. The tables with the raw data are saved to the folder `Data-raw`. 
   - [Analyses.R](R/Analyses.R): This file reads the data from the folder `Data-raw` , conducts the analyses and prepares the figures for the manuscript. 

