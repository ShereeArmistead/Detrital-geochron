# Detrital-geochron

# Description:
This R script formats and filters detrital zircon data using common methods, particularly useful for large datasets or compiled data. These include: calculating maximum depositional ages, calculating and filtering for concordance, excluding zircon rim data, exclduing data that have too large uncertainties, choosing the 'best age' (i.e. Pb207/Pb206 or Pb206/U238 age) based on age of data etc. 

This R script uses detrital zircon and magmatic age data to produce a range of plots including Kernel Denisty Estimates (KDE) with coloured age ranges, Multidimensional Scaling (MDS) plots with pie chart symbols, and geographical maps with pie chart symbols. 
Inspired largely from the plots in the python package DetritalPy (https://github.com/grsharman/detritalPy) by Glenn R. Sharman.

Example data used in this script are from Armistead et al., in prep (compiled and published in PhD thesis 2019, Uni Adelaide)

## Example plots

### KDE plots for 'source regions'
These plots use magmatic data as proxies for potential sources of detrital zircons. These are coloured by age intervals for potential source regions (using magmatic data)

<img src="https://github.com/ShereeArmistead/Detrital-geochron/blob/master/Fig1_MagmaticSourceRegions_KDEs.png" alt="alt text" width="500">

### Map with pie chart symbols
Formats detrital zircon data so that pie symbols representing the age distribution can be plotted onto a geographical map.
Better/higher resolution maps can be produced in R, but I've just included a very basic version here.

<img src="https://github.com/ShereeArmistead/Detrital-geochron/blob/master/Fig2_Detrital_Pie_map.png" alt="alt text" width="500">

### Multidimensional Scaling with pie symbols
This section using the 'Provenance' package by Pieter Vermeesch to extract coordinates from the MDS plot and plot each sample as a pie symbol using conventional ggplot() plotting. 

<img src="https://github.com/ShereeArmistead/Detrital-geochron/blob/master/Fig3_Detrital_Pie_MDS.png" alt="alt text" width="800">

This is my first upload to Github, please let me know if you run into any issues or have any feedback/suggestions! :) 
