# Detrital-geochron

# Description:
This R script 'DetritalPie.v.1.R' uses detrital zircon and magmatic age data to produce a range of plots including Kernel Denisty Estimates (KDE), Multidimensional Scaling (MDS) and geographical maps. 
Inspired largely from the python package DetritalPy (https://github.com/grsharman/detritalPy)

## Example plots

### KDE plots for 'source regions'
These plots use magmatic data as proxies for potential sources of detrital zircon. These are coloured by age intervals for potential source regions (using magmatic data)

<img src="https://github.com/ShereeArmistead/Detrital-geochron/blob/master/Fig1_MagmaticSourceRegions_KDEs.png" alt="alt text" width="500">

### Map with pie chart symbols
Formats detrital zircon data so that pie symbols representing the age distribution can be plotted onto a geographical map.
Better/higher resolution maps can be produced in R, but I've just included a very basic version here.

<img src="https://github.com/ShereeArmistead/Detrital-geochron/blob/master/Fig2_Detrital_Pie_map.png" alt="alt text" width="500">

### Multidimensional Scaling with pie symbols
This section using the 'Provenance' package by Pieter Vermeesch to extract coordinates from the MDS plot and plot each sample as a pie symbol

<img src="https://github.com/ShereeArmistead/Detrital-geochron/blob/master/Fig3_Detrital_Pie_MDS.png" alt="alt text" width="800">
