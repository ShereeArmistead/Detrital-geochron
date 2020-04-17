# ------------------------------------------------------------------------------
# Script for detrital zircon data formatting, filtering and plotting
# Data: Armistead et al. in prep. (published in PhD thesis 2019, Uni Adelaide)
# Script author: Sheree Armistead
# Email: sarmistead@laurentian.ca 
# Date: 17th April, 2020
# 0 - load libraries (install these first if not already installed) ------------
library(tidyverse)
library(provenance)
library(cowplot)
library(ggpubr)
library(scatterpie)
library(rnaturalearth)
library(data.table)
library(ggrepel)

# 1 - Colours, intervals, parameters for plots ---------------------------------

# Set the bandwidth for KDE plots in Ma 
bw <- 15

# Colours for plots (picked from colorbrewer2.org) use whatever colours you want
col1 <- '#c7eae5'
col2 <- '#5ab4ac'
col3 <- '#01665e'
col4 <- '#feedde'
col5 <- '#fdae6b'
col6 <- '#e6550d'
col7 <- '#bcbddc'
col8 <- '#756bb1'

# Intervals for the data to be split into for displaying on pie charts, kdes etc 
# NOTE: there should be one more interval than colors i.e. here 8 colour and 9 
# interval points. Make sure these capture the majority of the data, anything 
# outside these intervals will not be plotted on the pies.
i1 <- 1500
i2 <- 1700
i3 <- 1900
i4 <- 2100
i5 <- 2400
i6 <- 2600
i7 <- 2925
i8 <- 3150
i9 <- 3400

# 2 - Data input and formatting ------------------------------------------------
setwd('U:/5. R/Detrital-geochron') # Set working directory

# Magmatic and detrital data from database
mag_data <- read.csv("Itmagzir_upb.csv")
det_data <- read.csv("Itdetzir_compupb.csv")

# Calculate concordance, and whether to use the Pb206/U238 or Pb207/Pb206 age
det_data$concordance <- (det_data$age638 / det_data$age76)*100
det_data$bestage     <- ifelse(det_data$age76 >= 900, 
                               det_data$age76, det_data$age638)
det_data$bestage_2s  <- ifelse(det_data$age76 >= 900, 
                               det_data$age76_2s, det_data$age638_2s)

# Filter data for concordance, exclude rim data & data with uncertainties < 10%
det_data_select <- filter(det_data, concordance > 80 & concordance < 105 & 
                              CoreRim != 'R' & bestage*0.1 >= bestage_2s)

# Calculate Max dep age and count number of analyses
det_data_select <-
    det_data_select %>% 
    group_by(Sample) %>% 
    mutate(MDA = min(bestage), grains=n())

# Filter dataset for: Age (if interested in particular time) & no. of analyses
det_data_gt1500 <- filter(det_data_select, MDA >= 1500 & grains >=30)

# 3 - KDE plots for potential sources (magmatic data) --------------------------
# NOTE: Unfortunately I don't think there's a way to use facet for 
# geom_density() and annoyingly have to create a new dataframe for each plot, 
# if anyone figures out how to do this more efficiently please let me know

# Create a unique dataframe for each 'source region' KDE plot (here there will 
# be 4) based on whatever filter you want (Region, sample etc.)
mag_data_af <- subset(mag_data, Region == 'Africa - Tanzania Craton')
mag_data_ma <- subset(mag_data, Region == 'Madagascar - central' | 
                                Region == 'Madagascar - southern')
mag_data_dh <- subset(mag_data, Region == 'India - Dharwar Craton' | 
                                Region == 'Madagascar - Antongil-Masora')
mag_data_si <- subset(mag_data, Region == 'India - Southern Granulite Terrane')

# Set theme for plots to make it look cleaner (no grid lines) 
# See https://ggplot2.tidyverse.org/reference/ggtheme.html for other themes
theme_set(theme_classic())

# KDE PLOTS - each geom_area() line of code is for one coloured interval - 
# if you have less/more intervals then delete/add a geom_area() line of code. 
# NOTE: The following code will throw an error if the geom_area() interval is 
# outside the x limits set in scale_x_continuous()

# AFRICA
a_af <- ggplot(mag_data_af, aes(x = age)) + 
    geom_density(bw = bw) + 
    labs(x = "Age (Ma)") + 
    annotate('label', x = 3000, y = 0.002, label = "Africa (n=80)", size = 4) +
    scale_x_continuous(limits = c(1500,3500), 
                       breaks = c(1750,2000,2250, 2500,2750,3000,3250)) +
    coord_cartesian(expand = c(0,0), xlim = c(1500,3500)) +
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.title.y=element_blank())
b_af <- ggplot_build(a_af)$data[[1]]
c_af <- a_af + 
    geom_area(data=subset(b_af, x >= i1 & x <= i2+5), aes(x=x,y=y), fill=col1) +
    geom_area(data=subset(b_af, x >= i2 & x <= i3+5), aes(x=x,y=y), fill=col2) +
    geom_area(data=subset(b_af, x >= i3 & x <= i4+5), aes(x=x,y=y), fill=col3) +
    geom_area(data=subset(b_af, x >= i4 & x <= i5+5), aes(x=x,y=y), fill=col4) +
    geom_area(data=subset(b_af, x >= i5 & x <= i6+5), aes(x=x,y=y), fill=col5) +
    geom_area(data=subset(b_af, x >= i6 & x <= i7+5), aes(x=x,y=y), fill=col6) +
    geom_area(data=subset(b_af, x >= i7 & x <= i8+5), aes(x=x,y=y), fill=col7) +
    geom_area(data=subset(b_af, x >= i8 & x <= i9), aes(x=x,y=y), fill=col8)
c_af  

# DHARWAR
a_dh <- ggplot(mag_data_dh, aes(x = age)) +  
    geom_density(bw = bw) + 
    labs(x = "Age (Ma)") +
    annotate('label', x = 1900, y = 0.002, label = "Dharwar (n=91)", size = 4) +
    scale_x_continuous(limits = c(1500,3500), 
                       breaks = c(1750,2000,2250, 2500,2750,3000,3250)) +
    coord_cartesian(expand = c(0,0), xlim = c(1500,3500)) +
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.title.y = element_blank())
b_dh <- ggplot_build(a_dh)$data[[1]]
c_dh <- a_dh + 
    geom_area(data=subset(b_dh, x >= i1 & x <= i2+5), aes(x=x,y=y), fill=col1) +
    geom_area(data=subset(b_dh, x >= i2 & x <= i3+5), aes(x=x,y=y), fill=col2) +
    geom_area(data=subset(b_dh, x >= i3 & x <= i4+5), aes(x=x,y=y), fill=col3) +
    geom_area(data=subset(b_dh, x >= i4 & x <= i5+5), aes(x=x,y=y), fill=col4) +
    geom_area(data=subset(b_dh, x >= i5 & x <= i6+5), aes(x=x,y=y), fill=col5) +
    geom_area(data=subset(b_dh, x >= i6 & x <= i7+5), aes(x=x,y=y), fill=col6) +
    geom_area(data=subset(b_dh, x >= i7 & x <= i8+5), aes(x=x,y=y), fill=col7) +
    geom_area(data=subset(b_dh, x >= i8 & x <= i9), aes(x=x,y=y), fill=col8)
c_dh

# MADAGASCAR
a_ma <- ggplot(mag_data_ma, aes(x = age)) +  
    geom_density(bw = bw) + 
    labs(x = "Age (Ma)") +
    annotate('label', x = 3000, y = 0.005, label = "Madagascar (n=50)", 
             size = 4) +
    scale_x_continuous(limits = c(1500,3500), 
                       breaks = c(1750,2000,2250, 2500,2750,3000,3250)) +
    coord_cartesian(expand = c(0,0), xlim = c(1500,3500)) +
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.title.y = element_blank())
b_ma <- ggplot_build(a_ma)$data[[1]]
c_ma <- a_ma + 
    geom_area(data=subset(b_ma, x >= i1 & x <= i2+5), aes(x=x,y=y), fill=col1) +
    geom_area(data=subset(b_ma, x >= i2 & x <= i3+5), aes(x=x,y=y), fill=col2) +
    geom_area(data=subset(b_ma, x >= i3 & x <= i4+5), aes(x=x,y=y), fill=col3) +
    geom_area(data=subset(b_ma, x >= i4 & x <= i5+5), aes(x=x,y=y), fill=col4) +
    geom_area(data=subset(b_ma, x >= i5 & x <= i6+5), aes(x=x,y=y), fill=col5) +
    geom_area(data=subset(b_ma, x >= i6 & x <= i7+5), aes(x=x,y=y), fill=col6) +
    geom_area(data=subset(b_ma, x >= i7 & x <= i8+5), aes(x=x,y=y), fill=col7) +
    geom_area(data=subset(b_ma, x >= i8 & x <= i9), aes(x=x,y=y), fill=col8)
c_ma

# SOUTHERN INDIA
a_si <- ggplot(mag_data_si, aes(x = age)) +  
    geom_density(bw = bw) + 
    labs(x = "Age (Ma)") + 
    annotate('label', x = 3000, y = 0.005, 
             label = "Southern India (n=48)", size = 4) +
    scale_x_continuous(limits = c(1500,3500), 
                       breaks = c(1750,2000,2250,2500,2750,3000,3250)) +
    coord_cartesian(expand = c(0,0), xlim = c(1500,3500)) +
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.title.y = element_blank())
b_si <- ggplot_build(a_si)$data[[1]]
c_si <- a_si + 
    geom_area(data=subset(b_si, x >= i1 & x <= i2+5), aes(x=x,y=y), fill=col1) +
    geom_area(data=subset(b_si, x >= i2 & x <= i3+5), aes(x=x,y=y), fill=col2) +
    geom_area(data=subset(b_si, x >= i3 & x <= i4+5), aes(x=x,y=y), fill=col3) +
    geom_area(data=subset(b_si, x >= i4 & x <= i5+5), aes(x=x,y=y), fill=col4) +
    geom_area(data=subset(b_si, x >= i5 & x <= i6+5), aes(x=x,y=y), fill=col5) +
    geom_area(data=subset(b_si, x >= i6 & x <= i7+5), aes(x=x,y=y), fill=col6) +
    geom_area(data=subset(b_si, x >= i7 & x <= i8+5), aes(x=x,y=y), fill=col7)+
    geom_area(data=subset(b_si, x >= i8 & x <= i9), aes(x=x,y=y), fill=col8)
c_si

# Arrange plots into the same figure, removing x axes for the top three plots
figure <- ggarrange(
    c_dh+ theme(axis.text.x = element_blank(), 
                axis.ticks.x = element_blank(), 
                axis.title.x = element_blank(), 
                plot.margin = margin(0,0,0,0, "cm")),
    c_si+ theme(axis.text.x = element_blank(), 
                axis.ticks.x = element_blank(), 
                axis.title.x = element_blank(), 
                plot.margin = margin(0,0,0,0, "cm")),
    c_af+ theme(axis.text.x = element_blank(), 
                axis.ticks.x = element_blank(), 
                axis.title.x = element_blank(), 
                plot.margin = margin(0,0,0,0, "cm")),
    c_ma + theme(plot.margin = margin(0,0,0,0, "cm")),
    nrow = 4, align = 'hv')
figure 

# Add a title and y axis label to the plots
fig1 <- annotate_figure(figure,
                        top = text_grob("Potential sources (magmatic samples)", 
                                        face = "bold", size = 14),
                        left = text_grob("Relative probability", rot = 90, 
                                         size=14))
fig1
ggsave(paste0("Fig1_MagmaticSourceRegions_KDEs.png"), 
       width = 14, height = 10, dpi=300, units="cm")

# 4 - Convert detrital data into pie chart format ------------------------------
# Select only the relevant columns of data here
dat_pie <- select(det_data_gt1500, Sample, long, lat, bestage)

# For each sample, calulcate the number of grain ages in each interval 
# specified in section 1
dat_pie <- dat_pie %>%
    group_by(Sample) %>%
    summarise(long = toString(unique(long)), 
              lat = toString(unique(lat)),
              i1 = sum(bestage >= i1 & bestage < i2),
              i2 = sum(bestage >= i2 & bestage < i3),
              i3 = sum(bestage >= i3 & bestage < i4),
              i4 = sum(bestage >= i4 & bestage < i5),
              i5 = sum(bestage >= i5 & bestage < i6),
              i6 = sum(bestage >= i6 & bestage < i7),
              i7 = sum(bestage >= i7 & bestage < i8),
              i8 = sum(bestage >= i8 & bestage < i9))

# 5 - Pie charts on a geographical map  ----------------------------------------
# NOTE: this is a very rough map, you can make much bettermaps with R but I 
# didn't for this purpose

# Ensures the long and lat is in numeric format (required for scatterpie)
dat_pie$long <- as.numeric(dat_pie$long)
dat_pie$lat  <- as.numeric(dat_pie$lat)

# imports a world map from the 'rnaturalearth' package
world   <- ne_countries(returnclass = 'sf')
worldcl <- ne_coastline(returnclass = 'sf')

# plot a map with detrital sample locations represented by pie charts
# NOTE: this would obviously look much better with regions that are closer, but 
# I've included the basic code anyway so that it can be modified/improved if you 
# want to make maps this way
fig2 <- ggplot(data = worldcl)+
    geom_sf(data = world, colour = 'gray97', fill = 'gray97')+
    geom_sf() +
    coord_sf(ylim = c(-30,18), xlim = c(25,81), expand = FALSE)+
    geom_scatterpie(data = dat_pie, aes(x = long, y = lat, group = Sample), 
                    cols = c('i1','i2','i3','i4','i5','i6','i7','i8'), 
                    pie_scale = 0.75, color = NA, 
                    show.legend = FALSE)+
    scale_fill_manual(values = c(col1,col2,col3,col4,col5,col6,col7,col8))
fig2
ggsave(paste0("Fig2_Detrital_Pie_map.png"), 
       width = 20, height = 12, dpi = 300, units = "cm")

# 6 - Format data to create MDS plots with pie chart symbols -------------------

# Select columns needed for Multi-dimensional scaling (MDS)
det_select_MDS <- select(det_data_gt1500, Sample, bestage) 

# These functions transform data into MDS format for the 'Provenance' package
det_select_MDS <- det_select_MDS %>%
    group_by(Sample) %>%
    mutate(grouped_id=row_number())

det_format_MDS <- det_select_MDS %>%
    spread(key=Sample, value=bestage)

det_format_MDS$grouped_id <- NULL

# Saves data in MDS format so that MDS plots can be made
write.csv(det_format_MDS, file = "MDSdat.csv", row.names = FALSE, na = "") 
# Read data back in
det_data_dist <- read.distributional("MDSdat.csv", errorfile = NA, method ="KS")  

# Optional - uncomment the following line to see the basic MDS plot
# plot(MDS(det_data_dist), pch=21, cex=2, nnlines=FALSE)  

MDS_raw <- MDS(det_data_dist)    # Store MDS plot data
MDS_xy  <- MDS_raw[["points"]]   # Extract MDS x and y coordinates from MDS plot
MDS_xy  <- data.frame(MDS_xy)    # Covert x and y data into a dataframe
MDS_xy  <- setDT(MDS_xy, keep.rownames = 'Sample')[] 

# 6b - Combine MDS data with pie chart data and additional attributes if desired 
# NOTE: The no of samples in MDS_xy and dat_pie should be the same 

# Merge the pie data with MDS data based on matching the Sample column
MDS_pie_data_merge <- merge(MDS_xy, dat_pie, by = 'Sample') 

# Merge additional attributes from the original det_data e.g. Region2, Country. 
# (not currently used but could be to highlight particular Regions, Countries)
MDS_pie_data_merge_attributes <- merge(MDS_pie_data_merge, 
                                       det_data[, c("Sample", "Region2", 
                                                    "Country")], by="Sample")

# Remove duplicate rows
MDS_pie_data <- unique(MDS_pie_data_merge_attributes)

# 7 - MDS plot with pie symbols ------------------------------------------------

fig3 <- ggplot(data = MDS_pie_data, 
               aes(x = X1, y = X2, group = Sample, label = Sample)) +  
    geom_scatterpie(data = MDS_pie_data, aes(x = X1, y = X2, group = Sample), 
                    cols = c('i1','i2','i3','i4','i5','i6','i7','i8'), 
                    pie_scale = 1.2, color = NA, show.legend = FALSE) +
    geom_text_repel(aes(label = Sample), 
                    min.segment.length = 0.7, 
                    segment.color = 'darkgrey', 
                    size = 2.5, vjust = -1.3) +
    scale_fill_manual(values = c(col1,col2,col3,col4,col5,col6,col7,col8)) +  
    coord_equal()+
    scale_x_continuous(breaks = c(-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1.0)) +
    scale_y_continuous(breaks = c(-1.0,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6)) +
    theme(axis.title = element_blank())
fig3
ggsave(paste0("Fig3_Detrital_Pie_MDS.png"), 
       width = 20, height = 12, dpi = 300, units = "cm")

# arrange the MDS plot with the sources plot
plot_grid(fig3, fig1)
ggsave(paste0("Fig4_MDS_andSourceKDEs.png"), 
       width = 30, height = 15, dpi = 300, units = "cm")

