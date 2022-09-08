####################
#Purpose: this is a script to read sf point data and create a map using ggplot2 package
#Author: Michal Michalski
#Date: 07/02/2021
#Revised:
#Import format: csv
#Output format: sf and geopackage
# resources: https://inbo.github.io/tutorials/tutorials/spatial_crs_coding/

# install packages

# load packages

library(sf) # representation for spatial vector data
library(ggplot2) # plotting and mapping
library(dplyr) # data transformation
library(tidyverse)
library(RColorBrewer)

# load data

sites = readRDS("data/ths_sites_2500_500_BC.rds")

class(sites)

# explore
head(sites)

str(sites)

summary(sites)


# select period to map = e.g. 

sites_LTM = sites %>% filter(period_name == "Late Third Millenium")

plot(sites_LTM$geom)

str(sites_LTM)

# MAP

# define colors
myColors = brewer.pal(8, "Dark2")
names(myColors) = levels(sites$size_cat)
factor()

# create plot
g = ggplot() + 
  geom_sf(data = sites_LTM,
          pch = 21,
          stroke = 1,
          alpha = 0.5,
          aes(
            size =  size_cat,
            fill = size_cat,
          )) +
  #scale_size_manual(values = c(2,4,6,8,10,12,14,16), drop = FALSE) +
  #scale_fill_manual(values = myColors, drop = FALSE ) +
  coord_sf(datum = st_crs(sites_LTM)) +
  theme_minimal()

# move legend to the bottom