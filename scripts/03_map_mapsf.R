####################
#Purpose: this is a script to read sf point data and create a map using mapsf package
#Author: Michal Michalski
#Date: 04/02/2021
#Revised:
#Import format: csv
#Output format: sf and geopackage
# resources: https://inbo.github.io/tutorials/tutorials/spatial_crs_coding/

# install packages

install.packages("mapsf")

# load packages

library(sf) # representation for spatial vector data
library(mapsf) # thematic mapping
library(dplyr) # data transformation

# load data

sites = st_read("data/data.gpkg", layer = "ths_sites_2500_500_BC") %>% 
  mutate(
    period_name = as.factor(period_name)
  )

# explore
head(sites)

str(sites)

class(sites$period)

# select period to map = e.g. 
levels(sites$period_name)

sites_LTM = sites %>% filter(period_name == "Late Third Millenium")

# MAP

mf_export(x = mtq, filename = "mtq.png",  
          width = 600, 
          res = 300, 
          theme = "green", 
          expandBB = c(1,1,1,1)) 

# initiate a base map
mf_init(sites_LTM, theme = "default")

#plot sites
mf_map(
  x = sites_LTM,   # sf feature
  var = "area",    #  variable to be mapped
  type = "prop",    # cartography type - Proportional Symbols
  inches = 0.25,
  col = "brown4",
  lwd = 1,
  leg_pos = "bottomleft2",
  leg_title = "Site Area (ha)"
  )

# layout
mf_layout(
  title = "Late Third Millenium",
  credits = paste0(unique(sites$source))
)

dev.off()






