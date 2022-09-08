####################
#Purpose: this is a script to read sf point data and create a map using ggplot2 package
#Author: Michal Michalski
#Date: 07/02/2021
#Revised:
#Import format: csv
#Output format: sf and geopackage
# resources: 

#

# load packages

library(sf) # representation for spatial vector data
library(janitor)


# import the csv

khabur_sites = read.csv("./data/raw/menze-ur_khabur_sites-data.csv", na.strings =c(" NA", "NA", "NA ", " NA ")) %>% 
  janitor::clean_names()

head(khabur_sites)

khabur_sites_sf = st_as_sf(khabur_sites, coords = c("longitude_utm", "latitude_utm"))

st_crs(khabur_sites_sf) = 32637

# write to geopackage

st_write(khabur_sites_sf, "data/data.gpkg", layer = "khabur_sites_point", delete_layer = TRUE)
