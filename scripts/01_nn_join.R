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
library(nngeo)
library(tmap)

# import data

st_layers("data/data.gpkg")

tbs_sites = st_read("data/data.gpkg", layer="tbs_sites_2600_300BC")

khabur_sites =  st_read("data/data.gpkg", layer="tbs_khabur_pol")

khabur_pol_fix=  st_read("data/data.gpkg", layer="khabur_outlines_tbs_fix")


# join pol top points

khabur_pol = st_join(khabur_pol_fix, khabur_sites, join = st_nn, k = 1, maxdist = 100)

# add id column
khabur_pol = khabur_pol %>% 
  mutate(
    id = seq(1:length(khabur_pol$geometry))
  ) %>% 
  relocate(id)

# save

st_write(khabur_pol, "data/data.gpkg", layer = "tbs_khabur_pol", delete_layer = TRUE)



