####################
#Purpose: this is a script to read a csv file with archaeological settlements, creat sf class, export to GIS file
#Author: Michal Michalski
#Date: 05/10/2021
#Revised:
#Import format: csv
#Output format: sf and geopackage
# resources: https://inbo.github.io/tutorials/tutorials/spatial_crs_coding/

# install packages

install.packages("sf")

# load packages

library(sf) # representation for spatial vector data

# import the csv

ths_sites_2500_500_BC = read.csv("ths_sites_2500_500_BC.csv")

# check structure

str(ths_sites_2500_500_BC)

# check first entries

head(ths_sites_2500_500_BC, n = 1)

# check external software used by sf

sf::sf_extSoftVersion()

# provide the EPSG code using st_crs()

crs_wgs84 = st_crs(4326) # WGS84 has EPSG code 4326

# it is a so colled crs object

class(crs_wgs84)

str(crs_wgs84)

# access the wkt element

cat(crs_wgs84$wkt)

# return user input as a number

crs_wgs84$epsg

# PROJ string - old way

crs_wgs84$proj4string

# create a sf object - no crs yet

ths_sites_2500_500_BC_sf = st_as_sf(ths_sites_2500_500_BC, coords = c("longitude", "latitude"))

ths_sites_2500_500_BC_sf

# you can see NA value in CRS

# assign now crs code

# 1 - use EPSG number code, or

st_crs(ths_sites_2500_500_BC_sf) = 4326

# 2 - use the crs_wgs84 object

st_crs(ths_sites_2500_500_BC_sf) = crs_wgs84

# return the crs of sf object

st_crs(ths_sites_2500_500_BC_sf)

# get the WKT2 string

cat(st_crs(ths_sites_2500_500_BC_sf)$wkt)

# plot sf object

plot(ths_sites_2500_500_BC_sf$geometry)

# transform from geodetic to projected

# create

# provide the EPSG code using st_crs() - https://epsg.io/32637

UTM_Zone_37N = st_crs(32637) # IGRS_UTM_Zone_38N EPSG: 3891

# transform sites from 

ths_sites_2500_500_BC_sf = st_transform(ths_sites_2500_500_BC_sf, UTM_Zone_37N)

# look at crs

st_crs(ths_sites_2500_500_BC_sf)

# look at sf class

ths_sites_2500_500_BC_sf

# plot the geometry

plot(ths_sites_2500_500_BC_sf)

# save to geopackage

st_write(ths_sites_2500_500_BC_sf, "data.gpkg", layer = "ths_sites_2500_500_BC", delete_layer = TRUE)

# have a look at content

st_layers("data/data.gpkg",do_count = TRUE)