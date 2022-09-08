# install packages

install.packages("sftime")


#load libraries
library(sf)
library(ggplot2)
library(dplyr)

# read datata

st_layers("./data/data.gpkg")

sites = st_read("./data/data.gpkg", layer = "sites_point")

survey = st_read("./data/data.gpkg", layer = "survey_extent")

# have a look
str(sites)

head(sites)

# select Iron Age sites

sites_iron_age = sites %>% 
  filter(period == "Late Third Millennium")

# voronoi ----
voronoi <- sites_iron_age %>%  # consider the master points
  st_geometry() %>% # ... as geometry only (= throw away the data items)
  st_union() %>% # unite them ...
  st_voronoi() %>% # ... and perform the voronoi tessellation
  st_collection_extract() %>% # select the polygons
  st_sf(crs = 32637) %>% # set metric crs
  st_join(sites_iron_age) %>% # & re-connect the data items
  st_intersection(survey)# limit to Prague city boundaries

# buffer ----

# create a 2km buffer around sites
sites_buffer = st_buffer(sites_iron_age, dist = 2000)

plot(sites_buffer$geom)

# union of the buffers
area_union = st_union(sites_buffer)

plot(area_union)

st_area(area_union)





plot_buffer = ggplot()+
  geom_sf(data = survey, fill = NA, lwd = 1, linetype = 21) +
  geom_sf(data = sites_iron_age, size = 3, pch = 21, fill = "black", color = "white") +
  geom_sf(data = area_union, fill = NA, color = "grey") +
  coord_sf(datum  = st_crs(sites)) +
  theme_bw()
plot_buffer
