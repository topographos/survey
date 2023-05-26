# PACKAGES ----

library(sf)
library(spatstat)
library(fmap)
library(ggplot2)
library(tmap)
library(mapiso)
library(terra)

# DATA ----

st_layers("./data/vect/njs.gpkg")
sites = st_read("./data/vect/njs.gpkg", layer = "sites")

survey = st_read("./data/vect/data.gpkg", layer = "njs_survey")

# LARGEST SITES ----

max_sites = sites %>% 
  #group_by(time_start) %>%
  slice_max(size_ha)

## plot large sites ----

(max.sites.plot = ggplot(max_sites) + 
   geom_point(aes(x = time_start, y = size_ha)) +
   geom_line(aes(x = time_start, y = size_ha)) +
   theme_bw()
 ) 

# SINGLE PERIOD ----

# 1: select settlement from one period
sites2550BC = sites %>% filter(period == "2550BC to 2000BC")

# 2: select largest site in settlement system
max.site = sites2550BC %>% filter(size_ha == max(size_ha))

# 3: calculate maximum distance between max.sites and sites
max.dist = st_distance(max.site,sites2550BC) %>% max() %>% unclass()

# 4: create rings around max site; 12 circles / summarize sites size
rings2550BC = fmap_multi(radius_outer = max.dist + 1, 
                        ncircles = 12, 
                        geo_centre = max.site, 
                        geo_points = sites2550BC, 
                        id_var = "id",
                        sum = "size_ha",
                        output = "data")

rings2550BC.clip = st_intersection(rings2550BC,survey)

# 5: calculate  proportion
rings_df = st_drop_geometry(rings2550BC)

rings_df$size.prop = rings_df$sum / sum(rings_df$sum) * 100

rings_df$cum.prop = cumsum(rings_df$size.prop)

# 6: calculate B coefficient
Bcoeff_2550BC = ( sum(rings_df$cum.prop) - 650 ) / 550


