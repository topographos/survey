# load packahes
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tmap)
library(sp)

# DATA ----

# survey
survey = st_read("./data/vect/data.gpkg", layer = "njs_survey")

#  sites
sites = readRDS("./data/tab/njs_sites_LONG_SF.rds")

sites_ia = sites %>% filter(time_start == - 900)

sites_ia_df = data.frame(id = 1:83, size_ha = sites_ia$size_ha)

# REGULAR ----

set.seed(225)

regular  = st_sample(survey, 83, type = "regular") %>% 
  st_as_sf() %>% 
  rename(geometry = x)

# plot
plot(regular)

# add id column
regular$id = 1:83

# add period column
regular$period = "Regular"

# join to add size_ha column
regular = left_join(regular, sites_ia_df)

# re-order
regular = regular[,c(1,3,2,4)]

# HEXAGON ----

set.seed(225)

hexagonal  = st_sample(survey, 82, type = "hexagonal") %>% 
  st_as_sf() %>% 
  rename(geometry = x)

# plot
plot(hexagonal)

# add id column
hexagonal$id = 1:83

# add period column
hexagonal$period = "Hexagonal"

# join to add size_ha column
hexagonal = left_join(hexagonal, sites_ia_df)

# re-order
hexagonal = hexagonal[,c(1,3,2,4)]

# RANDOM ----

set.seed(300)

random  = st_sample(survey, 83, type = "random")  %>% 
  st_as_sf() %>% 
  rename(geometry = x)

# plot
plot(random)

# add id column
random$id = 1:83

# add period column
random$period = "Random"

# join to add size_ha column
random = left_join(random, sites_ia_df)

# re-order
random = random[,c(1,3,2,4)]

# CLUSTERED ----

set.seed(225)
clust = sp::spsample(as(survey, "Spatial"), 83, type = "clustered",
                     nclusters = 8)

clust = st_as_sf(clust)
plot(clust)

# add id column
clust$id = 1:77

# add period column
clust$period = "Clustered"

# join to add size_ha column
clust = left_join(clust, sites_ia_df)

# re-order
clust = clust[,c(1,3,2,4)]

# COMBINE ----

point_pattern = rbind(regular, random, clust, hexagonal)

saveRDS(point_pattern, "data/tab/point_pattern.rds")

# PLOT ----

tm_shape(survey) +
  tm_borders() +
tm_shape(point_pattern) +
  tm_symbols(size = 0.5) +
  tm_facets(by = "period",free.coords = FALSE) +
tm_layout(inner.margins = 0.1)
