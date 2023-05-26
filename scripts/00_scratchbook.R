library(tidyverse)
library(sf)
library(tmap)

sites = st_read("./data/vect/njs.gpkg", layer = "sites")

survey = st_read("./data/vect/data.gpkg", layer = "njs_survey") |> select(code)

area_s = as.numeric(st_area(survey))

sites_df= st_drop_geometry(sites)

sites_df$pop = sites_df$size_ha * 100


rBaseTotal <- sqrt(area_s/(sum(sites_df$pop) * pi))

radiusVectorTotal <- sqrt(sites_df$pop)*rBaseTotal

sites.buf = st_buffer(sites, dist = radiusVectorTotal )

tm_shape(sites.buf) +
  tm_borders() +
  tm_facets(by = "time_start")


agr_zones_sf$radius = sqrt(agr_zones_sf$agr_area/pi)

agr_zones_sf$radius_test = radiusVectorTotal

sum(radiusVectorTotal^2 * pi)

sum(area_2)
r.teoret<-sqrt(area/(83*pi))

area.teorer = ((r.teoret^2) * pi) * 83

radiusVectorTotal <- sqrt(pop)*rBaseTotal

sum(radiusVectorTotal)

class(radiusVectorTotal)

sum(sites_df[,2])
# radius = scrt(are/pi)

st_buffer(sites_ia, dist = radiusVectorTotal)

plot(st_buffer(xy_theoretical, dist = rBaseTotal ))

xy_theoretical = sf::st_sample(survey, 83, type = "regular", offset = c(0,0)) %>% st_as_sf()

sum(st_area(sites.buf))

st_area(survey)
