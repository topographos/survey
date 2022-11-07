# packages ----
library(sf)
library(spatstat)
library(maptools)

# data ----

# check for layers in geopackage
st_layers("data/data.gpkg")

# read survey extent - espg 32637
survey = st_read("data/data.gpkg", layer = "njs_survey") %>% 
  st_buffer(2000) %>%  
  st_as_sfc()

# create ppp window
survey.win = as.owin(survey)

class(window)  

# read sites in long format
sites_raw = readRDS("data/njs_sites_LONG.rds")

# wrangle by period ----

# collapse by id and period
sites = sites_raw %>% 
group_by(id, period) %>% 
  summarise(
    longitude = first(longitude),
    latitude =  first(latitude),
    period =    first(period),
    size_ha = mean(size_ha)
  ) %>% 
  ungroup()

# select longitude and latitude
sites_sf = sites %>% 
  select(longitude, latitude, period, size_ha) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4327) %>% 
  st_transform(32637)



# create ppp with period marks ----

## extract coordinates
sites_coords = st_coordinates(sites_sf)

## create ppp
survey.ppp = ppp( x = sites_coords[,1], 
                  y = sites_coords[,2], 
                  marks = as.factor(sites_sf$period), 
                  window = survey.win)

summary(survey.ppp)

## save
saveRDS(survey.ppp, "data/njs_by_ppp.rds")


# wrangle by time_start ----

sites_sf = sites_raw %>% 
  select(longitude, latitude, time_start) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4327) %>% 
  st_transform(32637)

# create ppp with time marks ----

## extract coordinates
sites_coords = st_coordinates(sites_sf)

## create ppp
survey.ppp = ppp( x = sites_coords[,1], 
                  y = sites_coords[,2], 
                  marks = as.factor(sites_sf$time_start), 
                  window = survey.win)

summary(survey.ppp)

## save
saveRDS(survey.ppp, "data/njs_by_time_ppp.rds")


nn = nndist(survey.ppp, by = marks(survey.ppp))






  