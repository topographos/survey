# packages ----
library(sf)
library(spatstat)
library(maptools)
library(janitor)

# sites 
sites_raw = readRDS("data/njs_sites_LONG.rds") %>% filter(period %in% c("Khabur","Middle Assyrian"))

# collapse by id and period
sites = sites_raw %>% 
  group_by(id, period) %>% 
  summarise(
    id = first(id),
    longitude = first(longitude),
    latitude =  first(latitude),
    period =    first(period),
    size_ha = mean(size_ha)
  ) %>% 
  ungroup()

# select longitude and latitude
sites_sf = sites %>% 
  select(id, longitude, latitude, period, size_ha) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4327) %>% 
  st_transform(32637)

# read ppp
ppp = readRDS("data/njs_period_ppp.rds")

nn = nndist(survey.ppp, by = marks(survey.ppp))

nn_df = as.data.frame(nn) %>% janitor::clean_names()

test = sites_nn %>% filter(late_assyrian > 0)

nn_khabur = nndist(split(survey.ppp)$"Khabur") %>% as.data.frame()


# bind with sites data frame

sites_nn = cbind(sites_sf, nn_df)

sites_sum = sites_nn %>% 
  filter(khabur > 0 & period == "Khabur") 

stienen(X = split(ppp)$'Late 3rd Millenium')

?nndist


