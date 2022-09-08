library(sf)
library(ggplot2)


# load data

st_layers("data/data.gpkg")

tbs_sites = st_read("data/data.gpkg", layer = "tbs_sites")

head(tbs_sites)

period_count = tbs_sites %>% 
count(period)
