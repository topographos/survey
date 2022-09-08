library(sf)
library(dplyr)
library(stringr)

# load all sites

n_mesopotamia_sites = st_read("./data/raw/n_mesopotamia/north_mesopotamia_sites.shp")

# clean names
n_mesopotamia_sites = janitor::clean_names(n_mesopotamia_sites)

# glimpse at data
head(n_mesopotamia_sites)

# add column with code field
n_mesopotamia_sites = n_mesopotamia_sites %>%  mutate(
  code = str_extract(n_mesopotamia_sites$id, pattern = "[a-zA-Z]+")
)

# write to data geopackage

st_write(n_mesopotamia_sites, "data/data.gpkg", layer = "n_mesopotamia_sites")