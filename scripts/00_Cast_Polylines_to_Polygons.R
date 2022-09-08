library(sf)

# check layers

st_layers("data/raw/khabur.gpkg")

# read outlines 
khabur_outlines = st_read("data/raw/khabur.gpkg", layer = "khabur_outilines") %>% 
  janitor::clean_names() %>% 
  select(name) %>% 
  st_zm(drop = TRUE)

plot(khabur_outlines$geom)

khabur_outlines_pol = st_cast(khabur_outlines, "POLYGON")

plot(khabur_outlines_pol$geom)


st_write(khabur_outlines_pol, "data/data.gpkg", layer = "khabur_outlines_pol")