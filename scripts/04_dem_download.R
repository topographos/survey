remotes::install_github("jhollist/elevatr")

library(sf)
library(elevatr)

set_opentopo_key("56e5496ce2be07db4541731b6282d1e0")

# import data

sites = readRDS("data/ths_sites_2500_500_BC_SF.rds")

sites

st_crs(sites) = 4326

sites_wgs = st_transform(sites, crs = 4326)

sites_sp = as_Spatial(sites_wgs)

plot(sites$geom)

test = data.frame(x = 41.906, y =36.80467)

sites_alos = get_elev_raster(test, src = "gl3", prj = expand = 100)

elevation <- get_elev_raster(sites, z = 9)

lake_sf = st_as_sf(lake)

raster::plot(sites_alos)

examp_df
