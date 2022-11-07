# load packages
library(sf)

# read long data 

sites = readRDS("data/tab/njs_sites_LONG.rds")

head(sites, n = 1)

# provide the EPSG code using st_crs()

crs_wgs84 = st_crs(4326) # WGS84 has EPSG code 4326

sites_sf = st_as_sf(sites, coords = c("longitude", "latitude"))

st_crs(sites_sf) = crs_wgs84

# provide the EPSG code using st_crs() - https://epsg.io/32637

UTM_Zone_37N = st_crs(32637) # IGRS_UTM_Zone_38N EPSG: 3891

# transform sites from 

sites_sf = st_transform(sites_sf, UTM_Zone_37N)

saveRDS(sites_sf,"data/tab/njs_sites_LONG_SF.rds")
