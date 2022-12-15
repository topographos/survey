library(nngeo)
library(spatstat)
library(terra)
# read sites

source("./func/calc_agr_zones_sf.R")

sites = readRDS("data/tab/njs_sites_LONG_SF.rds") %>% 
  select(id, time_start) %>% 
  rename(period = time_start) %>% 
  arrange(desc(period))

sites = st_read("./data/vect/njs.gpkg", layer = "sites")

agr_zones = calc_agr_zones_sf(sites, size = "size_ha", 2)

raster_template = rast(ext(agr_zones), resolution = 250, nlyrs = 12,
                       crs = st_crs(agr_zones)$wkt)


agr_zones = vect(agr_zones)

period <- unique(agr_zones$period)
out <- list()

for (i in 1:length(period)) {
  vv <- agr_zones[agr_zones$period == period[i], ]
  x <- rasterize(vv, raster_template, field = 1)
  out[[i]] <- x
}

out <- rast(out)

names(out) <- period

plot(out)

empty_list = list()


for(i in 1:length(empty_list)){
  
  empty_list = rasterize(vect(sites_list[[i]]), raster_template[[i]],field = 1)
  
}

r
empty_list

plot(out$`-450`)
plot(point_pattern)

library(terra)
library(bespatial)
point_pattern = rast(system.file("raster/point_pattern.tif", package = "bespatial"))
ce3 = bes_p_cushman(out, 100)


ce3 = ce3 %>% 
  mutate(
    value_plus = value * -1,
    period = period
  ) 
plot(out, main = round(ce3$value, 2))

#ce3b = bes_p_cushman(point_pattern, 100, independent = TRUE)
#plot(point_pattern, main = round(ce3b$value, 2))
install.packages("bespatial", repos = "https://nowosad.r-universe.dev")

ggplot() +
  geom_line(data = ce3, mapping = aes(x = period, y = value_plus))
