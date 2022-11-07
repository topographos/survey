library(nngeo)
library(spatstat)
library(terra)
# read sites

sites = readRDS("data/tab/njs_sites_LONG_SF.rds") %>% 
  select(id, time_start) %>% 
  rename(period = time_start) %>% 
  arrange(desc(period))



raster_template = rast(ext(sites), resolution = 250, nlyrs = 76,
                       crs = st_crs(sites)$wkt)


sites = vect(sites)

period <- unique(sites$period)
out <- list()

for (i in 1:length(period)) {
  vv <- sites[sites$period == period[i], ]
  x <- rasterize(vv, raster_template, field = 1)
  out[[i]] <- x
}
out <- rast(out)

names(out) <- period

plot(out)

empty_list = list()


for(i in 1:length(sites_list)){
  
  empty_list = rasterize(vect(sites_list[[i]]), raster_template[[i]],field = 1)
  
}

r
empty_list
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
