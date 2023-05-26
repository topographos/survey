# PACKAGES ----
library(sf)
library(SpatialKDE)
library(dplyr)
library(tmap)


# DATA ----

sites = st_read("./data/vect/njs.gpkg", layer = "sites")

survey = st_read("./data/vect/data.gpkg", layer = "njs_survey")

sites_2550BC = sites %>% filter(period == "2550BC to 2000BC")

sites_900BC = sites %>% filter(period == "900BC to 600BC")


# BANDWITH

x = sf::st_coordinates(sites)

sigma = c(sd(x[,1]),sd(x[,2])) * (2 / (3 * nrow(x))) ^ (1/6)

choose_bw = function(sf){
  x = sf::st_coordinates(sf)
  
  sigma = c(sd(x[,1]),sd(x[,2])) * (2 / (3 * nrow(x))) ^ (1/6)
  
  return(sigma)
}

choose_bw(sites_900BC)

# KDE SINGLE ----

raster_grid <- create_raster(survey, cell_size = 250, side_offset = 500)

kde <- kde(sites_900BC, band_width = 1975.030, kernel = "quartic", grid = raster_grid)

plot(kde)

# MULTIPLE

sites_by_time = split(sites, sites$time_start)

# loop

kde_multi = lapply(sites_by_time, kde, band_width = 1975.030, kernel = "quartic", grid = raster_grid)

# FUNCTION ----

survey_kde = function(data = sites, band_width = band_width){
  
  grid <- create_raster(sites, cell_size = 250, side_offset = 500)
  
  sites_by_time = split(sites, sites$time_start)
  
  kde_list = lapply(sites_by_time, kde, band_width = band_width, kernel = "quartic", grid = grid)
  
  kde_list = lapply(kde_list, terra::rast)
  
  kde_spat.rast = terra::rast(kde_list)
  
  return(kde_spat.rast)
}



kde = survey_kde(sites, band_width = 3000)

plot(kde)

# MAP ----
tm_shape(kde) +
  tm_raster(palette = "viridis", title = "KDE Estimate", style = "cont") +
  tm_shape(sites_900BC) +
  tm_bubbles(size = 0.1, col = "red") +
  tm_layout(legend.outside = TRUE)
