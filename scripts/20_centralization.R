
# PACKAGES ----
library(sf)
library(spatstat)
library(fmap)
library(ggplot2)
library(tmap)
library(mapiso)
library(terra)

# FUNCTIONS ----

source("func/mulit_buffer.R")

# DATA ----

st_layers("./data/vect/data.gpkg")
st_layers("./data/vect/njs.gpkg")

st_write(survey,"./data/vect/njs.gpkg", layer = "survey")
sites = st_read("./data/vect/njs.gpkg", layer = "sites")

survey = st_read("./data/vect/data.gpkg", layer = "njs_survey")

st_layers("./data/vect/urban.gpkg")

urban_sites = st_read("./data/vect/urban.gpkg", layer = "urban_settlements")

urban_sites = st_transform(urban_sites, st_crs(sites))

urban_sites = urban_sites[survey,]

# PPP ----


# create ppp window object
survey.win = as.owin(survey)
class(survey.win)

# ALL SITES
# create ppp with period marks

## extract coordinates
sites_coords = st_coordinates(sites)

## create data frame with marks
# marks_df = data.frame(time = as.factor(sites$time_start), size = sites$size_ha)
marks_df = data.frame(id = sites$id, time = as.factor(sites$time_start), size = sites$size_ha)

## create ppp
survey.ppp = ppp( x = sites_coords[,1], 
                  y = sites_coords[,2], 
                  marks = marks_df,
                  window = survey.win)

# URBAN SITES ----

## extract coordinates
sites_coords = st_coordinates(urban_sites)

## create ppp
urban.ppp = ppp( x = sites_coords[,1], 
                  y = sites_coords[,2], 
                  window = survey.win)


# METHOD - AQUAL AREA RINGS ----
# equal area concentric zones
# Robert D. Drennan, Christian E. Peterson 2008

## SELECT CENTRAL PLACE ----

NJS_1_0_0 = sites %>% 
  select(id) %>% 
  filter(id == "NJS_1_0_0") %>% 
  unique()

## SURVEY ----

tm_shape(survey) +
  tm_borders(lty = "dashed") +
  tm_shape(sites) +
  tm_dots() +
  tm_shape(NJS_1_0_0) +
  tm_symbols(shape = 2,col = "red", size = 1)


## PERIOD 2550 BC ----

### subset
sites2550BC = sites %>% filter(period == "2550BC to 2000BC")

### create rings
rings2550BC = fmap_data(radius_outer = 27000, 
                        ncircles = 12, 
                        geo_centre = NJS_1_0_0, 
                        geo_points = sites2550BC, 
                        sum = "size_ha")

### calculate  proportion
rings_df = st_drop_geometry(rings2550BC)

rings_df$size.prop = rings_df$sum / sum(rings_df$sum) * 100

rings_df$cum.prop = cumsum(rings_df$size.prop)

## calculate B coefficient
Bcoeff_2550BC = ( sum(rings_df$cum.prop) - 650 ) / 550

print(paste("B coefficient for period 2550BC to 2000BC is ", round(Bcoeff_2550BC, 2) ))

# map
(map_2550BC = tm_shape(rings2550BC) +
  tm_polygons(col = "sum", alpha = 0.4) +
  tm_shape(sites2550BC) +
  tm_dots() +
  tm_shape(NJS_1_0_0) +
  tm_symbols(shape = 2,col = "red", size = 1) +
  tm_shape(survey) +
  tm_borders(lty = "dashed") +
  tm_layout(frame = FALSE))

# plot
(plot_2550BC = ggplot(data = rings_df) +
  geom_line(mapping = aes(x = zonal_area , y = size.prop )) +
  scale_x_continuous(breaks = seq(1,12, by = 1)) +
  labs(title = paste0("2550BC - B coefficient: ", round(Bcoeff_2550BC, 2) )) +
  theme_minimal_hgrid() +
  theme(
      axis.text = element_text(size = 8),
      plot.margin = unit(c(0,0,0,2), units = , "cm")))

plot_2550BC_RINGS = plot_grid(tmap_grob(map_2550BC),plot_2550BC)

## PERIOD 900 BC ----

### subset
sites900BC = sites %>% filter(period == "900BC to 600BC")

### create rings
rings900BC = fmap_data(radius_outer = 27000, 
                        ncircles = 12, 
                        geo_centre = NJS_1_0_0, 
                        geo_points = sites900BC, 
                        sum = "size_ha")

### calculate  proportion
rings_df = st_drop_geometry(rings900BC)

rings_df$size.prop = rings_df$sum / sum(rings_df$sum) * 100

rings_df$cum.prop = cumsum(rings_df$size.prop)

## calculate B coefficient
Bcoeff_900BC = ( sum(rings_df$cum.prop) - 650 ) / 550

print(paste("B coefficient for period 900BC to 600BC is ", round(Bcoeff_900BC, 2) ))

# map
(map_900BC = tm_shape(rings900BC) +
    tm_polygons(col = "sum", alpha = 0.4) +
    tm_shape(sites900BC) +
    tm_dots() +
    tm_shape(NJS_1_0_0) +
    tm_symbols(shape = 2,col = "red", size = 1) +
    tm_shape(survey) +
    tm_borders(lty = "dashed") +
    tm_layout(frame = FALSE))

# plot
(plot_900BC = ggplot(data = rings_df) +
    geom_line(mapping = aes(x = zonal_area , y = size.prop )) +
    scale_x_continuous(breaks = seq(1,12, by = 1)) +
    labs(title = paste0("900BC - B coefficient: ", round(Bcoeff_900BC, 2) )) +
    theme_minimal_hgrid() +
    theme(
      axis.text = element_text(size = 8),
      plot.margin = unit(c(0,0,0,2), units = , "cm")))

plot_900BC_RINGS = plot_grid(tmap_grob(map_900BC),plot_900BC)

## PERIOD 300 BC ----

### subset
sites300BC = sites %>% filter(period == "300BC to 50BC")

### create rings
rings300BC = fmap_data(radius_outer = 27000, 
                       ncircles = 12, 
                       geo_centre = NJS_1_0_0, 
                       geo_points = sites300BC, 
                       sum = "size_ha")

### calculate  proportion
rings_df = st_drop_geometry(rings300BC)

rings_df$size.prop = rings_df$sum / sum(rings_df$sum) * 100

rings_df$cum.prop = cumsum(rings_df$size.prop)

## calculate B coefficient
Bcoeff_300BC = ( sum(rings_df$cum.prop) - 650 ) / 550

print(paste("B coefficient for period 300BC to 50BC is ", round(Bcoeff_300BC, 2) ))

# map
(map_300BC = tm_shape(rings300BC) +
    tm_polygons(col = "sum", alpha = 0.4) +
    tm_shape(sites300BC) +
    tm_dots() +
    tm_shape(NJS_1_0_0) +
    tm_symbols(shape = 2,col = "red", size = 1) +
    tm_shape(survey) +
    tm_borders(lty = "dashed") +
    tm_layout(frame = FALSE))

# plot
(plot_300BC = ggplot(data = rings_df) +
    geom_line(mapping = aes(x = zonal_area , y = size.prop )) +
    scale_x_continuous(breaks = seq(1,12, by = 1)) +
    labs(title = paste0("300BC - B coefficient: ", round(Bcoeff_300BC, 2) )) +
    theme_minimal_hgrid() +
    theme(
      axis.text = element_text(size = 8),
      plot.margin = unit(c(0,0,0,2), units = , "cm")))

plot_300BC_RINGS = plot_grid(tmap_grob(map_300BC),plot_300BC)






# METHOD - SIGNATURE LANDSCAPE ----
##  DISTANCE ISO ----

# breaks = seq(0,12000, 1000)

iso = distmap(urban.ppp) %>% 
  rast() %>% 
  mapiso(nbreaks = 12) %>% 
  st_set_crs(st_crs(survey)) %>% 
  rename(
   ring_id = id
  )

## SURVEY ----
# urban sites from Third Millenium as benchmark 

tm_shape(survey) +
  tm_borders(lty = "dashed") +
  tm_shape(sites) +
  tm_dots() +
  tm_shape(urban_sites) +
  tm_symbols(shape = 2,col = "red", size = 1) +
  tm_shape(iso) +
  tm_borders(lty = "dashed")

## PERIOD 2550 BC ----

### subset
sites2550BC = sites %>% filter(period == "2550BC to 2000BC")


iso = st_join(iso, sites2550BC) %>%
  group_by(ring_id) %>% 
  summarise(
    area = sum(size_ha)
  ) %>% 
  mutate(
    area = replace_na(area, 0)
  )

# add sites count
iso$count = lengths(st_intersects(iso, sites2550BC))

### calculate  proportion
iso_df = st_drop_geometry(iso)

iso_df$size.prop = iso$area / sum(iso$area) * 100

iso_df$cum.prop = cumsum(iso_df$size.prop)

## calculate B coefficient
Bcoeff_2550BC = ( sum(iso_df$cum.prop) - 650 ) / 550

print(paste("B coefficient for period 2550BC to 2000BC is ", round(Bcoeff_2550BC, 2) ))

# map 
(map_2550BC = tm_shape(survey) +
  tm_borders(lty = "dashed") +
  tm_shape(iso) +
  tm_polygons(col = "area", lty = "dashed", alpha = 0.4) +
  tm_shape(sites2550BC) +
  tm_dots() +
  tm_shape(urban_sites) +
  tm_symbols(shape = 2,col = "red", size = 1) +
  tm_layout(frame = FALSE))

# plot
(plot_2550BC = ggplot(data = iso_df) +
    geom_line(mapping = aes(x = ring_id , y = size.prop )) +
    geom_line(mapping = aes(x = ring_id , y = count ), lty = "dashed") +
    labs(title = paste0("2550BC - B coefficient: ", round(Bcoeff_2550BC, 2) )) +
    scale_x_continuous(breaks = seq(1,12, by = 1)) +
    theme_minimal_hgrid() +
    theme(
      axis.text = element_text(size = 8),
      plot.margin = unit(c(0,0,0,2), units = , "cm")))

plot_2550BC_ISO = plot_grid(tmap_grob(map_2550BC),plot_2550BC)

## PERIOD 900 BC ----

### subset
sites900BC = sites %>% filter(period == "900BC to 600BC")



iso = st_join(iso, sites900BC) %>%
  group_by(ring_id) %>% 
  summarise(
    area = sum(size_ha)
  ) %>% 
  mutate(
    area = replace_na(area, 0)
  )

# add sites count
iso$count = lengths(st_intersects(iso, sites900BC))

### calculate  proportion
iso_df = st_drop_geometry(iso)

iso_df$size.prop = iso$area / sum(iso$area) * 100

iso_df$cum.prop = cumsum(iso_df$size.prop)

## calculate B coefficient
Bcoeff_900BC = ( sum(iso_df$cum.prop) - 650 ) / 550

print(paste("B coefficient for period 900BC to 600BC is ", round(Bcoeff_900BC, 2) ))

# map 
(map_900BC = tm_shape(survey) +
    tm_borders(lty = "dashed") +
    tm_shape(iso) +
    tm_polygons(col = "area", lty = "dashed", alpha = 0.4) +
    tm_shape(sites900BC) +
    tm_dots() +
    tm_shape(urban_sites) +
    tm_symbols(shape = 2,col = "red", size = 1) +
    tm_layout(frame = FALSE))

# plot
(plot_900BC = ggplot(data = iso_df) +
    geom_line(mapping = aes(x = ring_id , y = size.prop )) +
    geom_line(mapping = aes(x = ring_id , y = count ), lty = "dashed") +
    labs(title = paste0("900BC - B coefficient: ", round(Bcoeff_900BC, 2) )) +
    scale_x_continuous(breaks = seq(1,12, by = 1)) +
    theme_minimal_hgrid() +
    theme(
      axis.text = element_text(size = 8),
      plot.margin = unit(c(0,0,0,2), units = , "cm")))

plot_900BC_ISO = plot_grid(tmap_grob(map_900BC),plot_900BC)

## PERIOD 300 BC ----

### subset
sites300BC = sites %>% filter(period == "300BC to 50BC")


iso = st_join(iso, sites300BC) %>%
  group_by(ring_id) %>% 
  summarise(
    area = sum(size_ha)
  ) %>% 
  mutate(
    area = replace_na(area, 0)
  )

# add sites count
iso$count = lengths(st_intersects(iso, sites300BC))

### calculate  proportion
iso_df = st_drop_geometry(iso)

iso_df$size.prop = iso$area / sum(iso$area) * 100

iso_df$cum.prop = cumsum(iso_df$size.prop)

## calculate B coefficient
Bcoeff_300BC = ( sum(iso_df$cum.prop) - 650 ) / 550

print(paste("B coefficient for period 300BC to 50BC is ", round(Bcoeff_300BC, 2) ))

# map 
(map_300BC = tm_shape(survey) +
    tm_borders(lty = "dashed") +
    tm_shape(iso) +
    tm_polygons(col = "area", lty = "dashed", alpha = 0.4) +
    tm_shape(sites300BC) +
    tm_dots() +
    tm_shape(urban_sites) +
    tm_symbols(shape = 2,col = "red", size = 1) +
    tm_layout(frame = FALSE))

# plot
(plot_300BC = ggplot(data = iso_df) +
    geom_line(mapping = aes(x = ring_id , y = size.prop )) +
    geom_line(mapping = aes(x = ring_id , y = count ), lty = "dashed") +
    labs(title = paste0("300BC - B coefficient: ", round(Bcoeff_300BC, 2) )) +
    scale_x_continuous(breaks = seq(1,12, by = 1)) +
    theme_minimal_hgrid() +
    theme(
      axis.text = element_text(size = 8),
      plot.margin = unit(c(0,0,0,2), units = , "cm")))

plot_300BC_ISO = plot_grid(tmap_grob(map_300BC),plot_300BC)

# PLOTS ----

plot_2550BC_RINGS
plot_900BC_RINGS
plot_300BC_RINGS

plot_2550BC_ISO
plot_900BC_ISO
plot_300BC_ISO



# AUTOMATE -----
# list

sites_list = split(sites, sites$start_date)

names(sites_list[1])
result = list()

for (i in 1:length(sites_list)) {
  rings = fmap_data(radius_outer = 13000, ncircles = 12, geo_centre = TBS_1_0_0, 
                    geo_points = sites_list[[i]], sum = "size_ha")
  result[[i]] = rings
  
  
}


names(result) = names(sites_list)

out = list()

for (i in 1:length(result)) {
  
  df = result[[i]]
  
  df$size.prop = df$sum / sum(df$sum) * 100
  
  df$cum.prop = cumsum(df$size.prop)
  
  out[[i]] = df

}

Bcoeff.List = list()

for (i in 1:length(out)) {
  
  df = out[[i]]
  
  Bcoeff = ( sum(df$cum.prop) - 650 ) / 550

  Bcoeff.List[[i]] = Bcoeff
}

names(Bcoeff.List) = names(sites_list)

Bcoeff.df = as.data.frame(do.call(rbind, Bcoeff.List))

colnames(Bcoeff.df) = c("Bcoeff")

Bcoeff.df$time = rownames(Bcoeff.df)

rownames(Bcoeff.df) = NULL

ggplot(Bcoeff.df) +
  geom_point(aes(x = as.numeric(time), y = Bcoeff))






