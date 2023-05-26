library(sf)
library(tmap)

zones = st_read("./data/vect/agr_zones.shp")

plot(zones)

int = st_intersection(zones)

lengths(st_overlaps(zones))

leng

int$area = unclass(st_area(int))

int$area_overlap = int$agr_area - int$area

int$index = int$area_overlap / int$agr_area

t = subset(test, n.overlaps == 1)

int[1,]

plot(int)


# plot

tmap_mode("view")
tmap_options(check.and.fix = TRUE)
tm_shape(int) +
  tm_polygons()
