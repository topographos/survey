#
area_s = as.numeric(st_area(survey))

area_agr = sum(as.numeric(st_area(agr_zones_sf)))

sites_df= st_drop_geometry(sites_ia)

sites_df$size_km = sites_df$size_ha / 100

pop = round(sites_df$size_ha * 100)

rBaseTotal <- sqrt(area_s/(sum(sites_df$size_ha) * pi))

radiusVectorTotal <- sqrt(sites_df$size_ha)*rBaseTotal

plot(st_buffer(sites_ia$geometry, dist = radiusVectorTotal ))
plot(agr_zones_sf$geometry)


agr_zones_sf$radius = sqrt(agr_zones_sf$agr_area/pi)

agr_zones_sf$radius_test = radiusVectorTotal

sum(radiusVectorTotal^2 * pi)

sum(area_2)
r.teoret<-sqrt(area/(83*pi))

area.teorer = ((r.teoret^2) * pi) * 83

radiusVectorTotal <- sqrt(pop)*rBaseTotal

sum(radiusVectorTotal)

class(radiusVectorTotal)

sum(sites_df[,2])
# radius = scrt(are/pi)

st_buffer(sites_ia, dist = radiusVectorTotal)

plot(st_buffer(xy_theoretical, dist = rBaseTotal ))

xy_theoretical = sf::st_sample(survey, 83, type = "regular", offset = c(0,0)) %>% st_as_sf()



