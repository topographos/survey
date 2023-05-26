calc_agr_zones_sf = function(data, size, person_per_h = 100, h_per_person = 1){
  
  data = subset(data,select = c("id","size_ha"))
  
  pop = base::round(person_per_h * data[[size]])                 # calculate population - 100 people per hectare

  agr_zone_h = pop * h_per_person                       # calculate agricultural sustaining

  agr_zone_m = agr_zone_h * 10000                       # convert hectares to meters

  agr_radi_m = round(sqrt(agr_zone_m/pi))               # calculate radius

  zones = sf::st_buffer(data, dist = agr_radi_m)

  zones$agr_area = as.numeric(st_area(zones))
  
  zones = zones[,c(1,2,4,3)]

  return(zones)

}
