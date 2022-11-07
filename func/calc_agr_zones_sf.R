calc_agr_zones_sf_old = function(data, size, h_per_person){

  pop = base::round(100 * data[[size]])                 # calculate population - 100 people per hectar

  agr_zone_h = pop * h_per_person                       # calculate agricultural sustaining

  agr_zone_m = agr_zone_h * 10000                       # convert hectares to meters

  agr_radi_m = round(sqrt(agr_zone_m/pi))               # calculate radius

  zones = sf::st_buffer(data, dist = agr_radi_m)

  zones$agr_area = as.numeric(st_area(zones))

  zones = zones[,c(1,2,4,6,3,5)]

  return(zones)

}

calc_agr_zones_sf = function(data, size, h_per_person){
  
  area = st_area(survey)
  
  pop = base::round(100 * data[[size]])
  
  rBaseTotal <- sqrt(area/(sum(pop)*pi))
  
  radiusVectorTotal <-sqrt(pop)*rBaseTotal
  
                # calculate population - 100 people per hectar
  
  agr_zone_h = pop * h_per_person                       # calculate agricultural sustaining
  
  agr_zone_m = agr_zone_h * 10000                       # convert hectares to meters
  
  agr_radi_m = round(sqrt(agr_zone_m/pi))               # calculate radius
  
  zones = sf::st_buffer(data, dist = agr_radi_m)
  
  zones$agr_area = as.numeric(st_area(zones))
  
  zones = zones[,c(1,2,4,6,3,5)]
  
  return(zones)
  
}
