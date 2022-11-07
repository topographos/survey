calc_agr_zones_df = function(data, size, h_per_person){

  pop = base::round(100 * data[[size]])                 # calculate population - 100 people per hectar

  agr_zone_h = pop * h_per_person                       # calculate agricultural sustaining

  agr_zone_m = agr_zone_h * 10000                       # convert hectares to meters

  agr_radi_m = round(sqrt(agr_zone_m/pi))               # calculate radius

  zones = sf::st_buffer(data, dist = agr_radi_m)

  zones$agr_area = as.numeric(st_area(zones))

  zones = sf::st_drop_geometry(zones)

  zones = zones[,c(1,2,4,5,3)]

  return(zones)

}
