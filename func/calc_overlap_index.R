calc_over_index = function(data, size, person_per_h = 100, h_per_person = 1, total = TRUE){

  if (!total) {

    agr_area = calc_agr_zones_sf(data = data, size = size, h_per_person)

    agr_area_total = sum(agr_area$agr_area)

    category = split(agr_area, agr_area$category)

    sapply(category, function(x){

      agr_area_union = x %>% st_union() %>% st_area() %>% as.numeric()

      i.over = agr_area_union / agr_area_total

      return(i.over)

    })


  } else {

    agr_area = calc_agr_zones_sf(data = data, size = size, person_per_h, h_per_person)

    agr_area_total = sum(agr_area$agr_area)

    agr_area_union = agr_area %>%
      sf::st_union() %>%
      sf::st_area() %>%
      as.numeric()

    i.over = agr_area_union / agr_area_total

    return(c(i.over))


  }

}



