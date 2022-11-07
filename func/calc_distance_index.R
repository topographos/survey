calc_dist_index = function(data, survey, total){


  if (!total) {

    category = split(data, data$category)

    sapply(category, function(x){

      n = nrow(x)

      if (n > 1) {

      xy_theoretical = sf::st_sample(survey, n, type = "regular", offset = c(0,0)) %>% st_as_sf()

      dist_emprical = mean(dist(as.matrix(st_coordinates(x))))

      dist_theoretical = mean(dist(as.matrix(st_coordinates(xy_theoretical))))

      i.dist = dist_emprical / dist_theoretical

      return(i.dist)

      } else {

        return(0)
      }

    })

  } else {

    xy_empirical = data

    n = nrow(xy_empirical)

    xy_theoretical = sf::st_sample(survey, n, type = "regular", offset = c(0,0)) %>% st_as_sf()

    dist_emprical = mean(dist(as.matrix(st_coordinates(xy_empirical))))

    dist_theoretical = mean(dist(as.matrix(st_coordinates(xy_theoretical))))

    i.dist = dist_emprical / dist_theoretical

    return(c(i.dist))

  }

}

