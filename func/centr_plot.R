# data arguments
sites = sf::st_read("./data/vect/njs.gpkg", layer = "sites")

survey = sf::st_read("./data/vect/njs.gpkg", layer = "survey") |> dplyr::select(code)

# time_start arguments

sites = sites |> dplyr::filter(time_start == -3000)

period = sites |> sf::st_drop_geometry() |> distinct(period) |> as.character()

time_start = -3000

# TEST

centr_area_b_panel(sites = sites, survey = survey, time = -3000)

centr_area_b_coeff(sites = sites, survey = survey, time = -2550, geo_centre = "max")

rbind(a,b)


vec <- c(6, 3, 9, 0, 6, 5)  

time.vc = as.vector(unique(sites$time_start))


df <- data.frame()

for (i in 1:length(time.vc)) {
  
  result = centr_area_b_coeff(sites = sites, survey = survey, time = time.vc[i], geo_centre = "max")

  df = rbind(df,result)
}

for (i in 1:length(time.vc)) {
  
   centr_area_b_panel(sites = sites, survey = survey, time = time.vc[i])
}


# function
centr_area_b_coeff = function(sites, survey, time, geo_centre = c("cmed", "max")){
  
  # load libraries
  library(fmap)
  source("func/mulit_buffer.R")
  
  # subset sites to time period
  sites = sites |> dplyr::filter(time_start == time)
  
  if(geo_centre == "cmed") {
    
  # calculate median center
  cmed <- sfdep::center_median(sites) |> 
    sf::st_as_sf() |>  
    dplyr::rename(
      geom = x
    ) |> 
    dplyr::mutate(
      id = "cmed"
    )
  
  # calculate maximum distance between cmd and sites
  cmed.dist = sf::st_distance(cmed,sites) |> unclass() |>  max()
  
  # 4: create rings - equal area
  rings = fmap::fmap_multi(
                     radius_outer = cmed.dist,
                     ncircles = 12, 
                     geo_centre = cmed, 
                     geo_points = sites, 
                     id_var = "id",
                     sum = "size_ha",
                     output = "data")
  
  } else if(geo_centre == "max") {
    
    # select largest site in settlement system
    max.site = sites %>% filter(size_ha == max(size_ha))
    
    rings = dist_rings(geo_centre = max.site,geo_points = sites,from = 1000, to = 24000, by = 2000 )
    
    
    
  }
  
  # 5: clip rings to survey extent
  rings.clip = sf::st_intersection(rings,survey)
  
  # 6: calculate  per proportions
  rings_df = st_drop_geometry(rings.clip)
  
  rings_df$size.prop = rings_df$sum / sum(rings_df$sum) * 100
  
  rings_df$cum.prop = cumsum(rings_df$size.prop)
  
  # 6: calculate B coefficient
  Bcoeff = ( sum(rings_df$cum.prop) - 650 ) / 550
  
  # 7: return as data frame
  
  b.df = data.frame(
    time = time,
    b.coeff = Bcoeff
  )
  
  return(b.df)
}
