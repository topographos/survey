calc_nn_index = function(data, survey){
  
  # emprical
  ppp_emp = as.ppp(data)
  
  emp_ann = mean(nndist(ppp_emp))
  
  # theoretical
  n = nrow(data)
  
  xy_theoretical = sf::st_sample(survey, n, type = "regular", offset = c(0,0)) %>% st_as_sf()
  
  ppp = as.ppp(xy_theoretical)
  
  theor_ann = mean(nndist(ppp))
  
  # index
  nn_index = emp_ann / theor_ann
  
  return(nn_index)
}

