calc_cov_index = function(data, size, h_per_person,category, total){

  if (!total) {

    agr_area = calc_agr_zones_df(data = data, size = size, h_per_person)

    denominator.coverage = sum(agr_area[,4]) # total area

    category = unique(agr_area[[category]])

    i.cov <- sapply(category, function(x){
      return (sum(agr_area[agr_area[,3]==x,4])/denominator.coverage)

    })
  

    return(c(i.cov))
      


  } else {

    return(c(1))
  }

}
