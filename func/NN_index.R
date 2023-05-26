library(sf)
library(dplyr)
library(spatstat)
library(tmap)
library(sfdep)

sites = st_read("./data/vect/njs.gpkg", layer = "sites")

survey = st_read("data/vect/data.gpkg", layer = "njs_survey")

# nn
ppp = as.ppp(sites)

marks(ppp) = as.factor(sites$time_start)

ppp_list = split(ppp)

sites_list = split(sites, sites$time_start)

for (i in 1:length(sites_list)){
  
  nn = sapply(ppp_list, nndist)
  
  sites_list[[i]]$dist = nn[[i]]
  
}

# 

# combine into one list

combined_list = do.call("rbind",sites_list)

combined_list = combined_list %>% relocate(geom, .after = last_col())

rownames(combined_list) = NULL

sites = combined_list

st_write(sites, "./data/vect/njs.gpkg", layer = "sites_nn",overwrite = TRUE )

#####

calc_nn_index(data = sites_2550_BC, nn_dist = "dist", survey = survey)


