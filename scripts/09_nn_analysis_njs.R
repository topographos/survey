# r packages ----
library(sf)
library(spatstat)

# read data
sites = readRDS("data/tab/njs_sites_LONG_SF.rds")

# nn
ppp = as.ppp(sites)

marks(ppp) = as.factor(sites$time_start)

ppp_list = split(ppp)

sites_list = split(sites, sites$time_start)

for (i in 1:length(sites_list)){
  
  nn = sapply(ppp_list, nndist)
  
  sites_list[[i]]$dist = nn[[i]]
  
}

# combine into one list

combined_list = do.call("rbind",sites_list)

combined_list = combined_list %>% relocate(geometry, .after = last_col())

rownames(combined_list) = NULL

saveRDS(combined_list,"data/tab/njs_sites_LONG_SF.rds")



