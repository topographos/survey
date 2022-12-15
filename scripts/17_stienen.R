library(sf)
library(dplyr)
library(spatstat)
library(tmap)
library(sfdep)
# read data
sites = readRDS("./data/tab/point_pattern.rds")

sites_njs = readRDS("./data/tab/njs_sites_LONG_SF.rds") %>% 
  filter(time_start == "-2300")

plot(sites_njs)

# nn
ppp = as.ppp(sites)

marks(ppp) = as.factor(sites$period)

ppp_list = split(ppp)

sites_list = split(sites, sites$period)

for (i in 1:length(sites_list)){
  
  nn = sapply(ppp_list, nndist)
  
  sites_list[[i]]$dist = nn[[i]]
  
}

# combine into one list

combined_list = do.call("rbind",sites_list)

combined_list = combined_list %>% relocate(geometry, .after = last_col())

rownames(combined_list) = NULL

# final sites

sites = combined_list

sites_stienen = st_buffer(sites, dist = sites$dist / 2)

sites_njs_stienen = st_buffer(sites_njs, dist = sites_njs$dist / 2)

plot(sites_njs_stienen$geometry)

# analysis

hex = sites_stienen %>% filter(period == "Hexagonal")

hex$area = st_area(hex)

std_distance(hex)

plot(hex$geometry)
plot(st_centroid(hex), add = TRUE)

clust = sites_stienen %>% filter(period == "Clustered")

std_distance(clust)

clust$area = st_area(clust)

plot(clust$geometry)
plot(st_centroid(clust), add = TRUE)
  
rand = sites_stienen %>% filter(period == "Random")

std_distance(rand)

rand$area = st_area(rand)

hist(rand$area)

plot(rand$geometry)
plot(st_centroid(rand), add = TRUE)

sum(mean(dist)) / sum(mean(dist))

sum(st_area(rand)) / sum(st_area(hex))

sum(clust$dist) / sum(hex$dist)

sum(st_area(sites_njs_stienen)) / sum(st_area(hex))

