library(sf)
library(dplyr)
library(tidyr)

# read data ----

# sites
sites = readRDS("data/tab/lcp_sites_LONG_SF.rds") %>% arrange(time_start)



# survey extent
survey = st_read("data/vect/data.gpkg", layer = "lcp_survey") %>% st_geometry()

# quick plot
plot(survey)
plot(sites$geometry, add = TRUE)

# voronoi ----
voronoi <- sites %>%  # consider the master points
  st_geometry() %>% # ... as geometry only (= throw away the data items)
  st_union() %>% # unite them ...
  st_voronoi() %>% # ... and perform the voronoi tessellation
  st_collection_extract() %>% # select the polygons
  st_sf(crs = 32637) %>% # set metric crs
  st_join(sites) %>% # & re-connect the data items
  st_intersection(survey)# limit to boundaries

sites_list = split(sites, sites$time_start)

result = list()

for(i in 1:length(sites_list)){
  
  voronoi <- sites_list[[i]] %>%  # consider the master points
    st_geometry() %>% # ... as geometry only (= throw away the data items)
    st_union() %>% # unite them ...
    st_voronoi() %>% # ... and perform the voronoi tessellation
    st_collection_extract() %>% # select the polygons
    st_sf(crs = 32637) %>% # set metric crs
    st_join(sites_list[[i]]) %>% # & re-connect the data items
    st_intersection(survey)
  
  result[[i]] = voronoi
  
}

sites_list$`650`
# spatial gini

library(sfdep)





sg_list = list()

nb_list = list()

for(i in 1:length(result)){

nb <- st_contiguity(result[[i]])

x <- result[[i]]$size_ha

sg = spatial_gini(x, nb)

sg_list[[i]] = sg

}


names(sites_list)

sg = do.call(rbind,sg_list)

sg$time_start = names(sites_list)

sg$time_start = as.numeric(sg$time_start)

head(sg)

sg_long = sg %>% 
  tidyr::pivot_longer(-time_start, names_to = "index", values_to = "value")

# pivot longer


ggplot() +
  geom_line(data = sg, mapping = aes(x = time_start, y = G, group = 1))

ggplot() +
  geom_line(data = sg_long, mapping = aes(x = time_start, y = value, color = index)) +
  #geom_point(data = sg_long, mapping = aes(x = time_start, y = value), color = "#999999") +
  scale_color_manual( labels = c("Gini Index", "neighbor composition", "non-neighbor composition","Spatial Gini"),
                      values = c("#000000", "#046C9A", "#C93312", "#FAEFD1")) +
  scale_x_continuous(n.breaks = 20) +
  labs(title = "LCP Survey from 3100BC to 650AC",
       subtitle = "0 is perfect equality and 1 is perfect inequality",
       caption = "add"
  ) +
  theme_ipsum()





  theme_ipsum()