library(sf)
library(nngeo)
library(dplyr)
library(ggplot2)
# DATA

# DATA ----

sites = st_read("./data/vect/njs.gpkg", layer = "sites")

survey = st_read("./data/vect/data.gpkg", layer = "njs_survey") |> select(code)

# LARGEST SITES ----

max_sites = sites %>% 
  #group_by(time_start) %>%
  slice_max(size_ha)

## plot large sites ----

(max.sites.plot = ggplot(max_sites) + 
   geom_point(aes(x = time_start, y = size_ha)) +
   geom_line(aes(x = time_start, y = size_ha)) +
   theme_bw()
) 

# SINGLE PERIOD

# 9000
sites_900BC = sites |> dplyr::filter(time_start == -900)

max.site_900BC = max_sites |> filter(time_start == -900)

n = st_nn(sites_900BC, max.site_900BC, k = 1,  returnDist = TRUE)

dists = sapply(n[[2]], "[", 1)

sites_900BC$dist = dists

sites_df = filter(sites_900BC, dist > 0)

nominator = sum(sites_df$dist * sites_df$size_ha)

denominator = sum(sites_900BC$size_ha)

nominator / denominator


# 2550
sites_2550BC = sites |> dplyr::filter(time_start == -2550)

max.site_2550BC = max_sites |> filter(time_start == -2550)

n = st_nn(sites_2550BC, max.site_2550BC, k = 1,  returnDist = TRUE)

dists = sapply(n[[2]], "[", 1)

sites_2550BC$dist = dists

sites_df = filter(sites_2550BC, dist > 0)

nominator = sum(sites_df$dist * sites_df$size_ha)

denominator = sum(sites_2550BC$size_ha)

nominator / denominator
