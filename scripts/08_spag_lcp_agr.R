# packages ----
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sp)
library(gdistance)
library(maptools)
library(rgeos)

# clear env 

# import SPAG functions ----

source("func/calc_agr_zones_df.R")
source("func/calc_agr_zones_sf.R")
source("func/calc_coverage_index.R")
source("func/calc_distance_index.R")
source("func/calc_overlap_index.R")
source("func/site_category.R")

# data - load dataset ----

st_layers("data/vect/data.gpkg")


sites_raw = readRDS("data/tab/point_pattern.rds")

plot(sites_raw)

survey = st_read("data/vect/data.gpkg", layer = "njs_survey")


# prepare for analysis

# 1: add category column

sites = site_category(sites_raw, "size_ha")

# 2: select the column into right order: id, size_ha, category
sites = sites %>%
  dplyr::select(id,size_ha, period, category) %>%
  relocate(geometry, .after = last_col())

# 2.1 Make a subset for one period 

sites_ia = sites %>%
  dplyr::filter(period == "Random") %>%
  dplyr::select(id,size_ha, period, category) %>%
  relocate(geometry, .after = last_col())


# 3: compute agricultural zones

agr_zones_df = calc_agr_zones_df(data = sites_ia, size = "size_ha", h_per_person = 2)

agr_zones_sf = calc_agr_zones_sf(data = sites, size = "size_ha", h_per_person = 2)

plot(agr_zones_sf$geometry)

# coverage index -----

## single period ----
i.cov.ia = calc_cov_index(data = sites_ia,
                          size = "size_ha", 
                          h_per_person = 2, 
                          category = "category", 
                          total = FALSE)

##  multipe periods, loop over a list of data frames

sites_list = split(sites, sites$period)

i.cov = sapply(sites_list, calc_cov_index, size = "size_ha", category = "category",h_per_person = 2, total = TRUE)

i.cov.df = as.data.frame(i.cov)

i.cov.df = cbind(period = rownames(i.cov.df),i.cov.df)

rownames(i.cov.df) = NULL

# distance index ----

# single period

i.dist.ia = calc_dist_index(data = sites_ia, survey = survey, total = TRUE)

# multiple period

#sites_list = split(sites, sites$period)

i.dist = sapply(sites_list, calc_dist_index, survey = survey, total = TRUE)

i.dist.df = as.data.frame(i.dist)

i.dist.df = cbind(period = rownames(i.dist.df),i.dist.df)

rownames(i.dist.df) = NULL

# overlap index ----

# single period

i.cov.ia = calc_over_index(data = sites_ia, size = "size_ha", h_per_person = 3, total = TRUE)

# multiple period

# sites_list = split(sites, sites$period)

i.over = sapply(sites_list, calc_over_index, size = "size_ha", h_per_person = 3, total = TRUE)

i.over.df = as.data.frame(i.over)

i.over.df = cbind(period = rownames(i.over.df),i.over.df)

rownames(i.over.df) = NULL

# SPAG ----

SPAG = merge(i.cov.df,i.dist.df) %>% 
  merge(i.over.df) %>% 
  mutate(
    i.spag = i.cov * i.dist * i.over,
    period = as.numeric(period)
  )

SPAG_long = SPAG %>% 
  tidyr::pivot_longer(!period, names_to = "index", values_to = "value")

SPAG_SC = SPAG %>% 
  mutate(
    i.cov.sc = scales::rescale(SPAG$i.cov, to = c(0,1)),
    i.dist.sc = scales::rescale(SPAG$i.dist, to = c(0,1)),
    i.over.sc = scales::rescale(SPAG$i.over, to = c(0,1))
  )



# SPAG PLOT ----

library(hrbrthemes)

pal = wesanderson::wes_palette("Darjeeling2")

print(pal)

ggplot() +
  geom_line(data = SPAG_long, mapping = aes(x = period, y = value, color = index)) +
  geom_point(data = SPAG_long, mapping = aes(x = period, y = value)) +
  scale_color_manual(labels = c("coverage", "distance", "overlap","total"),
                       values = c("#000000", "#046C9A", "#C93312", "#FAEFD1")) +
  scale_x_continuous(n.breaks = 20) +
  labs(title = "LCP Survey from 3100BC to 650AC",
       subtitle = "0 (agglomeration) to 1 (unifrom distribution)",
       caption = "low distance values mean sites are in spatial proximity, high values equal distribution
                  low overlap values mean sites  overlap and agglomerate, high values reflect uniform distribution "
       ) + 
  theme_ipsum()

ggplot() +
  geom_line(data = SPAG, mapping = aes(x = period, y = i.spag, group = 1)) +
  geom_line(data = SPAG, mapping = aes(x = period, y = i.dist, group = 2), color = "red") +
  geom_point(data = SPAG, mapping = aes(x = period, y = i.dist, group = 2), color = "red") +
  geom_line(data = SPAG, mapping = aes(x = period, y = i.over, group = 2), color = "blue") +
  geom_point(data = SPAG, mapping = aes(x = period, y = i.over, group = 2), color = "blue") +
  scale_x_continuous(n.breaks = 20) +
  theme_ipsum()

sites_count = sites_raw %>% 
  group_by(time_start) %>% 
  count()
