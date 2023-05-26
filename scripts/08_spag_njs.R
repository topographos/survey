# packages ----
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(spatstat)

# clear env 

# import SPAG functions ----

source("func/calc_agr_zones_df.R")
source("func/calc_agr_zones_sf.R")
source("func/calc_coverage_index.R")
source("func/calc_distance_index.R")
source("func/calc_overlap_index.R")
source("func/calc_nn_index.R")
source("func/site_category.R")

# data - load dataset ----

st_layers("./data/vect/njs.gpkg")
st_layers("./data/vect/data.gpkg")

sites_raw = st_read("./data/vect/njs.gpkg", layer = "sites")

survey = st_read("./data/vect/data.gpkg", layer = "njs_survey")


# prepare for analysis

# 1: add category column

sites = site_category(sites_raw, "size_ha")

# 2: select the column into right order: id, size_ha, category
sites = sites %>%
  #dplyr::filter(period == "Iron Age") %>%
  dplyr::select(id,size_ha, time_start, category) %>%
  relocate(geometry, .after = last_col())

# 2.1 Make a subset for one period 

sites_ia = sites %>%
  dplyr::filter(period == "Iron Age") %>%
  dplyr::select(id,size_ha, period, category) %>%
  relocate(geom, .after = last_col())


# 3: compute agricultural zones

agr_zones_sf = calc_agr_zones_sf(data = sites, size = "size_ha")

# coverage index -----

## single period ----
i.cov.ia = calc_cov_index(data = sites_ia,
                          size = "size_ha", 
                          h_per_person = 2, 
                          category = "category", 
                          total = FALSE)

##  multipe periods, loop over a list of data frames

sites_list = split(sites, sites$time_start)

i.cov = sapply(sites_list, calc_cov_index, size = "size_ha", person_per_h = 100, h_per_person = 1)

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

# nn index

i.nn = sapply(sites_list, calc_nn_index, survey = survey)

i.nn.df = as.data.frame(i.nn)

i.nn.df = cbind(period = rownames(i.nn.df),i.nn.df)

rownames(i.nn.df) = NULL



# overlap index ----

# single period

i.cov.ia = calc_over_index(data = sites_ia, size = "size_ha", h_per_person = 3, total = TRUE)

# multiple period

# sites_list = split(sites, sites$period)

i.over = sapply(sites_list, calc_over_index, size = "size_ha", person_per_h = 200, h_per_person = 1)

i.over.df = as.data.frame(i.over)

i.over.df = cbind(period = rownames(i.over.df),i.over.df)

rownames(i.over.df) = NULL

# SPAG ----

SPAG = merge(i.cov.df,i.dist.df) %>% 
  merge(i.over.df) %>% 
  merge(i.nn.df) %>% 
  mutate(
    i.spag = i.cov * i.dist * i.over,
    i.spag.nn = i.cov * i.dist * i.over * i.nn,
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

spag = SPAG_long %>% filter(index %in% c("i.spag", "i.spag.nn", "i.over"))

ggplot() +
  geom_line(data = spag, mapping = aes(x = period, y = value, color = index)) +
  geom_point(data = spag, mapping = aes(x = period, y = value, color = index)) +
 # scale_color_manual(labels = c("coverage", "distance", "overlap","total"),
  #                     values = c("#000000", "#046C9A", "#C93312", "#FAEFD1")) +
  scale_x_continuous(n.breaks = 30) +
  labs(title = "LCP Survey from 3100BC to 650AC",
       subtitle = "0 (agglomeration) to 1 (unifrom distribution)",
       caption = "low distance values mean sites are in spatial proximity, high values equal distribution
                  low overlap values mean sites  overlap and agglomerate, high values reflect uniform distribution "
       ) + 
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


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
