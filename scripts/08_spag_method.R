# packages ----
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gdistance)
library(rgeos)
install.packages("gdistance")
# clear env 

# import SPAG functions ----

source("func/calc_agr_zones_df.R")
source("func/calc_agr_zones_sf.R")
source("func/calc_coverage_index.R")
source("func/calc_distance_index.R")
source("func/calc_overlap_index.R")
source("func/site_category.R")

# data - load data ----

sites = readRDS("data/tab/njs_ppp.rds")

survey = st_read("data/vect/data.gpkg", layer = "njs_survey") %>% st_as_sfc()

ggplot() +
  geom_sf(data = sites, aes(fill= id)) +
  coord_sf(datum = st_crs(survey)) +
  facet_wrap(vars(period))

# prepare for analysis

# 1: add category column

sites = site_category(sites, "size_ha")

# 2: select the column into right order: id, size_ha, category
sites = sites %>%
  dplyr::select(id,size_ha, period, category) %>%
  relocate(geometry, .after = last_col())

sites_sf = sites %>% filter(period == "Period 1")

# 3: compute agricultural zones

agr_zones_df = calc_agr_zones_df(data = sites, size = "size_ha", h_per_person = 3)

agr_zones_sf = calc_agr_zones_sf(data = sites, size = "size_ha", h_per_person = 3)

plot(agr_zones_sf)

# Coverage Index -----

sites_list = split(sites, sites$period)

i.cov = sapply(sites_list, calc_cov_index, size = "size_ha", category = "category",h_per_person = 3, total = TRUE)

i.cov.df = as.data.frame(i.cov)

i.cov.df = cbind(period = rownames(i.cov.df),i.cov.df)

rownames(i.cov.df) = NULL

# Distance Index ----

calc_dist_index()

i.dist = sapply(sites_list, calc_dist_index, survey = survey, total = TRUE)

i.dist.df = as.data.frame(i.dist)

i.dist.df = cbind(period = rownames(i.dist.df),i.dist.df)

rownames(i.dist.df) = NULL

# Overlap Index ----

i.over = sapply(sites_list, calc_over_index, size = "size_ha", h_per_person = 3, total = TRUE)

i.over.df = as.data.frame(i.over)

i.over.df = cbind(period = rownames(i.over.df),i.over.df)

rownames(i.over.df) = NULL

# SPAG ----

SPAG_TAB = merge(i.cov.df,i.dist.df) %>% 
  merge(i.over.df) %>% 
  mutate(
    i.spag = i.cov * i.dist * i.over,
    period = period
  )

SPAG_long = SPAG_TAB %>% 
  tidyr::pivot_longer(!period, names_to = "index", values_to = "value")

SPAG_SC = SPAG %>% 
  mutate(
    i.cov.sc = scales::rescale(SPAG$i.cov, to = c(0,1)),
    i.dist.sc = scales::rescale(SPAG$i.dist, to = c(0,1)),
    i.over.sc = scales::rescale(SPAG$i.over, to = c(0,1))
  )

# SPAG OLD ----

survey_sp = as(survey,"Spatial")

sites_sp = as(sites, "Spatial")

sites_test = sites %>%
  st_drop_geometry() %>%
  transmute(
    lng = st_coordinates(sites)[,1],
    lat = st_coordinates(sites)[,2],
    pop = round(size_ha * 100),
    category = category,
    period = period
  )

sites_test_list = split(sites_test, sites_test$period)
sites_df_list = split(sites_df, sites_df$period)

class(sites)
class(sites_df)

spag_result = list()

for(i in 1:length(sites_test_list)){

spag = SPAGSingle(sites_test_list[[i]],survey_sp, totalOnly = TRUE,theoreticalSample = 80, empiricalSample = 80, numberOfSamples = 1000)

spag_result[[i]] = spag

}

spag_df = do.call(rbind, spag_result)

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
