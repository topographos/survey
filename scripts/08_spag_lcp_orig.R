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

source("./func/site_category.R")
source("./func/SPAG.R")

# DATA ----

# load survey extent as sf
survey = st_read("./data/vect/data.gpkg", layer = "lcp_survey") %>% st_as_sfc()

# create sp
survey_sp = as(survey,"Spatial")

# load site data
sites = readRDS("./data/tab/point_pattern.rds")

# prepare for analysis

# 1: add category column

sites = site_category(sites, "size_ha")

# 2: select the column into right order: id, size_ha, category
sites = sites %>%
  #dplyr::filter(period == "Iron Age") %>%
  dplyr::select(id,size_ha, time_start, category) %>%
  relocate(geometry, .after = last_col())

sites_df = sites %>%
  st_drop_geometry() %>%
  transmute(
    lng = st_coordinates(sites)[,1],
    lat = st_coordinates(sites)[,2],
    pop = round(size_ha * 100),
    category = category,
    period = as.factor(period)
  ) %>% 
  as.data.frame()

# create circles

circles = calcCircles(survey_sp, sites_df, totalOnly = TRUE)

st_as_sf(circles$Total)

# split df by time_block

sites_df_list = split(sites_df, sites_df$period)

spag_results = list()

for(i in 1:length(sites_df_list)){
  
  spag = SPAGSingle(sites_df_list[[i]],survey_sp, totalOnly = TRUE, numberOfSamples = 1000)
  
  spag_results[[i]] = spag
  
}

spag_df = do.call(rbind, spag_results) %>% 
  as.data.frame()

spag_df$period = as.numeric(as.character(unique(sites_df$period)))

spag_df$IDist = as.numeric(spag_df$IDist)

spag_df$IOver = as.numeric(spag_df$IOver)

spag_df$ISPAG = as.numeric(spag_df$ISPAG)

spag_df$ICov = as.numeric(spag_df$ICov)



SPAG_long = spag_df %>% 
  dplyr::select(-categories) %>% 
  tidyr::pivot_longer(cols = starts_with("I"), names_to = "index", values_to = "value")




# SPAG PLOT ----

library(hrbrthemes)

pal = wesanderson::wes_palette("Darjeeling2")

print(pal)

ggplot() +
  geom_line(data = SPAG_long, mapping = aes(x = period, y = value, color = index)) +
  #geom_point(data = SPAG_long, mapping = aes(x = period, y = value)) +
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
