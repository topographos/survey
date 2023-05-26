# Title:
# Author: Michal Michalski
# Data: 29-04-2023

# PACKAGES ----

library(sf)
library(knitr)
library(tidyverse)
library(kairos)

# FUNCTIONS ----

# function to round the period start or end to nearest half century e.g. 330 BC to 350BC
source("./func/round_any.R")

# SURVEY DATA ----

# read shapefile
n_mes_sites_raw = st_read("./data/raw/north_mesopotamia_sites.shp", quiet = TRUE) %>% 
  janitor::clean_names()

# add column with survey code
n_mes_sites_raw = n_mes_sites_raw %>% 
  mutate(
    code = str_extract(id, "[^_]+")
  )

# ROUND PERIODS ----

sites = n_mes_sites_raw %>% 
  mutate(
    from =round_any(start_date, 50),
    to = round_any(end_date, 50)
  )

# AORISTIC ANALYSIS ----

# create aoristic sum

# create df by dropping geometry
sites.df = st_drop_geometry(sites)

aorist_sum = aoristic(sites.df, start = -3200, stop = 700,step = 50, weight = TRUE)

# extract sum for each time step as a vector - to be ploted on y axis
y = as.vector(aorist_sum)

# create column with mid point of the time step

# 1 - get dates from aoristic object
blocks = get_dates(aorist_sum)

# 2 - get mid point across rows to be ploted on x axis
x = rowMeans(blocks)

# create a data frame
aorist_sum_df = tibble(x,y)

# extract weights
array_p = aorist_sum@p

p_df = as.data.frame(array_p[,,1])

# combine into wide table
aorist_wide_df = cbind(sites.df,p_df)

# pivot into a longer table
aorist_long_df = aorist_wide_df %>% 
  pivot_longer(
    cols = `-3200_-3150`:`650_700`,
    names_to = "time",
    values_to = "prob"
  ) %>% 
  filter(prob > 0) %>% 
  separate(col= time, 
           into=c('time_start', 'time_end'), 
           sep='_',
           remove = FALSE) %>% 
  mutate(
    time = str_replace(time, "_", " to ")
  ) %>% 
  mutate(
    time_start = as.numeric(time_start),
    time_end = as.numeric((time_end))
  )

# save to rds format
saveRDS(aorist_wide_df,"./analysis/derived/sites_WIDE.rds")

saveRDS(aorist_long_df,"./analysis/derived/sites_LONG.rds")

# SF CLASS ----

# read long data 
sites.df = readRDS("./analysis/derived/sites_LONG.rds")

# provide the EPSG code using st_crs()
crs_wgs84 = st_crs(4326) # WGS84 has EPSG code 4326

sites.sf = st_as_sf(sites.df, coords = c("longitude", "latitude"))

st_crs(sites.sf) = crs_wgs84

plot(sites.sf$geometry)

# save sites to geopackage
st_write(sites.sf, "./analysis/derived/surveys.gpkg", layer = "sites")