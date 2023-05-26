# Title:
# Author: Michal Michalski
# Data: 01-05-2023
# jaai

# PACKAGES ----

library(sf)
library(tidyverse)
library(tidyr)

# FUNCTIONS ----

source("func/time_block_diffs.R")

# DATA ----

# read all sites (step from 01_aoristic.R)
sites  = readRDS("analysis/derived/sites_LONG.rds")

# select survey by code: "JAA" "jaa" "jaa" "SS"  "jaa" "jaa"

sites = sites |> filter(code == "JAA")

# 1: count sites by time block
sites_count = sites %>% 
  mutate(time_start = factor(time_start)) %>% 
  st_drop_geometry() %>% 
  group_by(time_start) %>% 
  summarise(total = n())

# 2: split data into list
s <- split(sites$id, sites$time_start)

# 3: run function - IMPORTANT change sequence 2:n-1
diffs_list <- setNames(lapply(seq_along(s)[2:(length(s)-1)], time_block_diffs), names(s)[2:(length(s)-1)])

# 4: create dataframes
df = setNames(stack(diffs_list), c("id", "time_start"))

df= tidyr::separate(df,id, c("change", "id"), sep = " ")

df = df %>% filter(id !="")

df = df[,c(3,2,1)]

df_wide = df %>%
  add_count(id, time_start,change) %>%
  pivot_wider(id_cols = -id,names_from = change, values_from = n, values_fn = sum)

# join summary table with total number of settlements per time block
df_wide = left_join(df_wide,sites_count)

# calculate number of new sites and per change
df_wide  = df_wide  %>% 
  mutate(new_sites= (total - lag(total)), # new  cases per time block
         pct_change = new_sites / lag(total) * 100) # percentage change

# save
write_csv(df_wide,"analysis/derived/continuity/jaa_continuity.csv")

# PREPARE SPATIAL DATASET ----

# select periods where percentage change is greater or smaller then 0

transitions = df_wide |> filter(pct_change != 0)

#|> filter(time_start != -3100)

# save
write_csv(transitions,"analysis/derived/continuity/jaa_transitions.csv")

df.for.join = df %>% 
  filter(change != "forward" & change != "removed") %>% 
  mutate(time_start = as.numeric(as.character(time_start))) |> 
  filter(time_start %in% unique(transitions$time_start))

sf.for.join = sites |> 
  filter(from %in% unique(transitions$time_start))
  
join = left_join(df.for.join, sites)


# make selection
sites.df = join |> select(id, size_ha, start_date, end_date, code, time_start, change, longitude, latitude)

# count(sites.df, time_start)

# provide the projected (in m) EPSG code for survey 
crs_WGS84 = st_crs(4326)

crs_UTM = st_crs(32637)

sites.sf = st_as_sf(sites.df, coords = c("longitude", "latitude"))

st_crs(sites.sf) = crs_WGS84

# transfrom to UTM

sites.sf = st_transform(sites.sf, crs_UTM)

plot(sites.sf$geometry)

# save sites to geopackage

st_write(sites.sf, "./analysis/derived/surveys.gpkg", layer = "sites_jaa",delete_layer = TRUE)
              