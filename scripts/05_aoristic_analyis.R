# LIBRARIES ----

library(sf)
library(kairos)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(janitor)

# LOAD EXTRA FUNCTION ----

source("func/round_any.R")


# LOAD DATA ----

# read shapefile
n_mes_sites_raw = st_read("./data/raw/north_mesopotamia_sites.shp") %>% 
  janitor::clean_names()

# add column with survey code
n_mes_sites_raw = n_mes_sites_raw %>% 
  mutate(
   code = str_extract(id, "[^_]+")
  )

# select sites from one survey to analyse
sites_njs = n_mes_sites_raw %>% filter(code == "LCP")

# INTEGRITY CHECKS ----

# set geometry to null
sites_njs = st_drop_geometry(sites_njs)

# check periods 
summary_periods = sites_njs %>% 
  group_by(start_date, end_date) %>% 
  count()

# sum
sum(summary_periods$n)

# create column for aoristic analysis
# round the periods
sites_njs = sites_njs %>% 
  mutate(
    from =round_any(start_date, 50),
    to = round_any(end_date, 50)
  )

# check periods rounded 
summary_periods_rounded = sites_njs %>% 
  group_by(from, to) %>% 
  count()

# bind two df with periods
periods = cbind(summary_periods, summary_periods_rounded)

# AORISTIC ----

# create aoristic sum ----
aorist_sum = aoristic(sites_njs, start = -3100, stop = 700,step = 50, weight = TRUE)

# create aoristic roc ----
roc_weighted = kairos::roc(aorist_sum, n = 2)

kairos::plot(roc_weighted)

kairos::autoplot(aorist_sum)

# extract sum for each time step as a vector - to be ploted on y axis
y = as.vector(aorist_sum)

# create column with mid point of the time step

# 1 - get dates from aoristic object
blocks = get_dates(aorist_sum)

# 2 - get mid point across rows to be ploted on x axis
x = rowMeans(blocks)

# create a data frame
aorist_sum_df = tibble(x,y)

# plot aoristic sum ----

## line
ggplot(aorist_sum_df, aes(x=x,y=y)) +
  geom_line()
  
## bar
  ggplot(aorist_sum_df, aes(x=x,y=y)) +
    geom_col()

# extract weights ----
array_p = aorist_sum@p

p_df = as.data.frame(array_p[,,1])

# combine into wide table
aorist_wide_df = cbind(sites_njs,p_df)

# pivot into a longer table
aorist_long_df = aorist_wide_df %>% 
  pivot_longer(
    cols = `-3100_-3050`:`650_700`,
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
saveRDS(aorist_wide_df,"./data/tab/lcp_sites_WIDE.rds")

saveRDS(aorist_long_df,"./data/tab/lcp_sites_LONG.rds")


