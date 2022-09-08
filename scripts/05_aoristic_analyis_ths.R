install.packages("tidyverse")

library(sf)
library(kairos)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)


# package kairos ----

# load data
ths_sites = readRDS("data/ths_sites_2500_500_BC_SF.rds")

n_mes_sites = st_read("data/n_mesopotamia/north_mesopotamia_sites.shp")

head(n_mes_sites)

# set geometry to null
st_geometry(ths_sites) = NULL

# change column names needed for aoristic() function

ths_sites = ths_sites %>%
  rename(
    from = start_date,
    to = end_date
  )

##  create aoristic sum not weighted ----

aorist_sum = aoristic(ths_sites, start = -2300, stop = -300, step = 100, weight = TRUE)

# extract sum for each time step as a vector - to be ploted on y axis

y = as.vector(aorist_sum)


# create column with mid point of the time step

# 1 - get dates from aoristic object
blocks = get_dates(aorist_sum)

# 2 - get mid point across rows to be ploted on x axis
x = rowMeans(blocks)

# create a data frame
aorist_sum_df = tibble(x,y)

## plot aoristic sum ----

## line

ggplot(aorist_sum_df, aes(x=x,y=y)) +
  geom_line()
  
## bar
  ggplot(aorist_sum_df_a, aes(x=x_a,y=y_a)) +
    geom_col()

## extract weights ----

array_p = aorist_sum@p

p_df = as.data.frame(array_p[,,1])

ths_aorist_wide = cbind(ths_sites,p_df)

# pivot longer

select(ths_aorist_wide, `-2300_-2200` : `-2200_-2100`)

ths_aorist_long = ths_aorist_wide %>% 
  pivot_longer(
    cols = `-2300_-2200`:`-400_-300`,
    names_to = "century",
    values_to = "aorist_prob"
  ) %>% 
  filter(aorist_prob > 0) %>% 
  separate(col=century, 
           into=c('cent_start', 'century_end'), 
           sep='_',
           remove = FALSE) %>% 
  mutate(
    century = str_replace(century, "_", " to ")
  )

head(ths_aorist_long)

# save to rds format

saveRDS(ths_aorist_wide,"data/ths_sites_2500_500_BC_WIDE.rds")

saveRDS(ths_aorist_long,"data/ths_sites_2500_500_BC_LONG.rds")
