install.packages("tidyverse")

library(sf)
library(kairos)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(janitor)

n_mes_sites_LONG

# package kairos ----

# load data

n_mes_sites = st_read("data/raw/n_mesopotamia/north_mesopotamia_sites.shp") %>% 
  janitor::clean_names()

head(n_mes_sites)

# set geometry to null
st_geometry(n_mes_sites) = NULL

# change column names needed for aoristic() function

n_mes_sites = n_mes_sites %>%
  rename(
    from = start_date,
    to = end_date
  )

##  create aoristic sum not weighted ----

aorist_sum = aoristic(n_mes_sites, step = 100, weight = TRUE)

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

n_mes_aorist_wide = cbind(n_mes_sites,p_df)



# pivot longer

n_mes_aorist_long = n_mes_aorist_wide %>% 
  pivot_longer(
    cols = `-10000_-9900`:`1800_1900`,
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

head(n_mes_aorist_long)

# save to rds format

saveRDS(n_mes_aorist_wide,"data/n_mes_sites_WIDE.rds")

saveRDS(n_mes_aorist_long,"data/n_mes_sites_LONG.rds")
