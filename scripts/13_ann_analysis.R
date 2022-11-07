library(sf)
library(dplyr)
library(ggplot2)
library(scales)

# NJS ----

# data

sites_njs = readRDS("data/tab/njs_sites_LONG_SF.rds")

head(sites_njs)

ann_df = sites_njs %>% 
  group_by(time_start) %>% 
  summarise(
    ann = mean(dist)
  )

ann_df = ann_df %>% 
  mutate(
    ann_sc = scales::rescale(ann_df$ann, to = c(0,1))
  ) %>% 
  st_drop_geometry()

# plot
ggplot() + 
  geom_line(data = ann_df, mapping = aes(x = time_start, y = ann_sc))
  
ggplot() +
  geom_line(data = ann_df, mapping = aes(x = time_start, y = ann))

# LCP ----

sites_lcp = readRDS("data/tab/lcp_sites_LONG_SF.rds")

head(sites_lcp)

ann_df = sites_lcp %>% 
  group_by(time_start) %>% 
  summarise(
    ann = mean(dist)
  )

ann_df = ann_df %>% 
  mutate(
    ann_sc = scales::rescale(ann_df$ann, to = c(0,1))
  ) %>% 
  st_drop_geometry()

# plot
ggplot() + 
  geom_line(data = ann_df, mapping = aes(x = time_start, y = ann_sc))

ggplot() +
  geom_line(data = ann_df, mapping = aes(x = time_start, y = ann))


