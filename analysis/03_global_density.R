# PACKAGES ----

library(sf)
library(tidyr)
library(dplyr)
library(stringr)
library(forcats)
library(ggplot2)
library(cowplot)
library(scales)


# DATA ----

st_layers("analysis/derived/surveys.gpkg")

sites = st_read("analysis/derived/surveys.gpkg", layer = "sites_ths")

survey = st_read("analysis/derived/surveys.gpkg", layer = "survey_tbs")%>% select(code)

survey.area = st_area(survey) %>% units::set_units(km^2)

# TIDY ---

# drop geometry
sites.df = st_drop_geometry(sites)

# rename values in change column
sites.df = sites.df %>% 
  mutate(
    change = case_when(change == "backward" ~ "old",
                       change == "added" ~ "new")
  )

# summarize by time_start and period
sites.df.sum = sites.df %>% 
  group_by(time_start, change) %>% 
  summarise(
    n = n()
  )

# site density

global.density = sites.df %>% 
  group_by(time_start) %>% 
  summarise(
    n = n(),
    area = sum(size_ha)
  ) %>% 
  mutate(
    count.dens = unclass(n / survey.area),
    area.dens = unclass(area / survey.area)
  ) %>% 
  mutate(
    count.dens.r = scales::rescale(count.dens, to = c(1,2)),
    area.dens.r = scales::rescale(area.dens, to = c(1,2))
    
  )



# PLOTS ----

# plot site count
ggplot(data = sites.df.sum, aes(x = as.factor(time_start), y = n, fill = change)) +
  geom_col() +
  geom_text(aes(label = n), position = position_stack(vjust = .5),size = 4) +
  scale_y_continuous(name = "Sites Count") +
  scale_x_discrete(name = "Year") +
  scale_fill_manual(name = "Sites continuity",values = c("#15aabf", "#ced4da")) +
  labs(title = "tbs Survey - settlement system transitions") +
  theme_minimal_hgrid() +
  theme(
    legend.position = "top",
    axis.text = element_text(size = 8),
    plot.subtitle=element_text(face="italic"),
    plot.margin = unit(c(1,1,1,1), units = , "cm"))

# density

ggsave("analysis/figures/site_count_tbs.png", bg = "white")

colors <- c("Sites Count Density" = "#82c91e", "Sites Area Density" = "#e64980")

ggplot(data = global.density ) +
  geom_point(aes(x = as.factor(time_start), y = count.dens.r, color = "Sites Count Density" ), size = 3) +
  geom_line(aes(x = as.factor(time_start), y = count.dens.r, group = 1), colour = "#82c91e") +
  
  geom_point(aes(x = as.factor(time_start), y = area.dens.r, color = "Sites Area Density"), size = 3) +
  geom_line(aes(x = as.factor(time_start), y = area.dens.r, group = 1), colour = "#e64980") +
  
  scale_color_manual(name = "", values = colors) +
  
  labs(title = "tbs  - global settlement densities") +
  scale_y_continuous(name = "Normalized Density / km2") +
  scale_x_discrete(name = "Year") +
  theme_minimal_hgrid() +
  theme(
    legend.position = "top",
    axis.text = element_text(size = 8),
    plot.subtitle=element_text(face="italic"),
    plot.margin = unit(c(1,1,1,1), units = , "cm"))

ggsave("analysis/figures/global_density_tbs.png", bg = "white")
