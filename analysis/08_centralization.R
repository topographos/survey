# PACKAGES ----

library(sf)
library(spatstat)
library(fmap)
library(ggplot2)
library(tmap)
library(mapiso)
library(terra)
library(tidyr)
library(dplyr)
library(sfdep)
library(cowplot)
library(ggspatial)

# FUNCTIONS ----

source("func/mulit_buffer.R")

# DATA ----

# read sites from geopackage
sites = st_read("analysis/derived/surveys.gpkg", layer = "sites_tbs")

# read sites from geopackage
survey = st_read("analysis/derived/surveys.gpkg", layer = "survey_tbs") |> select(code)

# LARGEST SITES ----

max_sites = sites %>% 
  group_by(time_start) %>%
  slice_max(size_ha)

## plot large sites ----

(max.sites.plot = ggplot(max_sites) + 
   geom_point(aes(x = as.factor(time_start), y = size_ha), colour = "#53868B", size = 3) +
   geom_line(aes(x = as.factor(time_start), y = size_ha), colour = "#53868B") +
   #scale_x_continuous(breaks = seq(-3100,700, by = 200)) +
   labs(title ="tbs Survey Largest Site") +
   xlab("Time\n") +                                                  
   ylab("\nSize in Hectars") +
   theme_minimal_hgrid() +
   theme(
     axis.text = element_text(size = 8),
     plot.margin = unit(c(1,1,1,1), units = , "cm"))
)

ggsave("analysis/figures/max_sites_tbs.png", bg = "white")

# RINGS ----


# 1: create sites list split by periods
sites.list = split(sites, sites$time_start)


# 2: 
rings.list = lapply(sites.list, function(x){
  
  sites = x
  
  max.site = sites %>% filter(size_ha == max(size_ha))
  
  rings = dist_rings(geo_centre = max.site,geo_points = sites,from = 0, to = 24000, by = 2000 )
  
  rings.clip = st_intersection(rings,survey)
  
  return(rings.clip)
  
})



Bcoeff.list = lapply(rings.list, function(x){
  
  ring.df = st_drop_geometry(x)
  
  ring.df$size.prop = ring.df$sum  / sum(ring.df$sum ) * 100
  
  ring.df$cum.prop = cumsum(ring.df$size.prop)
  
  Bcoeff = ( sum(ring.df$cum.prop) - 650 ) / 550
  
})

# Convert Beta 
Bcoeff.df = bind_rows(Bcoeff.list)

Bcoeff.df = pivot_longer(Bcoeff.df,cols = everything(), names_to = "time_start", values_to = "beta")

write_csv(Bcoeff.df,"analysis/derived/b_coeff/beta_tbs.csv")
# 7: Map
for (i in 1:length(rings.list)) {
  
  # create a df for each period
  rings = do.call(rbind,rings.list[i])
  
  ring.map = ggplot() + 
    geom_sf(data = survey, fill = "grey80", colour = "grey80") +
    geom_sf(data = rings, fill = NA, colour = "gray40", linetype = "dotted") +
    geom_sf(data = sites[sites$time_start == as.numeric(names(rings.list[i])),],
            aes(
              size =  size_ha,
            ),
            colour = "#53868B") +
    scale_size_continuous(name = "ha",
                          range = c(1,12),
                          breaks = c(1,5,10,20,40,60),
                          limits=c(0,70),
                          guide = guide_legend(
                            direction = "horizontal",
                            nrow = 1,
                            label.position = "top")) +
    annotation_scale(mapping = aes(style = "ticks", location = "br" )) +
    coord_sf(datum = st_crs(survey)) +
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.margin = unit(c(1,1,1,1), units = , "mm"))

  
  # 8: Plot
  
  rings.df = st_drop_geometry(rings)
  
  rings.df$size.prop = rings.df$sum  / sum(rings.df$sum ) * 100
  
  rings.df$cum.prop = cumsum(rings.df$size.prop)
  
  Bcoeff = ( sum(rings.df$cum.prop) - 650 ) / 550
  
  ring.plot = ggplot(rings.df) + 
    geom_line(aes(x = zonal_area, y = size.prop), colour = "#53868B", linewidth = 1) +
    scale_x_continuous(breaks = seq(1,12,1))+
    scale_y_continuous(limits = c(0,100)) +
    labs(
      title = paste0("Centralization - from ", filter(sites, time_start == as.numeric(names(rings.list[i]))) %>% select(time_start) %>% st_drop_geometry() %>%  distinct()),
      subtitle = "equal area rings from median centroid") +
    annotate("text", x = 11, y = 90, label =  paste0("B = ", round(Bcoeff,2))) +
    annotate("text", x = 1, y = 0, label =  "2 km", size = 4) +
    annotate("text", x = 12, y = 0, label =  "24 km", size = 4) +
    xlab("ring\n") +                                                  
    ylab("\n%") +
    theme_minimal_hgrid() +
    theme(
      axis.text = element_text(size = 8),
      plot.subtitle=element_text(face="italic"),
      plot.margin = unit(c(1,1,1,1), units = , "cm"))
  
  
  panel = cowplot::plot_grid(ring.plot, ring.map, nrow = 1)
  
  
  save_plot(filename = paste("analysis/figures/centralization/dist",names(rings.list[i]), ".png", sep = '_'), panel,bg = 'white', base_width = 40, base_height = 15, units = "cm")
  
  # ggsave(ring.plot, file = paste(names(rings.list[i]), ".pdf", sep = ''), scale = 2) 
  
  
  
}
