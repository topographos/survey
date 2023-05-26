# packages

library(sf)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(magick)
library(gganimate)
library(gifski)

# DATA ----

# read sites from geopackage
sites = st_read("analysis/derived/surveys.gpkg", layer = "sites_tbs")

# split the sites into list by time attribute - for multiple histograms
time_list = split(sites, sites$time_start)

time_list[[1]]


# RANK - SIZE PLOT ----

# single graph ----

(rank_size = ggplot(data = sites) +
  geom_line(aes(x = log(1:length(sites$size_ha)), y = log(sort(sites$size_ha,TRUE))), color = "#53868B", size = 1) +
  geom_point(aes(x = log(1:length(sites$size_ha)), y = log(sort(sites$size_ha,TRUE))), shape= 22, fill = "#53868B", color = "white", size = 3 ) +
  labs(title = paste0("tbs Survey - ", unique(sites$period) ),
       subtitle = "Rank-Size Graph")+
  ylab("Log Size in Hectars\n") +                                                  
  xlab("\n Log Rank") +
  facet_wrap(vars(time_start)) +
  geom_line(aes(x = log(1:length(sites$size_ha)), y = log(tbs_ia_zipf)), linetype = "dashed", color = "#F4A460") +
  theme_cowplot())

(histogram = ggplot(data = sites, mapping = aes(x = size_ha)) +
  geom_histogram(binwidth = 1, boundary = 0, closed = "left", 
                 fill = "#53868B", color = "white") +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0,max(floor(sites$size_ha)), by = 1)) +
  labs(title = paste0(unique(sites$code)," Survey ", unique(sites$time_start) ),
       subtitle = "Site-Size Histogram"
       )+
  ylab("Count\n") +                                                  
  xlab("\n Site Size in Hectars") +
  facet_wrap(vars(time_start)) +
  theme_minimal_hgrid() +
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    plot.margin = unit(c(1,1,1,1), units = , "mm"))
    )

# save 
ggsave(histogram, file = "analysis/derived/figures/histogram/panel_histogram_tbs.png", width = 10, height = 8, bg = 'white')

# multiple graphs ----



for (i in 1:length(time_list)) {
  # create a data frame for each species
  sites = as.data.frame(time_list[[i]])
  # create an object that holds start date for time period
  time = unique(sites$time_start)
  # make the plots 
  histogram = ggplot(data = sites, mapping = aes(x = size_ha)) +
      geom_histogram(binwidth = 1, boundary = 0, closed = "left", 
                     fill = "#53868B", color = "white") +
      geom_rug() +
      geom_vline(aes(xintercept = median(sites$size_ha)),                       
                 colour = "red", linetype = "dashed") +
      scale_x_continuous(limits = c(0, 45), breaks = seq(0,46, by = 1)) +
      labs(title = paste0(unique(sites$code)," Survey ", unique(sites$time_start) ),
           subtitle = "Site-Size Histogram"
           )+
      ylab("Count\n") +                                                  
      xlab("\n Site Size in Hectars") +
      theme_minimal_hgrid() +
      theme(
        axis.text = element_text(size = 8),
        plot.margin = unit(c(1,1,1,1), units = , "cm"))
  
  ggsave(histogram, file = paste0("analysis/derived/figures/histogram/histogram_tbs_", time,".png"), width = 10, height = 8, bg = 'white')
  
}

# animation ----

histogram_gif = sites %>% 
  group_by(time_start) %>% 
  mutate(
    median_size = median(size_ha)
  ) %>% 
  ggplot(mapping = aes(x = size_ha)) +
  geom_histogram(binwidth = 1, boundary = 0, closed = "left", 
                 fill = "#53868B", color = "white") +
  geom_rug() +
  geom_vline(aes(xintercept = median_size),                       
            colour = "red", linetype = "dashed") +
  #scale_x_continuous(limits = c(0, 45), breaks = seq(0,46, by = 1)) +
  labs(title = paste0(unique(sites$code)," Survey ","Year: {current_frame}"),
       subtitle = "Site-Size Histogram",
       caption = paste0("Source: ",unique(sites$source)))+
  ylab("Count\n") +                                                  
  xlab("\n Site Size in Hectars") +
  theme_minimal_hgrid() +
  theme(
    axis.text = element_text(size = 8),
   ) +
  transition_manual(time_start) +
  ease_aes("linear")

#create animation
animate(histogram_gif, width = 800, height = 600)

anim_save("analysis/derived/figures/histogram/histogram_tbs.gif")

