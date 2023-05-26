# packages

library(sf)
library(tidyverse)
library(cowplot)

library(magick)
library(gganimate)
library(gifski)

# DATA ----

# read sites from geopackage
st_layers("./data/vect/tbs/tbs.gpkg")

sites = st_read("../data/vect/tbs/tbs.gpkg", layer = "sites")

head(sites)

unique(sites$time_start)

# select sites from 3100BC - for a single histogram
sites_3100BC = sites %>% filter(time_start == -3100)

# split the sites into list by time attribute - for multiple histograms
time_list = split(sites, sites$time_start)

time_list[[1]]


# SITE - SIZE HISTOGRAM ----

# single graph ----

(histogram = ggplot(data = sites_3100BC, mapping = aes(x = size_ha)) +
  geom_histogram(binwidth = 1, boundary = 0, closed = "left", 
                 fill = "#53868B", color = "white") +
  geom_rug() +
  geom_vline(aes(xintercept = median(size_ha)),                       
             colour = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0,max(floor(sites$size_ha)), by = 1)) +
  labs(title = paste0(unique(sites_3100BC$code)," Survey ", unique(sites_3100BC$time_start) ),
       subtitle = "Site-Size Histogram",
       caption = paste0("Source: ",unique(sites_3100BC$source)))+
  ylab("Count\n") +                                                  
  xlab("\n Site Size in Hectars") +
  theme_minimal_hgrid() +
  theme(
    axis.text = element_text(size = 8),
    plot.margin = unit(c(1,1,1,1), units = , "cm"))
    )

# save 

ggsave(histogram, file = "plots/histogram.png", width = 10, height = 8, bg = 'white')

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
           subtitle = "Site-Size Histogram",
           caption = paste0("Source: ",unique(sites$source)))+
      ylab("Count\n") +                                                  
      xlab("\n Site Size in Hectars") +
      theme_minimal_hgrid() +
      theme(
        axis.text = element_text(size = 8),
        plot.margin = unit(c(1,1,1,1), units = , "cm"))
  
  ggsave(histogram, file = paste0("plots/histogram_", time,".png"), width = 10, height = 8, bg = 'white')
  
}

# animation ----

histogram = sites %>% 
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
animate(histogram, width = 800, height = 600)

anim_save("tbs_histogram.gif")

ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
  geom_point() +
  transition_states(Species, transition_length = 2, state_length = 1) +
  shadow_mark(past = TRUE, future = TRUE, colour = 'grey') +
  view_zoom(pause_length = 1, step_length = 2, nsteps = 3, pan_zoom = -3)

