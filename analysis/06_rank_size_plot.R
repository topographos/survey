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


# RANK - SIZE HISTOGRAM ----

# multiple graphs ----

a.coeff = read.csv("analysis/derived/a_coeff/a_coeff_tbs.csv")

for (i in 1:length(time_list)) {

  # create a data frame for each species
  sites = as.data.frame(time_list[[i]])
  # calculate zipf line
  zipf =  max(sort(sites$size_ha,decreasing = TRUE))/(1:length(sites$size_ha))
  # create an object that holds start date for time period
  time = unique(sites$time_start)
  # filter A coeff
  a = dplyr::filter(a.coeff, period == time)
  # make the plots 
  rank_size = ggplot(data = sites) +
    geom_line(aes(x = log(1:length(sites$size_ha)), y = log(sort(sites$size_ha,TRUE))), color = "#53868B", size = 1) +
    geom_point(aes(x = log(1:length(sites$size_ha)), y = log(sort(sites$size_ha,TRUE))), shape= 22, fill = "#53868B", color = "white", size = 3 ) +
    labs(title = paste0("tbs Survey  ", unique(sites$time_start) ),
         subtitle = paste0("Rank-Size Graph ","A  ", round(a$A,2)))+
    ylab("Log Size in Hectars\n") +                                                  
    xlab("\n Log Rank") +
    geom_line(aes(x = log(1:length(sites$size_ha)), y = log(zipf)), linetype = "dashed", color = "#F4A460") +
    theme_cowplot()
      theme(
        axis.text = element_text(size = 8),
        plot.margin = unit(c(1,1,1,1), units = , "cm"))
  
  ggsave(rank_size, file = paste0("analysis/figures/rank_size/rank_tbs_", time,".png"), width = 10, height = 8, bg = 'white')
  
}



# animation ----

#png_files = list.files("analysis/figures/rank_size/", pattern = ".*png$", full.names = TRUE)
#gifski(png_files, gif_file = "rank_tbs_animation.gif", width = 800, height = 600, delay = 1)

