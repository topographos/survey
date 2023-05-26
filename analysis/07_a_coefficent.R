library(sf)
library(ggplot2)
library(dplyr)
library(cowplot)
library(tmap)

# function
source("func/a12coeff.R")

# DATA ----

# read sites from geopackage
sites = st_read("analysis/derived/surveys.gpkg", layer = "sites_tbs")

# split the sites into list by time attribute - for multiple histograms
time.list = split(sites, sites$time_start)

result.list = list()

for(i in 1:length(time.list)){
  
  a12c = a12coeff(time.list[[i]]$size_ha,plotting=TRUE)
  
  result.list[[i]] = a12c
  
}

# 

a12coeff.df = bind_rows(result.list)

a12coeff.df$period = as.numeric(names(time.list))

a12coeff.df$A2 = a12coeff.df$A2 * - 1
  
a12coeff.df$A = a12coeff.df$A1 + a12coeff.df$A2

write_csv(a12coeff.df,"analysis/derived/a_coeff/a_coeff_tbs.csv")


# PLOT ----

(a_plot = ggplot(data = a12coeff.df) +
  geom_line(aes(x = as.factor(period), y = A, group = 1) , color = "#53868B", linewidth = 1) +
  geom_point(aes(x = as.factor(period), y = A), shape= 22, fill = "#53868B", color = "white", size = 3 ) +
  labs(title = "tbs Survey -  A-coefficient index ",
       subtitle = "") +
  scale_y_continuous(name = "A") +
  scale_x_discrete(name = "Year") +
  theme_minimal_hgrid() +
  theme(
    legend.position = "top",
    axis.text = element_text(size = 8),
    plot.subtitle=element_text(face="italic"),
    plot.margin = unit(c(1,1,1,1), units = , "cm")))

ggsave("analysis/figures/a_plot_tbs.png", bg = "white")

