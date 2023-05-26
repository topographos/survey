# PACKAGES ----
library(spatstat)
library(sf)
library(terra)
library(tidyterra)
library(dplyr)
library(ggplot2)
library(cowplot)

# DATA ----

# points
sites = st_read("analysis/derived/surveys.gpkg", layer = "sites_tbs")

survey = st_read("analysis/derived/surveys.gpkg", layer = "survey_tbs")%>% select(code)


# PPP OBJECT ---

## create win object
survey.win = as.owin(survey)

## extract coordinates
sites_coords = st_coordinates(sites)

## create data frame with marks
marks_df = data.frame(time = as.factor(sites$time_start), size = sites$size_ha)

# create ppp object - it wukk have meters units
survey.m= ppp(
  x = sites_coords[,1],
  y = sites_coords[,2],
  window = survey.win,
  marks = marks_df
)

## re - scale from m to km
survey.km = rescale.ppp(survey.m, 1000, "km")

# split ppp object by time peridod
survey_by_time = split.ppp(survey.km, f = survey.km$marks$time)


# DENSITY ----


## sigma - choose one:

# bw.diggle test for optimal bandwidth
bw_diggle <- bw.diggle(survey.km)
#bw.scott test for optimal bandwidth
bw_scott<- bw.scott(survey.km)
# bw.frac test for optimum bandwidth
bw_frac<- bw.frac(survey.km)
# bw.ppl test for optimum bandwidth
bw_ppl <- bw.ppl(survey.km)

## density
density  = density.ppplist(survey_by_time,
                              sigma = 2, 
                              edge=TRUE, 
                              kernel="quartic",
                              positive = TRUE,
                              at = "pixels")

plot(density)

## density  at point 

density.p  = density.ppplist(survey_by_time,
                           sigma = 2, 
                           edge=TRUE, 
                           kernel="quartic",
                           positive = TRUE,
                           at = "points")

# CONVERT TO DATA FRAME ----

density.df = sapply(density.p, bind_cols)

density.df= tibble::enframe(density.p)

density.df = tidyr::unnest(density.df, cols = c(value))

density.df$period = as.numeric(density.df$name)



# CONVERT TO RASTER ----

# convert each img in the list to terra class
density.rast =  lapply(density, rast)

# convert to multi -layer raster stack
tbs.density= rast(density.rast)

# round
tbs.density = round(tbs.density,3)

# project
crs  = "+init=epsg:32637"

crs(tbs.density) = crs

# PLOT KDE SURFACE ----

tbs.density[tbs.density == 0] = NA

plot(tbs.density)


# theme
blank_theme = theme_bw() +
  theme(
  axis.line = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  strip.background = element_blank(),
  plot.margin = unit(c(1,1,1,1), units = , "cm"),
  legend.position = "left"
)

# plot
(map = ggplot() +
  geom_spatraster(data = tbs.density) +
  facet_wrap(~lyr,ncol = 3) +
  #labs(title = "tbs Survey - Kernel Smoothed Intensity" ) +
  scale_fill_whitebox_c(name = "intensity", na.value = "white", palette = "muted") +
  blank_theme)

# PLOT KDE VALUES ----


(plot = ggplot(density.df, aes(x = as.factor(period), y = value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(size = 2, alpha = 0.2, color = "#15aabf") +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "Intensity") +
  #labs(subtitle = "Intensity values at sites locations" ) +
  cowplot::theme_minimal_hgrid() +
  theme(
    plot.margin = unit(c(1,1,1,1), units = , "cm"),
  ))

plot_row = cowplot::plot_grid(map,plot, labels = c('(a)', '(b)'))

# now add the title
title <- ggdraw() + 
  draw_label(
    "tbs - Kernel Density Smoothing",
    fontface = 'bold',
    x = 0,
    hjust = 0,
    size = 16
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 10)
  )

panel = plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

ggsave2("analysis/figures/local_density.png", width = 28, height = 12, units = "in", panel, bg ="white")

plot(tbs.density$`-900`)

(map = ggplot() +
    geom_spatraster(data = tbs.density$`-900`) +
    geom_spatraster_contour(data = tbs.density$`-900`, breaks = c(0.1049)) +
    scale_fill_whitebox_c(name = "intensity", na.value = "white", palette = "muted") +
    blank_theme)
terra::summary(tbs.density)
