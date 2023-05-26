

centr_area_b_panel = function(sites, survey, time){
  
  # load libraries
  library(dplyr)
  library(fmap)
  library(ggplot2)
  library(ggspatial)
  library(cowplot)
  
  # 1: calculate median center
  cmed <- sfdep::center_median(sites) |> 
    sf::st_as_sf() |>  
    dplyr::rename(
      geom = x
    ) |> 
    dplyr::mutate(
      id = "cmed"
    )
  
  # 2: calculate maximum distance between cmd and sites
  cmed.dist = sf::st_distance(cmed,sites) |> unclass() |>  max()
  
  # 3: subset sites to time period
  sites = sites |> dplyr::filter(time_start == time)
  
  # period for title
  period = sites |> sf::st_drop_geometry() |> distinct(period) |> as.character()

  
  # 4: create rings - equal area
  rings = fmap::fmap_multi(
    radius_outer = cmed.dist,
    ncircles = 12, 
    geo_centre = cmed, 
    geo_points = sites, 
    id_var = "id",
    sum = "size_ha",
    output = "data")
  
  # 5: clip rings to survey extent
  rings.clip = sf::st_intersection(rings,survey)
  
  # 6: calculate  per proportions
  rings_df = st_drop_geometry(rings.clip)
  
  rings_df$size.prop = rings_df$sum / sum(rings_df$sum) * 100
  
  rings_df$cum.prop = cumsum(rings_df$size.prop)
  
  # 6: calculate B coefficient
  Bcoeff = ( sum(rings_df$cum.prop) - 650 ) / 550
  
  # 7: plot map
  ring.map = ggplot() + 
      geom_sf(data = survey, fill = "grey80", colour = "grey80") +
      geom_sf(data = rings.clip, fill = NA, colour = "gray40", linetype = "dotted") +
      geom_sf(data = sites,
              aes(
                size =  size_ha,
              ),
              colour = "#53868B") +
      geom_sf(data = cmed, pch = 21, stroke = 1, size = 3) +
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
  
  # 8: plot rings
  ring.plot = ggplot(rings_df) + 
    geom_line(aes(x = zonal_area, y = size.prop), colour = "#53868B", linewidth = 1) +
    scale_x_continuous(breaks = seq(1,12,1))+
    scale_y_continuous(limits = c(0,100)) +
    labs(
      title = paste0("Centralization - from ", period ),
      subtitle = "equal area rings from median centroid") +
    annotate("text", x = 11, y = 90, label =  paste0("B = ", round(Bcoeff,2))) +
    annotate("text", x = 1, y = 0, label =  "5.7 km", size = 4) +
    annotate("text", x = 12, y = 0, label =  "20 km", size = 4) +
    xlab("ring\n") +                                                  
    ylab("\n%") +
    theme_minimal_hgrid() +
    theme(
      axis.text = element_text(size = 8),
      plot.subtitle=element_text(face="italic"),
      plot.margin = unit(c(1,1,1,1), units = , "cm"))
  
  
  panel = cowplot::plot_grid(ring.plot, ring.map, nrow = 1)
  
  
  save_plot(filename = paste(time, ".png", sep = '_'), panel,bg = 'white', base_width = 40, base_height = 15, units = "cm")
  
  

}
