
library(potential)
library(mapsf)
library(isdas)

# data

data("PointPatterns")


point.pattern.sf = st_as_sf(PointPatterns, coords = c("x","y")) %>% 
  mutate(
    id = seq(1,240),
  ) %>% 
  rename(
     period = Pattern
  )

point.pattern.sf$size_ha = rnorm(60, mean = 2, sd = 1)

point.pattern.sf = point.pattern.sf[,c(2,4,1,3)]

saveRDS(point.pattern.sf, "data/tab/point_pattern_test.rds")
        )
# load data

sites = readRDS("data/tab/njs_sites_LONG_SF.rds")

sites_ia = sites %>% 
  filter(time_start == -1500) %>% 
  select(id, size_ha)

mf_map(sites_ia, col= "grey80")

# create grid

y <- create_grid(x = sites_ia, res = 250)


d <- create_matrix(x = sites, y = y)

d[1:5, 1:5]

y$pot <- potential(x = sites, y = y, d = d,
                   var = "size_ha", fun = "e",
                   span = 2000, beta = 2)

mf_map(n3_poly, col= "grey80", border = "white", lwd = .4)

mf_map(y, var = "pot", type = "prop", 
       inches= .1, 
       lwd = .5, 
       leg_val_cex = .5, 
       leg_val_rnd = -3,
       leg_frame = FALSE, 
       leg_pos = "topright")

y$pot2 <- 100 * y$pot / max(y$pot)

mf_map(n3_poly, col= "grey80", border = "white", lwd = .4)

mf_map(y, var = "pot2", type = "prop", 
       inches= .1, 
       lwd = .5, 
       leg_val_cex = .5, 
       leg_val_rnd = 0,
       leg_frame = TRUE, 
       leg_pos = "topright")


bks <- seq(1, 100, 10)
iso <- equipotential(x = y, var = "pot2", breaks = bks)
mf_map(x = iso, var = "min", type = "choro", 
       breaks = bks, 
       pal = hcl.colors(10, 'Teal'),
       lwd = .2,
       border = "#121725", 
       leg_pos = "topright",
       leg_val_rnd = 0,
       leg_title = "Potential of\nPopulation")
