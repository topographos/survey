library(fmap)
library(devtools)
install_github("lbuk/fmap")
# function fcircle ----
# Function for creating Fresnel circles polygons for mapping.

# Load the sf dataset of Soho pumps
data(sohopumps)

# Filter the Broad Street Pump
broadstreetpump =
  sohopumps %>%
  filter(Soho.Pump == "Broad Street")

circles_1 = fcircles(radius_inner = 125, ncircles = 8, geo_centre = broadstreetpump)

?fcircles

circles$area = st_area(circles)

# function fmap_data ----
#' Function for creating Fresnel Map polygons by aggregating data to the level of equal-area concentric circles (or annuli).

data(choleradeaths)

# Load the sf dataset of Soho pumps
data(sohopumps)

# Filter the Broad Street Pump
broadstreetpump =
  sohopumps %>%
  filter(Soho.Pump == "Broad Street")

circles_2 = fmap_data(radius_inner = 125, ncircles = 8, geo_centre = broadstreetpump, geo_points = choleradeaths, count = TRUE)

# function fmap_plot ----
plot = fmap_plot(radius_inner = 125, ncircles = 8, geo_centre = broadstreetpump, geo_points = choleradeaths, sum = "Cholera.Deaths")
class(plot)

# function fmap_multi
?fmap_multi

# Load the sf dataset of cholera deaths
data(choleradeaths)

# Load the sf dataset of Soho pumps
data(sohopumps)

multi_data = fmap_multi(ncircles = 6, radius_outer = 200, geo_points = choleradeaths, geo_centres = sohopumps, 
           id_var = "Soho.Pump", sum = "Cholera.Deaths", output = "data")

multi_stats = fmap_multi(ncircles = 6, radius_outer = 200, geo_points = choleradeaths, geo_centres = sohopumps, 
                        id_var = "Soho.Pump", sum = "Cholera.Deaths", output = "stats")

# 