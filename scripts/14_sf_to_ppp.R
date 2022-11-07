# packages ----
library(sf)
library(spatstat)
library(maptools)

# NJS ----

# load data
# sites
sites = readRDS("data/tab/njs_sites_LONG_SF.rds")

# survey extent
survey = st_read("data/vect/data.gpkg", layer = "njs_survey") %>% st_as_sfc()

# create ppp window object
survey.win = as.owin(survey)
class(survey.win)

# create ppp with period marks

## extract coordinates
sites_coords = st_coordinates(sites)

## create data frame with marks
marks_df = data.frame(time = as.factor(sites$time_start), size = sites$size_ha)

## create ppp
njs.ppp = ppp( x = sites_coords[,1], 
                  y = sites_coords[,2], 
                  marks = marks_df,
                  window = survey.win)

## save as an R object
saveRDS(njs.ppp, "data/tab/njs_survey_PPP.rds")

