library(sf)
library(dplyr)
install.packages("concaveman")
library(concaveman)
# load all sites

n_mesopotamia_sites = st_read("./data/raw/north_mesopotamia_sites.shp")

plot(concaveman(njs_sites,concavity = 2.3))

# glimpse at data
head(n_mesopotamia_sites)

# count unique sites
unique(n_mesopotamia_sites$Source)

# THS Survey ----
njs_sites = n_mesopotamia_sites %>% 
  filter(Source == "Wilkinson and Tucker 1995")

# plot sites
plot(ths_sites$geometry)

# export ths sites - Tell Hamoukar Survey
st_write(ths_sites,"data/ths_sites.shp")

# TBS Survey ----
tbs_sites = n_mesopotamia_sites %>% 
  filter(Source == "Ur and Wilkinson 2008")

# plot sites
plot(tbs_sites$geometry)

# export ths sites - Tell Hamoukar Survey
st_write(ths_sites,"data/ths_sites.shp")

