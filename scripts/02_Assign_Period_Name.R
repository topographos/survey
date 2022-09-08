library(sf)
library(purrr)
library(dplyr)
install.packages("purrr")
# load data

st_layers("data/data.gpkg")

tbs_sites = st_read("data/data.gpkg", layer = "tbs_sites")

head(tbs_sites)

# -6800 to -5200 Late Neolithic
# -5200 to 4000 Fith Millenium
# -2600 to -2000 Mid-Late Third Millennium
# -2000 to -1600 Early Second Millennium - Khabur

c(-2600:-1999)

tbs_sites = tbs_sites %>% 
  mutate(
    period = case_when(
      start_date %in% c(-2600:-2001) ~ "Late Third Millennium",
      start_date >= -2000 & start_date < -1600 ~ "Khabur",
      start_date >= -1600 & start_date < -1200 ~ "Late Bronze Age",
      start_date >= -1200 & start_date < -300 ~ "Iron Age",
    )
  )

rep(tbs_sites$start_date:tbs_sites$end_date)

tbs_test = tbs_sites %>% 
  mutate(
    range = list(start_date:end_date)
  ) 

Map(`:`, tbs_sites$start_date, tbs_sites$end_date)



tbs_sites_2600_300BC = tbs_sites %>% 
  filter(!is.na(period))

st_write(tbs_sites_2600_300BC, "data/data.gpkg", 
         layer = "tbs_sites_2600_300BC", delete_layer = TRUE)
