library(sf)
library(purrr)
library(dplyr)
install.packages("purrr")


# TBS ----

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


# NJS ----

# load data
njs_sites = readRDS("data/tab/njs_sites_LONG.rds")

# ad temporary mid_point coulmn
njs_sites = njs_sites %>% 
  mutate(
    mid_point = time_start + 25 # 25 years
  )

# add period name
njs_sites = njs_sites %>% 
  mutate(
    period = case_when(     mid_point > -3100 & mid_point < -2550 ~ "Early 3rd Millenium",
                            mid_point > -2550 & mid_point < -2000 ~ "Late 3rd Millenium",
                            mid_point > -2000 & mid_point < -1400 ~ "Khabur",
                            mid_point > -1400 & mid_point < -1000 ~ "Middle Assyrian",
                            mid_point > -1000 & mid_point < -600 ~  "Late Assyrian",
                            mid_point > -600  & mid_point < -300 ~  "Post Assyrian",
                            mid_point > -300  & mid_point < -150 ~  "Hellenistic",
                            mid_point > -150  & mid_point <  250 ~  "Parthian",
                            mid_point >  250  & mid_point <  650 ~  "Sasanian",
                            mid_point >  650  & mid_point <  1050 ~  "Early Islamic")
  ) %>% 
  select(-mid_point)

# save
saveRDS(njs_sites,"./data/tab/njs_sites_LONG.rds")
