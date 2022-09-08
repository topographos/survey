####################
#Purpose: this script converts ratio var: area into ordinal var: settlement_type
#Date: 05/10/2021
#Revised:10/01/2022
#Import format: sf
#Output format: sf

# install packages

install.packages("ggplot2")
install.packages("dplyr")


# load packages

library(ggplot2)
library(dplyr)
library(sf)

# import the data 

ths_sites = st_read("data/data.gpkg", layer = "ths_sites_2500_500_BC")

# add column and use case_when statement
# classification of settlement size based on
# Parker's Mechanics of Empire p.


ths_sites = ths_sites %>% 
  mutate(
    size_cat_n = case_when(
      area < 0.1 ~ "scatter",
      area >= 0.1 & area < 1 ~ "small village",
      area >= 1 & area < 5 ~ "village",
      area >= 5 & area < 10 ~ "large village",
      area >= 10 & area < 20 ~ "small town",
      area >= 20 & area < 30 ~ "town",
      area >= 30 & area < 50 ~ "small city",
      area >= 50 ~ "city"
    )
  )

# function
# data = sf or data frame
# colname = column name with area size of settlement
add_size_cat_parker = function(data, colname) {
  colname <- enquo(colname)
  data  %>% 
    mutate(
      size_cat := case_when(
        !!(colname) < 0.1 ~ "scatter",
        !!(colname) >= 0.1 & !!(colname) < 1 ~ "small village",
        !!(colname) >= 1 & !!(colname) < 5 ~ "village",
        !!(colname) >= 5 & !!(colname) < 10 ~ "large village",
        !!(colname) >= 10 & !!(colname) < 20 ~ "small town",
        !!(colname) >= 20 & !!(colname) < 30 ~ "town",
        !!(colname) >= 30 & !!(colname) < 50 ~ "small city",
        !!(colname) >= 50 ~ "city"
      )
    )
  
}

# run using function
ths_sites = add_size_cat_parker(ths_sites, colname = area)

# export to database

st_write(ths_sites, "data.gpkg", layer = "ths_sites_2500_500_BC", delete_layer = TRUE)


