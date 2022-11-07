####################
#Purpose: this script converts ratio var: area into ordinal var: settlement_type
#Date: 05/10/2021
#Revised:10/01/2022
#Import format: sf
#Output format: sf


library(sf)
library(dplyr)

# import the data 
st_layers("data/vect/data.gpkg")
sites = st_read("data/vect/data.gpkg", layer = "tbs_sites_point")

# add column and use case_when statement
# classification of settlement size based on
# Parker's Mechanics of Empire p.




head(ths_sites)
# function
# data = sf or data frame
# colname = column name with area size of settlement

add_category_parker = function(df, col){
  #define levels
  levels = c("scatter", "small village", "village", "large village", "small town", "town", "small city", "city")
  #define labels
  labels = c("scatter (<0.1)", "small village (0.1-1)", "village (1-5)", "large village (5-10)", "small town (10-20)", "town (20-30)", "small city (30-50)", "city (>50)")
  
  df$sizecat[df[[col]] < 0.1] <- "scatter"
  df$category[df[[col]] >= 0.1 & df[[col]] < 1 ] <- "small village"
  df$category[df[[col]] >= 1 & df[[col]] < 5 ] <- "village"
  df$category[df[[col]] >= 5 & df[[col]] < 10 ] <- "large village"
  df$category[df[[col]] >= 10 & df[[col]] < 20 ] <- "small town"
  df$category[df[[col]] >= 20 & df[[col]] < 30 ] <- "town"
  df$category[df[[col]] >= 30 & df[[col]] < 50 ] <- "small city"
  df$category[df[[col]] >= 50] <- "city"
  df$category = factor(df$category, levels = levels, labels = labels)
  print(df)
}


# add the column
sites = add_category_parker(sites, "size_ha")

str(ths_sites)

levels(sites$category)

# export to geopackage

st_write(ths_sites, "data/data.gpkg", layer = "ths_sites_2500_500_BC", delete_layer = TRUE)

saveRDS(sites,"data/ths_sites_2500_500_BC_SF.rds")

# save just table

st_geometry(sites) = NULL

saveRDS(sites,"data/ths_sites_2500_500_BC_TAB.rds")

# work in progress

add_category_parker2 = function(df, col){
  df$category = cut(
    df$category[df[[col]]],
    breaks = c(0, 0.1, 1, 5, 10, 20, 30, 50, Inf),
    labels = c("scatter", "small village", "village", "large village", "small town", "town", "small city", "city"),
    right = FALSE)
  
}
