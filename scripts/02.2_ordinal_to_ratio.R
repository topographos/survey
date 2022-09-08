####################
#Purpose: this script converts ratio var: area into ordinal var: settlement_type
#Date: 05/10/2021
#Revised:10/01/2022
#Import format: sf
#Output format: sf


library(sf)
library(dplyr)

# import the data 

sites = st_read("data/data.gpkg", layer = "ths_sites_2500_500_BC") %>%  select(-size_cat)

# add column and use case_when statement
# classification of settlement size based on
# Parker's Mechanics of Empire p.




head(ths_sites)
# function
# data = sf or data frame
# colname = column name with area size of settlement

add_size_cat_parker = function(df, col){
  #define levels
  levels = c("scatter", "small village", "village", "large village", "small town", "town", "small city", "city")
  #define labels
  labels = c("scatter (<0.1)", "small village (0.1-1)", "village (1-5)", "large village (5-10)", "small town (10-20)", "town (20-30)", "small city (30-50)", "city (>50)")
  
  df$size_cat[df[[col]] < 0.1] <- "scatter"
  df$size_cat[df[[col]] >= 0.1 & df[[col]] < 1 ] <- "small village"
  df$size_cat[df[[col]] >= 1 & df[[col]] < 5 ] <- "village"
  df$size_cat[df[[col]] >= 5 & df[[col]] < 10 ] <- "large village"
  df$size_cat[df[[col]] >= 10 & df[[col]] < 20 ] <- "small town"
  df$size_cat[df[[col]] >= 20 & df[[col]] < 30 ] <- "town"
  df$size_cat[df[[col]] >= 30 & df[[col]] < 50 ] <- "small city"
  df$size_cat[df[[col]] >= 50] <- "city"
  df$size_cat = factor(df$size_cat, levels = levels, labels = labels)
  print(df)
}


# add the column
sites = add_size_cat_parker(sites, "area")

str(ths_sites)

levels(sites$size_cat)

# export to geopackage

st_write(ths_sites, "data/data.gpkg", layer = "ths_sites_2500_500_BC", delete_layer = TRUE)

saveRDS(sites,"data/ths_sites_2500_500_BC_SF.rds")

# save just table

st_geometry(sites) = NULL

saveRDS(sites,"data/ths_sites_2500_500_BC_TAB.rds")

# work in progress

add_size_cat_parker2 = function(df, col){
  df$size_cat = cut(
    df$size_cat[df[[col]]],
    breaks = c(0, 0.1, 1, 5, 10, 20, 30, 50, Inf),
    labels = c("scatter", "small village", "village", "large village", "small town", "town", "small city", "city"),
    right = FALSE)
  
}
