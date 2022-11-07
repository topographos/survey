site_category = function(df, col){
  #define levels
  levels = c("scatter", "small village", "village", "large village", "small town", "town", "small city", "city")
  #define labels
  labels = c("scatter (<0.1)", "small village (0.1-1)", "village (1-5)", "large village (5-10)", "small town (10-20)", "town (20-30)", "small city (30-50)", "city (>50)")
  
  df$category[df[[col]] < 0.1] <- "scatter"
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