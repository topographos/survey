# packages
library(sf)
library(tidyverse)

# load data
sites = readRDS("./data/tab/njs_sites_LONG_SF.rds")

# 1: count sites by time block
sites_count = sites %>% 
  mutate(time_start = factor(time_start)) %>% 
  st_drop_geometry() %>% 
  group_by(time_start) %>% 
  summarise(total = n())

head(sites_count, n = 3)

# 1 method nest data
# split data into list
s <- split(sites$id, sites$time_start)

# function
diffs <- function(i) c(paste("added", setdiff(s[[i]], s[[i-1]])),
                       
                       paste("removed", setdiff(s[[i-1]], s[[i]])), 
                       
                       paste("backward", intersect(s[[i]], s[[i-1]])),
                       
                       paste("forward", intersect(s[[i]], s[[i+1]]))
)

# run function - IMPORTANT change seqeunce 2:n-1
diffs_list <- setNames(lapply(seq_along(s)[2:(length(s)-1)], diffs), names(s)[2:(length(s)-1)])

# create dataframes
df = setNames(stack(diffs_list), c("id", "time_start"))

df= tidyr::separate(df,id, c("change", "id"), sep = " ")

df = df %>% filter(id !="")

df = df[,c(3,2,1)]

df_wide = df %>%
  add_count(id, time_start,change) %>%
  pivot_wider(-id,names_from = change, values_from = n, values_fn = sum)

# join summary table with total number of settlements per time block
df_wide = left_join(df_wide,sites_count)

# calculate number of new sites and per change
df_wide  = df_wide  %>% 
  mutate(new_sites= (total - lag(total)), # new  cases per time block
         pct_change = new_sites / lag(total) * 100) # percentage change

head(df_wide)

# save

write_rds(df_wide,"data/tab/lcp_continuity.rds")


# plot

per_change = df_wide %>% 
  filter(pct_change > 0)

###
ggplot() +
  geom_col(df_wide, mapping = aes(x = time_start, y = total), alpha = 0.2, color = NA) +
  geom_vline(data = per_change, aes(xintercept = time_start), linetype="dotted") +
  geom_text(data = per_change, 
            aes(x = time_start, 
                y = 60, 
                label = paste(new_sites,"(", round(pct_change), "%)")),
            color = "red") +
  hrbrthemes::theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




