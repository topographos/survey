library(sf)
library(dplyr)

# read data ----

# sites
sites = readRDS("data/tab/njs_sites_LONG_SF.rds")

# sites from ia
sites_ia = filter(time_start == -700)

# survey extent
survey = st_read("data/vect/data.gpkg", layer = "njs_survey")

# quick plot
plot(survey$geom)
plot(sites$geometry, add = TRUE)

# read ppp

njs.ppp = readRDS("data/tab/njs_survey_PPP.rds")

ls = split(njs.ppp)

ce_index = sapply(ls, clarkevans)

ce_index_df = data.frame(
index = ce_index[1,],
time = as.numeric(colnames(ce_index))
)

ce_index_df$index_sc = scales::rescale(ce_index_df$index, to = c(0,1))

ggplot() + 
  geom_line(data = ce_index_df, mapping = aes(x = time, y = index))

ggplot() +
  geom_line(data = ce_index_df, mapping = aes(x = time, y = index_sc)) +
  geom_point(data = ann_df, mapping = aes(x = time_start, y = ann_sc), color = "red") +
  geom_point(data = SPAG_SC, mapping = aes(x = period, y = i.over.sc ), color = "blue")
