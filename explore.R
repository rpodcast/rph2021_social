library(dplyr)
library(tidyr)
library(pkgstats)
library(furrr)
library(progressr)

source("utils.R")

# declare options for the future package
options(parallelly.fork.enable = TRUE)
plan(multicore, workers = 6)

# load cached data set
message("-----loading cached data-------")
df <- readRDS("df.rds")

#df2 <- df %>% sample_n(2000)

message("-----begin package summary generation-------")

fop <- furrr_options(seed = TRUE, scheduling = 2L)
with_progress({
  p <- progressor(steps = nrow(df))
  df2 <- df %>%
    mutate(stats_summ = future_map(stats_obj, pkgstats_summary_custom, p = p, .options = fop))
    #mutate(ext_network = future_map(stats_summ, pkgstats_ext_network_custom, p = p, .options = fop))
})

message("------end package summary generation------")

df3 <- df2 %>% 
  select(., -stats_obj) %>%
  tidyr::unnest(cols = "stats_summ")

message("------ saving data to file ------")
save(df3, file = "df3.rds")
