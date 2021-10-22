# load packages ----
library(pkgstats)
library(future)
library(dplyr)
library(furrr)
library(progressr)

source("utils.R")

# declare options for the future package
options(parallelly.fork.enable = TRUE)
plan(multicore, workers = 12)

#rsync -rtlzvh --dry-run --delete cran.r-project.org::CRAN /mnt/storage/r_opensource_projects/cran_mirror/
#rsync -rtlzvh --delete cran.r-project.org::CRAN /mnt/storage/r_opensource_projects/cran_mirror/

# save file list for future use
src_path <- "/cran_mirror/tarballs/src/contrib"
if (fs::file_exists("flist.rds")) {
  flist <- readRDS("flist.rds")
} else {
  flist <- gen_pkg_list(src_path)
}

# create a sample of data if running in test mode
test_mode <- FALSE
if (test_mode) {
  flist <- flist[sample(1:length(flist), 500)]
}

message("-----begin package stats generation-------")
with_progress({
  df <- tibble::tibble(
    file = flist
  ) %>%
    mutate(stats_obj = pkgstats_mapper(file)) %>%
    mutate(failed_ind = purrr::map_lgl(stats_obj, ~is.null(.x))) %>%
    filter(!failed_ind) %>%
    mutate(r_version = purrr::map_chr(stats_obj, function(x) x[["r_version"]])) %>%
    mutate(package_type = purrr::map_chr(stats_obj, function(x) x[["package_type"]]))
})
message("------end package stats generation------")

message("------ saving data to file ------")
saveRDS(df, file = "df.rds")


