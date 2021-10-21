# load packages ----
library(pkgstats)
library(future)
library(dplyr)
library(furrr)
library(progressr)

source("utils.R")

# declare options for the future package
options(parallelly.fork.enable = TRUE)
plan(multicore, workers = 4)

#rsync -rtlzvh --dry-run --delete cran.r-project.org::CRAN /mnt/storage/r_opensource_projects/cran_mirror/
#rsync -rtlzvh --delete cran.r-project.org::CRAN /mnt/storage/r_opensource_projects/cran_mirror/

# assemble the relevant paths to scrape (not all are necessary)
src_path <- "/cran_mirror/tarballs/src/contrib"
pkg_dir_raw <- fs::dir_ls(src_path, recurse = FALSE, type = "directory")
pkg_latest_files <- fs::dir_ls(src_path, recurse = FALSE, type = "file")
pkg_dir_analysis <- c(
  fs::path_filter(pkg_dir_raw, regexp = "[0-9]\\."),
  fs::path(src_path, "Archive")
)

# save file list for future use
if (fs::file_exists("flist.rds")) {
  flist <- readRDS("flist.rds")
} else {
  flist <- pkgstats_files(pkg_dir_analysis, create_chunks = FALSE)
  flist <- c(flist, pkg_latest_files)
  saveRDS(flist, file = "flist.rds")
}

# create a sample of data
flist2 <- flist[sample(1:length(flist), 50)]

with_progress({
  df <- tibble::tibble(
    file = flist2
  ) %>%
    mutate(stats_obj = pkgstats_mapper(file)) %>%
    mutate(failed_ind = purrr::map_lgl(stats_obj, ~is.null(.x))) %>%
    filter(!failed_ind) %>%
    mutate(r_version = purrr::map_chr(stats_obj, function(x) x[["r_version"]])) %>%
    mutate(package_type = purrr::map_chr(stats_obj, function(x) x[["package_type"]]))
})

# analyzing failed packages
df_failed <- df %>%
  mutate(failed_ind = purrr::map_lgl(stats_obj, ~is.null(.x))) %>%
  filter(failed_ind)

