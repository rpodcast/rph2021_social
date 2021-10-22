pkgstats_files <- function(path, create_chunks = TRUE, chunk_size = 1000L) {
  flist <- fs::dir_ls(
    path = path,
    recurse = TRUE,
    glob = "*.tar.gz"
  )
  
  if (create_chunks) {
    n_files <- length(flist)
    n <- ceiling(n_files / chunk_size)
    n <- factor(rep(seq(n), each = chunk_size))[seq(n_files)]
    flist <- split(flist, f = n)
  }
  
  return(flist)
}

gen_pkg_list <- function(src_path, 
                         latest = TRUE, 
                         archive = FALSE, 
                         prev_versions = FALSE,
                         save_file_list = TRUE,
                         file_name = "flist.rds") {
  
  latest_files <- archive_files <- prev_files <- NULL
  
  if (latest) {
    flist <- fs::dir_ls(src_path, recurse = FALSE, type = "file")
  }
  
  if (archive) {
    archive_files <- fs::path(src_path, "Archive")
  }
  
  if (prev_versions) {
    pkg_dir_raw <- fs::dir_ls(src_path, recurse = FALSE, type = "directory")
    prev_version_files <- fs::path_filter(pkg_dir_raw, regexp = "[0-9]\\.")
  }
  
  if (any(archive, prev_versions)) {
    pkg_dir_analysis <- c(prev_version_files, archive_files)
    flist <- c(flist, pkgstats_files(pkg_dir_analysis, create_chunks = FALSE))
  }
  
  if (save_file_list) {
    saveRDS(flist, file = file_name)
  }
  
  return(flist)
}

pkgstats_custom <- function(file, 
                            p,
                            common_root = "/cran_mirror/tarballs/src/contrib",
                            save_full = FALSE,
                            create_summary = FALSE,
                            results_path = tempdir()) {
  
  # determine of package is from a specific R version or archived
  rel_path <- stringr::str_replace_all(file, common_root, "")
  
  if (fs::path_has_parent(rel_path, "/Archive")) {
    r_version <- NA
    package_type <- "archive"
  } else {
    tmp_paths <- fs::path_split(rel_path)
    if (length(tmp_paths[[1]]) > 2) {
      r_version <- fs::path_split(rel_path)[[1]][2]
      package_type <- "standard"
    } else {
      r_version <- "current"
      package_type <- "standard"
    }
  }
  
  s <- tryCatch(pkgstats::pkgstats(file), error = function(e) NULL)
  if (!is.null(s)) {
    s[["r_version"]] <- r_version
    s[["package_type"]] <- package_type
  }
  
  if (save_full) {
    pkg <- utils::tail(pkgstats:::decompose_path(file)[[1]], 1L)
    pkg <- gsub("\\.tar\\.gz$", "", pkg)
    saveRDS(s, file.path(results_path, pkg))
  }
  
  if (create_summary) {
    summ <- tryCatch(pkgstats::pkgstats_summary(s), error = function(e) NULL)
    
    if (is.null (summ)) { # pkgstats failed
      summ <- pkgstats_summary() # null summary
      p <- strsplit(file, .Platform$file.sep)[[1]]
      p <- strsplit(utils::tail(p, 1), "\\_")[[1]]
      summ["package"] <- p[1]
      summ["version"] <- gsub("\\.tar\\.gz$", "", p[2])
    } 
    
    summ[["r_version"]] <- r_version
    summ[["package_type"]] <- package_type
    p()
    return(summ)
  } else {
    p()
    return(s)
  }
}

pkgstats_mapper <- function(file) {
  p <- progressor(steps = length(file))
  future_map(file, pkgstats_custom, p = p, .options = furrr_options(seed = TRUE, scheduling = 2L))
}

pkgstats_summary_custom <- function(s, p) {
  summ <- tryCatch(pkgstats::pkgstats_summary(s), error = function(e) NULL)
  if (is.null (summ)) { # pkgstats failed
    summ <- pkgstats_summary() # null summary
    p <- strsplit(file, .Platform$file.sep)[[1]]
    p <- strsplit(utils::tail(p, 1), "\\_")[[1]]
    summ["package"] <- p[1]
    summ["version"] <- gsub("\\.tar\\.gz$", "", p[2])
  } 
  p()
  return(summ)
}

pkgstats_summary_mapper <- function(s) {
  p <- progressor(steps = length(s))
  future_map(s, pkgstats_summary_custom, p = p, .options = furrr_options(seed = TRUE, scheduling = 2L))
}

pkgstats_ext_network <- function(s) {
  summ <- tryCatch(pkgstats::pkgstats_summary(s), error = function(e) NULL)
  if (is.null(summ)) {
    return(NULL)
  } else {
    ext_calls <- summ$external_calls
    x <- strsplit(ext_calls, ",")[[1]]
    x <- do.call(rbind, strsplit(x, ":"))
    x <- data.frame(
      pkg = x[,1],
      ncalls = as.integer(x[,2])
    )
    x$ncalls_rel <- round(x$ncalls / sum(x$ncalls), 3)
    x <- x[order(x$ncalls, decreasing = TRUE), ]
    rownames(x) <- NULL
    return(x)
  }
}

pkgstats_ext_network_custom <- function(summ, p) {
  ext_calls <- summ$external_calls
  x <- strsplit(ext_calls, ",")[[1]]
  x <- do.call(rbind, strsplit(x, ":"))
  x <- data.frame(
    pkg = x[,1],
    ncalls = as.integer(x[,2]),
    n_unique = as.integer(x[,3])
  )
  x$ncalls_rel <- round(x$ncalls / sum(x$ncalls), 3)
  x$n_unique_rel <- round(x$n_unique / sum(x$n_unique), 3)
  x <- x[order(x$ncalls, decreasing = TRUE), ]
  rownames(x) <- NULL
  p()
  return(x)
}
