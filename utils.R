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
  future_map(file, pkgstats_custom, p = p, .options = furrr_options(seed = TRUE))
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
