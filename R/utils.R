pkg_name_from_desc <- function(path) {
  desc <- file.path(path, "DESCRIPTION")
  as.character(read.dcf(desc, "Package"))
}

get_all_file_names <- function(path) {
  if (!dir.exists(file.path(path, "R"))) {
    warning("Directory [", path, "] does not appear to be an R package")
    return(NULL)
  }

  dirs <- c(".", "R", "vignettes", "tests", "inst")
  sfxs <- c("\\.(R|r)?md$", "\\.(R|r)$", "\\.(R|r)md$", "\\.(R|r)$", "\\.(R|r)?md$")
  rec <- c(FALSE, FALSE, TRUE, TRUE, TRUE)

  flist <- lapply(seq_along(dirs), function(i) {
    list.files(file.path(path, dirs[i]),
      full.names = TRUE,
      recursive = rec[i],
      pattern = sfxs[i]
    )
  })
  flist <- normalizePath(unlist(flist))

  # Get any duplicated files, usually one `.Rmd`, one `.md`, and reduce to
  # `.md` only, so it can be removed:
  flist_noext <- tools::file_path_sans_ext(flist)
  dups <- which(duplicated(flist_noext))
  index <- vapply(dups, function(d) {
    these <- which(flist_noext == flist_noext[d])
    ret <- d
    if (all(c("md", "Rmd") %in% tools::file_ext(flist[these]))) {
      ret <- these[which(tools::file_ext(flist[these]) == "md")]
    }
    return(ret)
  }, integer(1))
  if (length(index) > 0) {
    flist <- flist[-index]
  }

  return(flist)
}
