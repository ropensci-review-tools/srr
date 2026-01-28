pkg_name_from_desc <- function (path) {

    desc <- fs::path (path, "DESCRIPTION")
    as.character (read.dcf (desc, "Package"))
}

get_all_file_names <- function (path) {

    if (!fs::dir_exists (fs::path (path, "R"))) {
        warning ("Directory [", path, "] does not appear to be an R package")
        return (NULL)
    }

    dirs <- fs::path_abs (fs::path (path, c (".", "R", "vignettes", "tests", "inst")))
    sfx_r_only <- "\\.(R|r)$"
    sfx_r_md <- "\\.(R|r)(?:md)?$"
    sfxs <- c (sfx_r_md, sfx_r_only, sfx_r_md, sfx_r_only, sfx_r_md)
    rec <- c (FALSE, FALSE, TRUE, TRUE, TRUE)
    index <- which (fs::dir_exists (dirs))

    flist <- lapply (index, function (i) {
        fs::dir_ls (dirs [i], recurse = rec [i], regexp = sfxs [i], type = "file")
    })
    flist <- unname (unlist (flist))

    # Get any duplicated files, usually one `.Rmd`, one `.md`, and reduce to
    # `.md` only, so it can be removed:
    flist_noext <- fs::path_ext_remove (flist)
    dups <- which (duplicated (flist_noext))
    index <- vapply (dups, function (d) {
        these <- which (flist_noext == flist_noext [d])
        ret <- d
        if (all (c ("md", "Rmd") %in% tools::file_ext (flist [these]))) {
            ret <- these [which (tools::file_ext (flist [these]) == "md")]
        }
        return (ret)
    }, integer (1))
    if (length (index) > 0) {
        flist <- flist [-index]
    }

    # Then add src files:
    dirs <- fs::path_abs (fs::path (path, c ("inst", "src")))
    dirs <- dirs [which (fs::dir_exists (dirs))]
    flist_src <- unname (unlist (lapply (dirs, function (d) {
        fs::dir_ls (d, recurse = TRUE, type = "file")
    })))
    flist_src <- flist_src [which (!grepl ("RcppExports", flist_src))]
    flist_src_exts <- tolower (fs::path_ext (flist_src))
    recognised_exts <- c ("cpp", "rs", "hpp", "h")
    flist_src <- flist_src [which (flist_src_exts %in% recognised_exts)]

    return (c (flist, flist_src))
}
