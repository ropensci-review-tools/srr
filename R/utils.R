pkg_name_from_desc <- function (path) {

    desc <- file.path (path, "DESCRIPTION")
    as.character (read.dcf (desc, "Package"))
}

get_all_file_names <- function (path) {

    if (!dir.exists (file.path (path, "R"))) {
        warning ("Directory [", path, "] does not appear to be an R package")
        return (NULL)
    }

    dirs <- c (".", "R", "vignettes", "tests")
    sfxs <- c ("\\.(R|r)md$", "\\.(R|r)$", "\\.(R|r)md$", "\\.(R|r)$")
    rec <- c (FALSE, FALSE, TRUE, TRUE)

    flist <- lapply (seq_along (dirs), function (i) {
        list.files (file.path (path, dirs [i]),
            full.names = TRUE,
            recursive = rec [i],
            pattern = sfxs [i]
        )
    })
    flist <- normalizePath (unlist (flist))

    return (flist)
}
