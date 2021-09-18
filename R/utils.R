
pkg_name_from_desc <- function (path) {

    desc <- file.path (path, "DESCRIPTION")
    as.character (read.dcf (desc, "Package"))
}
