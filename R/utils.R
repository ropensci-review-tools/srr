# Same code as in 'pkgstats', but that pkg not imported here:
extra_manifest_paths <- function (path) {

    path_src <- fs::path (path, "src")
    if (!fs::dir_exists (path_src)) {
        return (NULL)
    }
    f_manifest <- fs::dir_ls (
        path_src,
        type = "file",
        regexp = "source.*manifest",
        ignore.case = TRUE
    )
    if (length (f_manifest) == 0L) {
        return (NULL)
    }
    f_ext <- fs::path_ext (f_manifest)
    manifest_parse_fn <- paste0 ("parse_manifest_", f_ext)
    pkg_fns <- ls (envir = asNamespace ("pkgstats"), all.names = TRUE)
    index <- which (manifest_parse_fn %in% pkg_fns)
    if (length (index) == 0L) {
        return (NULL)
    }
    # If multiple versions, parse only first
    x <- do.call (manifest_parse_fn [1], list (f_manifest [1]))
    if (!"vendor_sources" %in% names (x)) {
        return (NULL)
    }
    vendor_sources <- vapply (x$vendor_sources, function (v) {
        fs::path_abs (fs::path (path_src, v))
    }, character (1L))
    vendor_sources <- vendor_sources [which (fs::dir_exists (vendor_sources))]

    return (vendor_sources)
}

parse_manifest_toml <- function (f) {
    requireNamespace ("toml", quietly = TRUE)
    toml::parse_toml (readLines (f))
}

parse_manifest_json <- function (f) {
    requireNamespace ("jsonlite", quietly = TRUE)
    jsonlite::read_json (f)
}

#' Regular expressions for matching the format of standards in the
#' `statistical-software-review-book/standards/*.Rmd` files.
#'
#' All patterns which grep text of standards, or of text derived from standards,
#' are defined here, so that any change in the format of the standards need only
#' be reflected in this one function. Patterns are deliberately minimal, with
#' anchors, brackets, and other context to be added by the calling code.
#'
#' @param type Name of pattern to return, one of:
#' \itemize{
#' \item "id": Standard identifier, such as "G1.0" or "BS2.15a".
#' \item "id_strong": Identifier wrapped in curly braces and preceded by
#'   roxygen2's `\\strong` or `\\emph` markup.
#' \item "rmd_anchor": Hyperlink anchor following the identifier in the
#'   original Rmd files, "]{#G1_0}".
#' }
#' @return A single regular expression as character string.
#' @noRd
stds_regex <- function (type = c (
                            "id",
                            "id_strong",
                            "rmd_anchor"
                        )) {

    type <- match.arg (type)

    switch (type,
        "id" = "[A-Z]+[0-9]+\\.[0-9]+([a-z]?)",
        "id_strong" = "\\\\(strong|emph)\\{[A-Z]+[0-9]+(\\.[0-9]+)?\\}",
        "rmd_anchor" = "\\]\\{#[A-Z]+[0-9]+\\_[0-9]+([a-z]?)\\}"
    )
}
