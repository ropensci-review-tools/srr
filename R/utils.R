pkg_name_from_desc <- function (path) {

    desc <- fs::path (path, "DESCRIPTION")
    as.character (read.dcf (desc, "Package"))
}

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
    pkg_fns <- ls (envir = asNamespace ("pkgstats"), all = TRUE)
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
