#' srr_release
#'
#' Check that all standards are present in code, and listed either as
#' '@srrstats' or '@srrstatsNA'
#' @param path Path to local repository to check
#' @return (Invisibly) List of any standards missing from code
#' @family helper
#' @export
srr_release <- function (path) {

    stds_in_code <- get_stds_from_code (path)
    all_stds_in_code <- unique (unlist (stds_in_code))

    if (length (stds_in_code$stds_todo) > 0)
        cli::cli_alert_warning (paste0 ("This package still has TODO ",
                                        "standards and can not be released"))

    categories <- get_categories (unique (do.call (c, stds_in_code)))

    all_stds <- unlist (lapply (categories$category, get_standard_nums))

    index <- which (!all_stds %in% unique (unlist (stds_in_code)))
    if (length (index) > 0) {
        msg <- "The following standards are missing from your code:"
        cli::cli_alert_warning (msg)
        cli::cli_ol ()
        for (i in index)
            cli::cli_li (all_stds [i])
        cli::cli_end ()
    }

    invisible (all_stds [index])
}

get_stds_from_code <- function (path) {

    flist <- list.files (file.path (path, "R"), full.names = TRUE)
    blocks <- lapply (flist, function (i) roxygen2::parse_file (i))
    names (blocks) <- flist
    blocks <- do.call (c, blocks)
    blocks <- collect_blocks (blocks, path)

    msgs <- collect_one_tag (path, blocks, tag = "srrstats")
    msgs_na <- collect_one_tag (path, blocks, tag = "srrstatsNA")
    msgs_todo <- collect_one_tag (path, blocks, tag = "srrstatsTODO")

    list (
          stds = parse_std_refs (msgs),
          stds_na = parse_std_refs (msgs_na),
          stds_todo = parse_std_refs (msgs_todo)
          )
}

parse_std_refs <- function (msgs) {

    s <- lapply (msgs, function (i) {
                i <- strsplit (i, "\\]") [[1]] [1]
                i <- strsplit (i, "\\[") [[1]] [2]
                i <- strsplit (i, ",\\s?") [[1]]

                chk <- grepl ("[A-Z]+[0-9]+\\.[0-9](+?[a-z]?)", i)
                if (any (!chk)) {
                    stop ("Standard references [",
                          paste0 (i [which (!chk)], collapse = ", "),
                          "] are not in proper format.")
                }

                return (i)
                   })

    return (sort (unique (unlist (s))))
}

get_categories <- function (stds) {

    categories <- unique (vapply (strsplit (stds, "[0-9]"),
                                  function (i) i [[1]],
                                  character (1)))

    all_cats <- srr_stats_categories ()
    if (any (!categories %in% all_cats$std_prefix))
        stop ("There are no standards with prefix [",
              paste0 (categories [which (!categories %in% all_cats$std_prefix)],
                      collapse = ", "),
              "]", call. = FALSE)

    all_cats [match (categories, all_cats$std_prefix), ]
}

get_standard_nums <- function (category) {

    s <- dl_standards (category, quiet = TRUE)
    s <- format_standards (s)

    # Then extract standard numbers only
    s <- s [grep ("\\-\\s+\\[\\s\\]\\s\\*\\*[A-Z]", s)]
    # explicit gsub operations for clarity:
    s <- gsub ("^\\s+\\-", "-", s)
    # remove first checkbox bits:
    s <- gsub ("^\\-\\s+\\[\\s\\]\\s\\*\\*", "", s)
    # then extract standard numbers only
    m <- gregexpr ("^[A-Z]+[0-9]+\\.[0-9]([0-9]?)([a-z]?)", s)
    s <- unlist (regmatches (s, m))

    return (s)
}
