#' Perform pre-submission checks
#'
#' Check that all standards are present in code, and listed either as
#' '@srrstats' or '@srrstatsNA'
#' @param path Path to local repository to check
#' @param quiet If 'FALSE', display information on status of package on screen.
#' @return (Invisibly) List of any standards missing from code
#' @family helper
#' @export
#' @examples
#' d <- srr_stats_pkg_skeleton ()
#' # The skeleton has 'TODO' standards, and also has only a few from the full
#' # list expected for the categories specified there.
#' srr_stats_pre_submit (d)
srr_stats_pre_submit <- function (path, quiet = FALSE) {

    msg <- ""

    stds_in_code <- get_stds_from_code (path)
    no_stds <- all (vapply (stds_in_code, is.null, logical (1)))
    if (no_stds) {
        msg <- "This package has no 'srr' standards"
        if (!quiet)
            cli::cli_alert_warning (msg)
        return (invisible ())
    }

    #all_stds_in_code <- unique (unlist (stds_in_code))

    if (length (stds_in_code$stds_todo) > 0) {

        msg <- paste0 ("This package still has TODO ",
                       "standards and can not be submitted")
        if (!quiet)
            cli::cli_alert_warning (msg)
    }

    categories <- get_categories (unique (do.call (c, stds_in_code)))

    all_stds <- unlist (lapply (categories$category, get_standard_nums))

    index <- which (!all_stds %in% unique (unlist (stds_in_code)))
    if (length (index) > 0) {
        msg1 <- paste0 ("Package can not be submitted because the ",
                        "following standards are missing from your code:")

        if (!quiet) {
            cli::cli_alert_warning (msg1)
            cli::cli_ol ()
            for (i in index)
                cli::cli_li (all_stds [i])
            cli::cli_end ()
        }
        msg <- c (msg,
                  msg1,
                  "",
                  all_stds [index],
                  "")
    } else if (length (stds_in_code$stds_todo) == 0) {
        msg <- "This package is ready to submit!"
        if (!quiet)
            cli::cli_alert_success (msg)
    }

    invisible (msg)
}

get_stds_from_code <- function (path) {

    if (!dir.exists (file.path (path, "R"))) {
        warning ("Directory [", path, "] does not appear to be an R package")
        return (NULL)
    }

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

    s <- dl_standards(category, quiet = TRUE)
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
