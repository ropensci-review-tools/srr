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
srr_stats_pre_submit <- function (path = ".", quiet = FALSE) {

    msg <- ""

    stds_in_code <- tryCatch (
        get_stds_from_code (path),
        error = function (e) e
    )
    if (methods::is (stds_in_code, "error")) {
        msg <- stds_in_code$message
        if (!quiet) {
            cli::cli_alert_warning (msg)
        }
        return (msg)
    }

    no_stds <- all (vapply (stds_in_code, is.null, logical (1)))
    if (no_stds) {
        msg_none <- "This package has no 'srr' standards"
        if (!quiet) {
            cli::cli_alert_warning (msg_none)
        }
        return (invisible ())
    }

    cat_check <- check_num_categories (unlist (stds_in_code)) # in report.R
    if (nzchar (cat_check)) {
        return (cat_check)
    }

    if (length (stds_in_code$stds_todo) > 0) {

        msg <- paste0 (
            "This package still has TODO ",
            "standards and can not be submitted"
        )
        if (!quiet) {
            cli::cli_alert_warning (msg)
        }
    }

    categories <- get_categories (unique (do.call (c, stds_in_code)))

    all_stds <- unlist (lapply (categories$category, get_standard_nums))

    index <- which (!all_stds %in% unique (unlist (stds_in_code)))
    index_not <- which (!unique (unlist (stds_in_code)) %in% all_stds)
    if (length (index) > 0) {
        msg1 <- paste0 (
            "Package can not be submitted because the ",
            "following standards [v",
            attr (categories, "stds_version"),
            "] are missing from your code:"
        )

        if (!quiet) {
            cli::cli_alert_warning (msg1)
            cli::cli_ol ()
            for (i in index) {
                cli::cli_li (all_stds [i])
            }
            cli::cli_end ()
        }
        msg <- c (
            msg,
            msg1,
            "",
            all_stds [index],
            ""
        )
    } else if (length (index_not) > 0L) {

        # issue#25
        not_a_std <- unique (unlist (stds_in_code)) [index_not]
        msg <- "Your code includes the following standard"
        if (length (not_a_std) > 1L) {
            msg <- paste0 (msg, "s")
        }
        msg <- paste0 (
            msg, " which are not actual standards: [",
            not_a_std, "]"
        )

    } else if (length (stds_in_code$stds_todo) == 0) {


        msg <- paste0 (
            "All applicable standards [v",
            attr (categories, "stds_version"),
            "] have been documented in this package (",
            length (stds_in_code$stds),
            " complied with; ",
            length (stds_in_code$stds_na),
            " N/A standards)"
        )

        if (!quiet) {
            cli::cli_alert_success (msg)
        }
    }

    invisible (msg)
}

get_stds_from_code <- function (path) {

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

    suppressWarnings ({
        blocks <- lapply (flist, function (i) {
            tryCatch (roxygen2::parse_file (i, env = NULL),
                error = function (e) list ()
            )
        })
    })
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
            stop (
                "Standard references [",
                paste0 (i [which (!chk)], collapse = ", "),
                "] are not in proper format."
            )
        }

        return (i)
    })

    return (sort (unique (unlist (s))))
}
