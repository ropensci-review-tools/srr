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

    if (any (grepl ("todo", stds_in_code$std_type))) {

        msg <- paste0 (
            "This package still has TODO ",
            "standards and can not be submitted"
        )
        if (!quiet) {
            cli::cli_alert_warning (msg)
        }
    }

    msg <- c (msg, check_missing_standards (stds_in_code, quiet = quiet))
    msg <- c (msg, check_standards_in_files (stds_in_code, quiet = quiet))

    invisible (msg)
}

get_stds_from_code <- function (path) {

    flist <- get_all_file_names (path)

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

    ret <- list (
        stds = parse_std_refs (msgs, "std"),
        stds_na = parse_std_refs (msgs_na, "na"),
        stds_todo = parse_std_refs (msgs_todo, "todo")
    )

    do.call (rbind, ret)
}

parse_std_refs <- function (msgs, std_type = "srr_stats") {

    s <- lapply (msgs, function (i) {

        stds <- regmatches (i, regexpr ("^\\[.*?\\]", i))
        i <- gsub (stds, "", i, fixed = TRUE)
        stds <- gsub ("^\\[|\\]$", "", stds)
        fname <- regmatches (i, regexpr ("\\[.*?\\]", i))
        fname <- gsub ("^\\[|\\]$", "", fname)
        lnum <- regmatches (i, regexpr ("\\#[0-9]+", i))
        lnum <- as.numeric (gsub ("^\\#", "", lnum))

        i <- strsplit (i, "\\]") [[1]] [1]
        i <- strsplit (i, "\\[") [[1]] [2]
        i <- strsplit (i, ",\\s?") [[1]]

        chk <- grepl ("[A-Z]+[0-9]+\\.[0-9](+?[a-z]?)", stds)
        if (any (!chk)) {
            stop (
                "Standard references [",
                paste0 (i [which (!chk)], collapse = ", "),
                "] are not in proper format."
            )
        }

        stds <- strsplit (stds, ",\\s?") [[1]]

        return (data.frame (
            std_type = rep (std_type, length (stds)),
            stds = stds,
            file = rep (fname, length (stds)),
            line_num = rep (lnum, length (stds))
        ))

    })

    return (do.call (rbind, s))
}

#' Check that documentation of standards is appropriately distributed throughout
#' all files.
#'
#' @param max_proportion The maximal permissible proportion of all standards
#' which may be documented in a single file. Any packages which documented more
#' than this value in a single file will trigger a warning message.
#' @noRd
check_standards_in_files <- function (stds_in_code, max_proprtion = 0.5,
                                      quiet = FALSE) {

    msg <- msg1 <- msg2 <- ""

    s <- stds_in_code [stds_in_code$std_type == "std", ]
    dirs <- table (regmatches (s$file, regexpr ("^.*\\/", s$file)))
    if (length (dirs) < 2) {
        msg1 <- paste0 (
            "Standards should be documented in multiple ",
            "directories, yet are only present in one"
        )
    }

    files <- table (s$file)
    if (max (files) > (max_proprtion * sum (files))) {
        msg2 <- paste0 (
            "Standards should be documented in most package ",
            "files, yet are mostly only documented in one file."
        )
    }

    # msg1 overrides msg2, so this function only issues one of these two!
    if (nzchar (msg2)) {
        msg <- msg2
    } else if (nzchar (msg1)) {
        msg <- msg1
    }

    if (nzchar (msg) & !quiet) {
        cli::cli_alert_warning (msg)
    }

    return (msg)
}

check_missing_standards <- function (stds_in_code, quiet = FALSE) {

    categories <- get_categories (unique (stds_in_code$stds))

    all_stds <- unlist (lapply (categories$category, get_standard_nums))

    index <- which (!all_stds %in% unique (stds_in_code$stds))
    index_not <- which (!unique (stds_in_code$stds) %in% all_stds)
    if (length (index) > 0) {
        msg <- paste0 (
            "Package can not be submitted because the ",
            "following standards [v",
            attr (categories, "stds_version"),
            "] are missing from your code:"
        )

        if (!quiet) {
            cli::cli_alert_warning (msg)
            cli::cli_ol ()
            for (i in index) {
                cli::cli_li (all_stds [i])
            }
            cli::cli_end ()
        }
        msg <- c (
            msg,
            "",
            all_stds [index],
            ""
        )
    } else if (length (index_not) > 0L) {

        # issue#25
        not_a_std <- unique (stds_in_code$stds) [index_not]
        msg <- "Your code includes the following standard"
        if (length (not_a_std) > 1L) {
            msg <- paste0 (msg, "s")
        }
        msg <- paste0 (
            msg, " which are not actual standards: [",
            not_a_std, "]"
        )

    } else if (!any (grepl ("todo", stds_in_code$stds))) {

        msg <- paste0 (
            "All applicable standards [v",
            attr (categories, "stds_version"),
            "] have been documented in this package (",
            length (which (stds_in_code$std_type == "std")),
            " complied with; ",
            length (which (stds_in_code$std_type == "na")),
            " N/A standards)"
        )

        if (!quiet) {
            cli::cli_alert_success (msg)
        }
    }

    return (msg)
}
