#' Generate report from `ssr` tags.
#'
#' @param path Path to package for which report is to be generated
#' @param view If `TRUE` (default), a html-formatted version of the report is
#' opened in default system browser. If `FALSE`, the return object includes the
#' name of a `html`-rendered version of the report in an attribute named 'file'.
#' @param branch By default a report will be generated from the current branch
#' as set on the local git repository; this parameter can be used to specify any
#' alternative branch.
#' @param roxygenise If `TRUE` (default), documentation will first be updated
#' with the \pkg{roxygen2} package. This requires local installation of the
#' package, which may take some time if the package has not previously been
#' installed. If this parameter is `FALSE`, the `roxygen2` package is not used,
#' documentation is not updated, and reports are generally generated faster.
#' @return (invisibly) Markdown-formatted lines used to generate the final html
#' document.
#' @family report
#' @export
#' @examples
#' \dontrun{
#' path <- srr_stats_pkg_skeleton ()
#' srr_report (path)
#' }
srr_report <- function (path = ".", branch = "",
                        view = TRUE, roxygenise = TRUE) {

    if (roxygenise) {
        o <- utils::capture.output (
            chk <- tryCatch (
                roxygen2::roxygenise (path),
                error = function (e) e
            )
        )
        if (methods::is (chk, "simpleError")) {
            stop (chk$message)
        }
    }

    requireNamespace ("rmarkdown")

    if (path == ".") {
        path <- here::here ()
    }
    path <- fs::path_abs (fs::path_expand (path))

    remote <- get_git_remote (path)
    branch <- get_git_branch (path, branch)

    msgs <- get_all_msgs (path)
    if (all (vapply (msgs, length, integer (1L)) == 0L)) {
        return ("This is not an 'srr' package")
    }
    std_txt <- get_stds_txt (msgs)

    # check_num_categories checks that 'srr' docs include at least General plus
    # one other category of standards, and returns message starting with "Error"
    # if not, otherwise an empty string.
    cat_check <- check_num_categories (std_txt$std)

    # count numbers of srr tags, returning counts in different categories
    num_stds <- function (m) {
        stds <- regmatches (m, gregexpr ("\\[(.*?)\\]", m))
        stds <- lapply (stds, function (i) {
            strsplit (gsub ("^\\[|\\]$", "", i), ",\\s?") [[1]]
        })
        stds <- gsub ("[0-9].*$", "", unique (unlist (stds)))
        res <- table (stds)
        if (length (res) == 0L) {
            res <- 0L
        }
        return (res)
    }
    n_srr <- num_stds (msgs$msgs)
    n_na <- num_stds (msgs$msgs_na)
    n_todo <- num_stds (msgs$msgs_todo)

    # Ensure all counts are same lengths:
    categories <- unique (c (names (n_srr), names (n_na), names (n_todo)))
    num_srr <- num_na <- num_todo <- rep (0, length (categories))
    names (num_srr) <- names (num_na) <- names (num_todo) <- categories

    num_srr [match (names (n_srr), categories)] <- n_srr
    num_na [match (names (n_na), categories)] <- n_na
    num_todo [match (names (n_todo), categories)] <- n_todo

    num_total <- num_srr + num_na + num_todo

    tags <- c ("srrstats", "srrstatsNA", "srrstatsTODO")
    md_lines <- lapply (tags, function (tag) {
        res <- one_tag_to_markdown (
            msgs,
            remote,
            tag,
            branch,
            std_txt
        )
        if (length (res) > 0) {

            dirs <- attr (res, "dirs")
            dirs [dirs == "."] <- "root"

            md <- res
            res <- NULL
            for (d in unique (dirs)) {

                res <- c (
                    res,
                    "",
                    paste0 ("### ", d, " directory"),
                    "",
                    unlist (md [which (dirs == d)])
                )
            }

            txt <- tolower (gsub ("srrstats", "", tag))
            if (!nzchar (txt)) {
                txt <- "srr"
            }
            n <- get (paste0 ("num_", txt))
            tag_counts <- lapply (seq_along (n), function (i) {
                paste (
                    "- ",
                    names (n) [i],
                    ": ",
                    n [i],
                    " / ",
                    num_total [i]
                )
            })
            tag_counts <- c (
                unlist (tag_counts),
                paste0 (
                    "- Total : ",
                    sum (n),
                    " / ",
                    sum (num_total)
                )
            )

            res <- c (
                paste0 (
                    "## Standards with `",
                    tag,
                    "` tag"
                ),
                "",
                "**Numbers of standards:**",
                tag_counts,
                "",
                res,
                "",
                "---",
                ""
            )
        }
        return (res)
    })
    md_lines <- unlist (md_lines)

    desc <- data.frame (read.dcf (fs::path (path, "DESCRIPTION")))
    pkg <- desc$Package

    if (is.null (remote)) {
        md_title <- paste0 ("# srr report for ", pkg)
    } else {
        md_title <- paste0 (
            "# srr report for [",
            pkg,
            "](",
            remote,
            ")"
        )
    }

    md_lines <- c (
        md_title,
        "",
        paste0 (
            "[Click here for full text of all standards](",
            "https://stats-devguide.ropensci.org/standards.html)"
        ),
        "",
        cat_check,
        "",
        add_missing_stds (md_lines, std_txt),
        md_lines
    )

    f <- tempfile (fileext = ".Rmd")
    # need explicit line break to html render
    writeLines (paste0 (md_lines, "\n"), con = f)
    out <- paste0 (tools::file_path_sans_ext (f), ".html")
    rmarkdown::render (input = f, output_file = out)
    if (nzchar (cat_check)) {
        # sub markdown fail X for cat_check with HTML
        md <- gsub (
            "\\:heavy\\_multiplication\\_x\\:",
            "&#10060;",
            readLines (out)
        )
        writeLines (md, out)
    }

    u <- paste0 ("file://", out)
    if (view) {
        utils::browseURL (u)
    } else {
        attr (md_lines, "file") <- out
    }

    invisible (md_lines)
}

get_all_msgs <- function (path = ".") {

    flist <- get_all_file_names (path)

    blocks <- lapply (flist, function (i) {
        this_file <- i
        is_rmd <- grepl ("\\.Rmd$", i)
        if (is_rmd) {
            fout <- fs::file_temp ()
            rcpp_parse_rmd (i, fout)
            this_file <- fout
        }
        res <- tryCatch (
            roxygen2::parse_file (this_file, env = NULL),
            error = function (e) NULL # ignore errors and do not parse
        )
        if (is_rmd) {
            res <- lapply (res, function (j) {
                j$file <- i
                return (j)
            })
        }
        return (res)
    })

    failing <- flist [sapply (blocks, inherits, "try-error")]
    if (length (failing) > 0L) {
        stop ("parsing problem in: ", paste (failing, collapse = ", "))
    }

    names (blocks) <- flist
    blocks <- do.call (c, blocks)

    blocks <- collect_blocks (blocks, path)

    tags <- c ("srrstats", "srrstatsNA", "srrstatsTODO")
    res <- lapply (
        tags,
        function (i) collect_one_tag (path, blocks, tag = i)
    )
    msgs <- res [[1]]$message
    msgs_na <- res [[2]]$message
    msgs_todo <- res [[3]]$message

    list (
        msgs = msgs,
        msgs_na = msgs_na,
        msgs_todo = msgs_todo
    )
}

#' Get text of actual standards contained in lists of standards messages
#'
#' @param msgs Result of 'get_all_msgs()' function
#' @noRd
get_stds_txt <- function (msgs) {

    s_msgs <- parse_std_refs (msgs$msgs)
    s_na <- parse_std_refs (msgs$msgs_na)
    s_todo <- parse_std_refs (msgs$msgs_todo)
    # s_todo <- parse_std_refs (msgs$msgs_todo)
    cats_msg <- get_categories (s_msgs$stds)
    cats_na <- get_categories (s_na$stds)
    cats_todo <- get_categories (s_todo$stds)
    # cats_todo <- get_categories (s_todo)
    cats <- unique (c (
        cats_msg$category,
        cats_na$category,
        cats_todo$category
    ))
    s <- get_standards_checklists (cats)
    ptn <- "^\\s?\\-\\s\\[\\s\\]\\s\\*\\*"
    s <- gsub (ptn, "", grep (ptn, s, value = TRUE))
    g <- regexpr ("\\*\\*", s)
    std_nums <- substring (s, 1, g - 1)
    std_txt <- gsub (
        "^\\*|\\*$", "",
        substring (s, g + 3, nchar (s))
    )

    data.frame (
        std = std_nums,
        text = std_txt
    )
}

#' one_tag_to_markdown
#'
#' Convert all messages for one defined tag into multiple markdown-formatted
#' lines
#' @param m List of all messages, divided into the 3 categories of tags
#' @param std_txt Result of 'get_stds_txt' function
#' @noRd
one_tag_to_markdown <- function (m, remote, tag, branch, std_txt) {

    i <- match (tag, c ("srrstats", "srrstatsNA", "srrstatsTODO"))
    tag <- c ("msgs", "msgs_na", "msgs_todo") [i]
    m <- m [[tag]]

    files <- gsub ("^.*of file\\s\\[|\\]$", "", unlist (m))
    dirs <- vapply (
        strsplit (files, .Platform$file.sep),
        function (i) i [1],
        character (1)
    )

    m <- vapply (
        m, function (i) {
            one_msg_to_markdown (i, remote, branch, std_txt)
        },
        character (1)
    )

    ret <- strsplit (m, "\n")
    attr (ret, "dirs") <- dirs

    return (ret)
}

#' one_msg_to_markdown
#'
#' Convert single-entry character vector of one message into one
#' markdown-formatted line
#' @param m One message
#' @noRd
one_msg_to_markdown <- function (m, remote, branch, std_txt) {

    g <- gregexpr ("[A-Z]+[0-9]+\\.[0-9]([0-9]?)([a-z]?)", m)

    stds <- regmatches (m, g) [[1]]
    stds_g <- sort (stds [grep ("^G", stds)])
    stds_other <- sort (stds [!stds %in% stds_g])
    stds <- c (stds_g, stds_other)

    g <- gregexpr ("\\sline#[0-9]+", m)
    line_num <- NA_integer_
    if (any (g [[1]] > 0)) {
        line_num <- gsub ("\\sline#", "", regmatches (m, g) [[1]])
    }

    fn <- NA_character_
    if (grepl ("\\sfunction\\s", m)) {
        g <- gregexpr ("\\sfunction\\s+\\'.*\\'", m)
        fn <- gsub ("^\\sfunction\\s+\\'|\\'$", " ", regmatches (m, g) [[1]])
    }

    g <- gregexpr ("file\\s+\\[.*\\]$", m)
    file_name <- gsub ("file\\s+\\[|\\]$", "", regmatches (m, g) [[1]])

    if (!is.null (remote)) {

        remote_file <- paste0 (remote, "/blob/", branch, "/", file_name)
        if (!is.na (line_num)) {
            remote_file <- paste0 (remote_file, "#L", line_num)
        }
    }

    stds <- stds [which (stds %in% std_txt$std)]
    index <- match (stds, std_txt$std)
    stds <- paste0 (
        "- ", std_txt$std [index],
        " ", std_txt$text [index]
    )

    br_open <- br_close <- ""
    if (!is.null (remote)) {
        br_open <- "["
        br_close <- "]"
    }

    msg <- paste0 ("Standards ")
    if (!is.na (fn)) {
        msg <- paste0 (msg, "in function '", fn, "'")
    }
    if (!is.na (line_num)) {
        msg <- paste0 (msg, "on line#", line_num)
    }
    msg <- paste0 (msg, " of file ", br_open, file_name, br_close)
    if (!is.null (remote)) {
        msg <- paste0 (msg, "(", remote_file, ")")
    }
    msg <- paste0 (msg, ":")

    return (paste0 (c (msg, stds), collapse = "\n"))
}

#' Find any missing standards, first by getting all non-missing standards
#' from md_lines, then matching will std_txt which has all applicable stds
#'
#' @param md_lines Markdown-formatted list of standards addressed in package
#' @param std_txt A `data.frame` of all applicable standards, with columns of
#' `std` and `text`.
#' @return The `md_lines` input potentially modified through additional details
#' of missing standards
#' @noRd
add_missing_stds <- function (md_lines, std_txt) {

    md_stds <- grep (
        "^\\-\\s+[A-Z]+[0-9]+\\.[0-9]+([a-z]?)",
        md_lines,
        value = TRUE
    )
    g <- regexpr ("^\\-\\s+[A-Z]+[0-9]+\\.[0-9]+([a-z]?)", md_stds)
    md_stds <- gsub ("^\\-\\s+", "", regmatches (md_stds, g))
    missing_stds <- std_txt$std [which (!std_txt$std %in% md_stds)]

    missing_md <- NULL
    if (length (missing_stds) > 0) {

        missing_md <- c (
            "## Missing Standards",
            "",
            "The following standards are missing:"
        )

        cats <- get_categories (missing_stds)
        for (i in seq (nrow (cats))) {

            stds_i <- grep (
                paste0 ("^", cats$std_prefix [i]),
                missing_stds,
                value = TRUE
            )

            missing_md <- c (
                missing_md,
                "",
                paste0 (
                    tools::toTitleCase (cats$category [i]),
                    " standards:"
                ),
                "",
                paste0 (stds_i, collapse = ", "),
                ""
            )
        }
    }

    return (missing_md)
}

#' Check that standards document General plus at least one additional category.
#'
#' @param std_codes Character vector of all standards codes included in package.
#' Either the "std" column of result from call to `get_stds_txt` function (for
#' full submissions), or the result of the `stds_in_code()` function (for
#' pre-submissions).
#' @return An empty string is general and category-specific standards are
#' present, otherwise a text message describing missing standards.
#' @noRd
check_num_categories <- function (std_codes) {

    std_cats <- unique (regmatches (std_codes, regexpr ("^[A-Z]+", std_codes)))

    ret <- ""

    if (length (std_cats) < 2) {

        ret <- ":heavy_multiplication_x: Error: "
        if (!"G" %in% std_cats) {
            ret <- paste0 (ret, "No general standards have been documented.")
        } else {
            ret <- paste0 (
                ret,
                "Package documents compliance only with general standards. ",
                "Statistical packages must document compliance with at least ",
                "one set of category-specific standards as well."
            )
        }
    }

    return (ret)
}
