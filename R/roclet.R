#' srr_stats_roclet
#'
#' Get values of all `srrstats` tags in function documentation
#'
#' Note that this function should never need to be called directly. It only
#' exists to enable "@srrstats" tags to be parsed from \pkg{roxygen2}
#' documentation.
#'
#' @importFrom roxygen2 roclet
#' @return A \pkg{roxygen2} roclet
#' @family roxygen
#'
#' @examples
#' srr_stats_roclet ()
#' @export
srr_stats_roclet <- function () {
    roxygen2::roclet ("srr_stats")
}

#' @importFrom roxygen2 roclet_process
#'
#' @export
roclet_process.roclet_srr_stats <- function (x, blocks, env, base_path) { # nolint

    if (!get_verbose_flag (blocks)) {
        return (NULL)
    }

    blocks <- collect_blocks (blocks, base_path)

    # ------ @srrstats tags:
    msgs <- collect_one_tag (base_path, blocks, tag = "srrstats")
    msgs_na <- collect_one_tag (base_path, blocks, tag = "srrstatsNA")
    msgs_todo <- collect_one_tag (base_path, blocks,
        tag = "srrstatsTODO"
    )

    num_stds <- function (m) {
        stds <- regmatches (m, gregexpr ("\\[(.*?)\\]", m))
        stds <- lapply (stds, function (i) {
            strsplit (gsub ("^\\[|\\]$", "", i), ",\\s?") [[1]]
        })
        length (unlist (stds))
    }
    num_mgs <- num_stds (msgs)
    num_mgs_na <- num_stds (msgs_na)
    num_mgs_todo <- num_stds (msgs_todo)
    num_total <- num_mgs + num_mgs_na + num_mgs_todo

    check_no_mixed_tags (msgs, msgs_na, msgs_todo)

    has_output <- (length (msgs) > 0L |
        length (msgs_na) > 0L |
        length (msgs_todo) > 0L)
    if (has_output) {

        txt <- "rOpenSci Statistical Software Standards"
        message (cli::rule (center = cli::col_green (txt), line_col = "green"))
    }

    if (length (msgs) > 0L) {
        cli::cli_h3 ("@srrstats standards ({num_mgs} / {num_total}):")
        print_one_msg_list (msgs)
    }

    if (length (msgs_na) > 0L) {
        cli::cli_h3 ("@srrstatsNA standards ({num_mgs_na} / {num_total}):")
        print_one_msg_list (msgs_na)
    }

    if (length (msgs_todo) > 0L) {
        cli::cli_h3 ("@srrstatsTODO standards ({num_mgs_todo} / {num_total}):")
        print_one_msg_list (msgs_todo)
    }
    if (has_output) {
        message (cli::rule (line_col = "green"))
    }

    return (NULL)
}

collect_blocks <- function (blocks, base_path) {

    rcpp <- vapply (
        blocks, function (block) {
            basename (block$file) == "RcppExports.R"
        },
        logical (1)
    )
    rcpp_blocks <- blocks [which (rcpp)]
    blocks <- blocks [which (!rcpp)]

    file_paths <- vapply (blocks, function (i) {
        gsub (base_path, "", i$file)
    }, character (1), USE.NAMES = FALSE)
    re <- regexpr ("^.*\\/", file_paths)
    file_dirs <- rep (".", length (file_paths))
    index <- which (re > 0)
    file_dirs [index] <- regmatches (file_paths, re)
    file_dirs <- gsub ("^\\/", "", file_dirs)
    file_dirs <- gsub ("\\/.*$", "", file_dirs)

    readme_blocks <- blocks [which (file_dirs == "")]
    test_blocks <- blocks [which (file_dirs == "tests")]
    r_blocks <- blocks [which (file_dirs == "R")]
    vignette_blocks <- blocks [which (file_dirs == "vignettes")]
    inst_blocks <- blocks [which (file_dirs == "inst")]

    blocks <- list (
        R = r_blocks,
        src = rcpp_blocks,
        tests = test_blocks,
        inst = inst_blocks,
        vignettes = vignette_blocks,
        readme = readme_blocks
    )

    return (blocks)
}

get_verbose_flag <- function (blocks) {

    n <- vapply (
        blocks, function (i) {
            length (roxygen2::block_get_tags (i, "srrstatsVerbose"))
        },
        integer (1)
    )
    if (sum (n) > 1) {
        stop (
            "There must be only one @srrstatsVerbose flag ",
            "in your documentation"
        )
    }

    if (sum (n) == 0) {
        return (TRUE)
    }

    block <- blocks [[which (n == 1)]]
    flag <- roxygen2::block_get_tags (block, "srrstatsVerbose") [[1]]$val

    if (is.na (as.logical (flag))) {
        stop (
            "The @srrstatsVerbose tag should only have ",
            "'TRUE' or 'FALSE' after it"
        )
    }

    return (as.logical (flag))
}

parse_one_msg_list <- function (msgs, block, tag, fn_name = TRUE, dir = "R") {

    if (length (roxygen2::block_get_tags (block, tag)) > 0L) {
        msgs <- c (
            msgs,
            process_srrstats_tags (
                tag = tag,
                block = block,
                fn_name = fn_name,
                dir = dir
            )
        )
    }

    return (msgs)
}

print_one_msg_list <- function (msgs) {

    if (length (msgs) > 0L) {
        message (paste0 ("  * ", msgs, collapse = "\n"), sep = "")
    }
}

# Collect all messages for one tag
collect_one_tag <- function (base_path, blocks, tag = "srrstats") {

    msgs <- list ()
    for (block in blocks$R) {
        msgs <- parse_one_msg_list (msgs, block, tag = tag, fn_name = TRUE)
    }
    msgs <- c (
        msgs,
        get_other_tags (blocks$tests, tag = tag, dir = "tests/testthat"),
        get_other_tags (blocks$inst, tag = tag, dir = "inst")
    )
    msgs <- c (msgs, get_src_tags (blocks$src, base_path, tag = tag))
    msgs <- c (msgs, get_other_tags (blocks$readme, tag = tag, dir = "."))
    msgs <- c (msgs, get_other_tags (blocks$vignettes, tag = tag, dir = "vignettes"))

    return (msgs)
}


#' check_block_title
#'
#' Ensure that standards with either 'srrstats' or 'srrstatsTODO' are NOT in a
#' block with a title of 'NA_standards'
#'
#' @noRd
check_block_title <- function (block, tag) {

    block_title <- roxygen2::block_get_tag_value (block, "title")
    block_title <- ifelse (is.null (block_title), "", block_title)
    if (tag != "srrstatsNA" && grepl ("^NA\\_st", block_title)) {
        stop (paste0 (
            "An NA_standards block should only contain ",
            "'@srrstatsNA' tags, and no '@",
            tag, "' tags."
        ))
    } else if (tag == "srrstatsNA" & !block_title == "NA_standards") {
        stop (
            "@srrstatsNA tags should only appear in ",
            "a block with a title of NA_standards"
        )
    }
}

#' process_srrstats_tags
#'
#' @param fn_name Include name of calling function in message?
#' @noRd
process_srrstats_tags <- function (tag = "srrstats", block,
                                   fn_name = TRUE, dir = "R") {

    check_block_title (block, tag)

    standards <- roxygen2::block_get_tags (block, tag)
    standards <- unlist (lapply (standards, function (i) i$val))
    snum <- extract_standard_numbers (standards)

    block_backref <- get_block_backref (block)
    block_line <- block$line

    msg <- paste0 ("[", paste0 (snum, collapse = ", "), "]")
    if (fn_name) {
        func_name <- block$object$alias
        if (!is.null (func_name)) {
            msg <- paste0 (msg, " in function '", func_name, "()'")
        }
    }
    ptn <- paste0 ("^.*", dir, "\\/")
    if (grepl (ptn, block$file)) {
        fpath <- regmatches (block$file, regexpr (ptn, block$file))
        term_ptn <- "/"
    } else {
        # Generally only 'tests/testthat.R' where 'dir = tests/testthat'
        term_ptn <- paste0 ("\\.", tools::file_ext (block$file))
        ptn <- paste0 ("^.*", dir, term_ptn, "$")
        fpath <- regmatches (block$file, regexpr (ptn, block$file))
    }
    fpath_full <- gsub (fpath, paste0 (dir, term_ptn), block$file)

    msg <- paste0 (
        msg, " on line#", block_line,
        " of file [",
        fpath_full,
        "]"
    )

    return (msg)
}


# extract the actual standards numbers from arbitrary text strings, first
# capturing everything inside first "[...]":
extract_standard_numbers <- function (standards) {

    # roxygen parses markdown "**A**" as "\\strong{A}", and the curly braces
    # muck up standards ID, so have to be removed here:
    g <- gregexpr ("\\\\(strong|emph)\\{[A-Z]+[0-9]+(\\.[0-9]+)?\\}", standards)
    m <- lapply (regmatches (standards, g), function (i) {
        res <- paste0 (i, collapse = "|")
        res <- gsub ("\\\\(strong|emph)", "\\\\\\\\(strong|emph)", res)
        res <- gsub ("\\{", "\\\\{", res)
        return (gsub ("\\}", "\\\\}", res))
    })
    for (i in seq_along (m)) {
        standards [i] <- gsub (m [[i]], "", standards [i])
    }

    # These use regexpr and not gregexpr to only match first '{...}' while
    # ignoring all subsequent ones
    g_open <- regexpr ("\\{[A-Z]+[0-9]+\\.[0-9]+([a-z]?)", standards)
    g_close <- regexpr ("[A-Z]+[0-9]+\\.[0-9]+([a-z]?)\\}", standards)
    g_close <- g_close + attr (g_close, "match.length") - 1
    standards <- gsub ("\\{|\\}", "", substring (standards, g_open, g_close))
    standards <- gsub ("\\s*", "", unlist (strsplit (standards, ",")))
    if (length (standards) < 1) {
        stop ("srrstats tags found but no correctly-formatted standard numbers")
    }

    return (standards)
}

get_block_backref <- function (block, base_path = NULL) {

    block_backref <- roxygen2::block_get_tag_value (block, "backref")

    if (is.null (block_backref)) {
        block_backref <- block$file
    }
    if (!is.null (base_path)) {
        block_backref <- gsub (base_path, "", block_backref)
    } else {
        block_backref <- basename (block_backref)
    }

    return (block_backref)
}

get_src_tags <- function (blocks, base_path, tag = "srrstats") {

    n <- vapply (
        blocks, function (i) {
            length (roxygen2::block_get_tags (i, tag))
        },
        integer (1)
    )
    blocks <- blocks [which (n > 0)]

    msgs <- list ()

    src_files <- list.files (file.path (base_path, "src"),
        pattern = "\\.cpp$|\\.hpp$|.h$",
        full.names = TRUE
    )
    src_files <- src_files [-grep ("RcppExports.cpp", src_files)]

    for (block in blocks) { # usually only 1 block for "RcppExports.R"

        block_tags <- roxygen2::block_get_tags (block, tag)

        for (tag in block_tags) {

            tag_txt <- paste0 (tag$tag, "\\s+", tag$val)
            tag_txt <- gsub ("\\{", "\\\\{", tag_txt)
            tag_txt <- gsub ("\\}", "\\\\}", tag_txt)
            which_file <- vapply (
                src_files, function (f) {
                    any (grepl (tag_txt, readLines (f)))
                },
                logical (1)
            )
            this_src <- src_files [which (which_file)]
            if (length (this_src) > 1) {
                this_src <- grep ("\\.cpp", this_src, value = TRUE)
            }

            src_lines <- readLines (this_src)
            line_num <- grep (tag_txt, src_lines)
            roxy_lines <- grep ("\\/\\/\\'", src_lines)
            index <- cumsum (c (FALSE, diff (roxy_lines) > 1))
            roxy_lines <- split (roxy_lines, index)
            this_group <- which (vapply (
                roxy_lines, function (i) {
                    line_num %in% i
                },
                logical (1)
            ))
            roxy_lines <- roxy_lines [[this_group]]

            src_lines <- src_lines [(max (roxy_lines) + 1):length (src_lines)]
            while (src_lines [1] == "" ||
                grepl ("Rcpp::export", src_lines [1])) {
                src_lines <- src_lines [-1]
            }
            this_fn <- strsplit (src_lines [1], "\\s") [[1]] [2]

            snum <- extract_standard_numbers (tag$val)

            this_src <- file.path ("src", basename (this_src))
            msgs <- c (msgs, paste0 (
                "[", paste0 (snum, collapse = ", "),
                "] in function '", this_fn,
                "()' on line#", line_num, " of file [",
                file.path ("src", basename (this_src)),
                "]"
            ))

        } # end for tag in block_tags
    } # end for block in blocks

    return (msgs)
}

get_other_tags <- function (blocks, tag = "srrstats", dir = "tests") {

    msgs <- list ()

    for (block in blocks) {

        msgs <- parse_one_msg_list (
            msgs,
            block,
            tag = tag,
            fn_name = FALSE,
            dir = dir
        )
    }

    return (msgs)
}

#' @importFrom roxygen2 roclet_output
#'
#' @export
roclet_output.roclet_srr_stats <- function (x, results, base_path, ...) { # nolint
    return (NULL)
}
