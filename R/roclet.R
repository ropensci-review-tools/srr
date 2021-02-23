
#' srr_stats_roclet
#'
#' Get values of all `srrstats` tags in function documentation
#'
#' Note that this function should never need to be called directly. It only
#' exists to enable "@srrstats" tags to be parsed from \pkg{roxygen2}
#' documentation.
#'
#' @importFrom roxygen2 roclet
#' @family roxygen
#'
#' @export
srr_stats_roclet <- function () {
    roxygen2::roclet ("srr_stats")
}

#' @importFrom roxygen2 roclet_process
#'
#' @export
roclet_process.roclet_srr_stats <- function (x, blocks, env, base_path) { # nolint

    if (!get_verbose_flag (blocks))
        return (NULL)

    blocks <- collect_blocks (blocks, base_path)

    # ------ @srrstats tags:
    msgs <- collect_one_tag (base_path, blocks, tag = "srrstats")
    msgs_na <- collect_one_tag (base_path, blocks, tag = "srrstatsNA")
    msgs_todo <- collect_one_tag (base_path, blocks,
                                           tag = "srrstatsTODO")

    check_no_mixed_tags (msgs, msgs_na, msgs_todo)

    has_output <- (length (msgs) > 0L |
                   length (msgs_na) > 0L |
                   length (msgs_todo) > 0L)
    if (has_output) {

        txt <- "rOpenSci Statistical Software Standards"
        message (cli::rule (center = cli::col_green (txt), line_col = "green"))
    }

    if (length (msgs) > 0L) {
        cli::cli_h3 ("@srrstats standards:")
        print_one_msg_list (msgs)
    }

    if (length (msgs_na) > 0L) {
        cli::cli_h3 ("@srrstatsNA standards:")
        print_one_msg_list (msgs_na)
    }

    if (length (msgs_todo) > 0L) {
        cli::cli_h3 ("@srrstatsTODO standards:")
        print_one_msg_list (msgs_todo)
    }
    if (has_output) {
        message (cli::rule (line_col = "green"))
    }

    return (NULL)
}

collect_blocks <- function (blocks, base_path) {

    rcpp <- vapply (blocks, function (block)
                    basename (block$file) == "RcppExports.R",
                    logical (1))
    rcpp_blocks <- blocks [which (rcpp)]
    blocks <- blocks [which (!rcpp)]
    test_blocks <- get_test_blocks (base_path)
    readme_blocks <- get_readme_blocks (base_path)

    blocks <- list (R = blocks,
                    src = rcpp_blocks,
                    tests = test_blocks,
                    readme = readme_blocks)

    return (blocks)
}

get_verbose_flag <- function (blocks) {

    n <- vapply (blocks, function (i)
                 length (roxygen2::block_get_tags (i, "srrstatsVerbose")),
                 integer (1))
    if (sum (n) > 1)
        stop ("There must be only one @srrstatsVerbose flag ",
              "in your documentation")

    if (sum (n) == 0)
        return (TRUE)

    block <- blocks [[which (n == 1)]]
    flag <- roxygen2::block_get_tags (block, "srrstatsVerbose") [[1]]$val

    if (is.na (as.logical (flag)))
        stop ("The @srrstatsVerbose tag should only have ",
              "'TRUE' or 'FALSE' after it")

    return (as.logical (flag))
}

get_readme_blocks <- function (base_path) {

    blocks <- NULL

    f <- file.path (base_path, "README.Rmd")
    if (file.exists (f)) {
        fout <- tempfile ()
        rcpp_parse_rmd (f, fout)
        blocks <- roxygen2::parse_file (fout, env = NULL)
        blocks <- lapply (blocks, function (i) {
                              i$file <- f
                              return (i)    })
    }

    return (blocks)
}

parse_one_msg_list <- function (msgs, block, tag, fn_name = TRUE, dir = "R") {

    if (length (roxygen2::block_get_tags (block, tag)) > 0L) {
        call_fn <- paste0 ("process_", tag, "_tags")
        msgs <- c (msgs, do.call (call_fn, list (block = block,
                                                 fn_name = fn_name,
                                                 dir = dir)))
    }

    return (msgs)
}

print_one_msg_list <- function (msgs) {

    if (length (msgs) > 0L) {
        message (paste0 ("  * ", msgs, collapse = "\n"), sep = "")
    }
}

# Collect all messages for one tag
collect_one_tag <- function (base_path, blocks, test_blocks, rcpp_blocks,
                             tag = "srrstats") {

    msgs <- list ()
    for (block in blocks$R) {
        msgs <- parse_one_msg_list (msgs, block, tag = tag, fn_name = TRUE)
    }
    msgs <- c (msgs, get_other_tags (blocks$tests, tag = tag, dir = "tests/testthat"))
    msgs <- c (msgs, get_src_tags (blocks$src, base_path, tag = tag))
    msgs <- c (msgs, get_other_tags (blocks$readme, tag = tag, dir = "."))

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
    if (grepl ("^NA\\_st", block_title))
        stop (paste0 ("An NA_standards block should only contain ",
                      "'@srrstatsNA' tags, and no ",
                      tag, " tags."))

}

#' process_srrstats_tags
#'
#' @param fn_name Include name of calling function in message?
#' @noRd
process_srrstats_tags <- function (block, fn_name = TRUE, dir = "R") {

    func_name <- block$object$alias

    check_block_title (block, "srrstats")

    standards <- roxygen2::block_get_tags (block, "srrstats")
    standards <- unlist (lapply (standards, function (i) i$val))

    snum <- extract_standard_numbers (standards)
    if (length (snum) < 1)
        stop ("srrstats tags found but no correctly-formatted standard numbers")

    block_backref <- get_block_backref (block)
    block_line <- block$line

    msg <- paste0 ("[", paste0 (snum, collapse = ", "), "]")
    if (fn_name)
        msg <- paste0 (msg, " in function '", func_name, "()'")
    msg <- paste0 (msg, " on line#", block_line,
                   " of file [",
                   file.path (dir, basename (block_backref)), "]")

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

    g <- regexpr ("\\{.*\\}\\s", standards)
    standards <- gsub ("\\{|\\}\\s", "", regmatches (standards, g))

    gptn <- "[A-Z]+[0-9]+\\.([0-9]+)?[a-z]?(\\s||\\n\\*)"
    snum <- lapply (standards, function (i) {
                     res <- gregexpr (gptn, i) [[1]]
                     std_start <- as.integer (res)
                     std_end <- std_start + attr (res, "match.length") - 1
                     substring (i, std_start, std_end)  })
    snum <- gsub ("\\s+", "", unlist (snum))

    return (snum)
}

#' process_srrstats_NA_tags
#'
#' @param fn_name Just a dummy here to allow do.call
#' @noRd
process_srrstatsNA_tags <- function (block, fn_name = TRUE, dir = "R") { # nolint

    block_title <- roxygen2::block_get_tag_value (block, "title")
    block_title <- ifelse (length (block_title) == 0L, "", block_title)
    if (!block_title == "NA_standards")
        stop ("@srrstatsNA tags should only appear in ",
              "a block with a title of NA_standards")

    standards <- roxygen2::block_get_tags (block, "srrstatsNA")
    standards <- unlist (lapply (standards, function (i) i$val))
    snum <- extract_standard_numbers (standards)
    #standards <- gsub ("\\s.*$", "", standards)

    block_backref <- get_block_backref (block)
    block_line <- block$line

    msg <- paste0 ("[", paste0 (snum, collapse = ", "),
                   "] on line#", block_line,
                   " of file [",
                   file.path (dir, basename (block_backref)), "]")

    return (msg)
}

#' process_srrstats_TODO_tags
#'
#' @param fn_name Just a dummy here to allow do.call
#' @noRd
process_srrstatsTODO_tags <- function (block, fn_name = TRUE, dir = "R") { # nolint

    check_block_title (block, "srrstatsTODO")

    standards <- roxygen2::block_get_tags (block, "srrstatsTODO")
    standards <- unlist (lapply (standards, function (i) i$val))
    #standards <- gsub ("\\s.*$", "", standards)
    snum <- extract_standard_numbers (standards)

    block_backref <- get_block_backref (block)
    block_line <- block$line

    msg <- paste0 ("[", paste0 (snum, collapse = ", "),
                   "] on line#", block_line,
                   " of file [",
                   file.path (dir, basename (block_backref)), "]")

    return (msg)
}

get_block_backref <- function (block, base_path = NULL) {

    block_backref <- roxygen2::block_get_tag_value (block, "backref")

    if (is.null (block_backref))
        block_backref <- block$file
    if (!is.null (base_path))
        block_backref <- gsub (base_path, "", block_backref)
    else
        block_backref <- basename (block_backref)

    return (block_backref)
}

get_src_tags <- function (blocks, base_path, tag = "srrstats") {

    n <- vapply (blocks, function (i)
                 length (roxygen2::block_get_tags (i, tag)),
                 integer (1))
    blocks <- blocks [which (n > 0)]

    msgs <- list ()

    src_files <- list.files (file.path (base_path, "src"),
                             pattern = "\\.cpp$|\\.hpp$|.h$",
                             full.names = TRUE)
    src_files <- src_files [-grep ("RcppExports.cpp", src_files)]

    for (block in blocks) { # usually only 1 block for "RcppExports.R"

        block_tags <- roxygen2::block_get_tags (block, tag)

        for (tag in block_tags) {

            tag_txt <- paste0 (tag$tag, "\\s+", tag$val)
            tag_txt <- gsub ("\\{", "\\\\{", tag_txt)
            tag_txt <- gsub ("\\}", "\\\\}", tag_txt)
            which_file <- vapply (src_files, function (f)
                                  any (grepl (tag_txt, readLines (f))),
                                  logical (1))
            this_src <- src_files [which (which_file)]
            if (length (this_src) > 1) {
                this_src <- grep ("\\.cpp", this_src, value = TRUE)
            }

            src_lines <- readLines (this_src)
            line_num <- grep (tag_txt, src_lines)
            roxy_lines <- grep ("\\/\\/\\'", src_lines)
            index <- cumsum (c (FALSE, diff (roxy_lines) > 1))
            roxy_lines <- split (roxy_lines, index)
            this_group <- which (vapply (roxy_lines, function (i)
                                         line_num %in% i,
                                         logical (1)))
            roxy_lines <- roxy_lines [[this_group]]

            src_lines <- src_lines [(max (roxy_lines) + 1):length (src_lines)]
            while (src_lines [1] == "" | grepl ("Rcpp::export", src_lines [1]))
                src_lines <- src_lines [-1]
            this_fn <- strsplit (src_lines [1], "\\s") [[1]] [2]

            snum <- extract_standard_numbers (tag$val)

            this_src <- file.path ("src", basename (this_src))
            msgs <- c (msgs, paste0 ("[", paste0 (snum, collapse = ", "),
                                     "] in function '", this_fn,
                                     "()' on line#", line_num, " of file [",
                                     file.path ("src", basename (this_src)),
                                     "]"))

        } # end for tag in block_tags
    } # end for block in blocks

    return (msgs)
}

get_test_blocks <- function (base_path) {


    flist <- list.files (file.path (base_path, "tests"),
                         pattern = "\\.R$",
                         recursive = TRUE,
                         full.names = TRUE)

    blocks <- lapply (flist, function (i) roxygen2::parse_file (i, env = NULL))
    names (blocks) <- flist
    blocks <- do.call (c, blocks)

    return (blocks)
}

get_other_tags <- function (blocks, tag = "srrstats", dir = "tests") {

    msgs <- list ()

    for (block in blocks) {

        msgs <- parse_one_msg_list (msgs,
                                    block,
                                    tag = tag,
                                    fn_name = FALSE,
                                    dir = dir)

    }

    return (msgs)
}

#' @importFrom roxygen2 roclet_output
#'
#' @export
roclet_output.roclet_srr_stats <- function (x, results, base_path, ...) { # nolint
    return (NULL)
}
