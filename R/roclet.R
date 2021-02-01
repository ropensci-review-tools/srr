
#' rssr_roclet
#'
#' Get values of all `rssr` tags in function documentation
#'
#' Note that this function should never need to be called directly. It only
#' exists to enable "@rssr" tags to be parsed from \pkg{roxygen2} documentation.
#'
#' @importFrom roxygen2 roclet
#'
#' @export
rssr_roclet <- function () {
    roxygen2::roclet ("rssr")
}

#' @importFrom roxygen2 roclet_process
#'
#' @export
roclet_process.roclet_rssr <- function (x, blocks, env, base_path) { # nolint

    if (!get_verbose_flag (blocks))
        return (NULL)

    rcpp <- vapply (blocks, function (block)
                    basename (block$file) == "RcppExports.R",
                    logical (1))
    rcpp_blocks <- blocks [which (rcpp)]
    blocks <- blocks [which (!rcpp)]

    # test blocks:
    test_blocks <- get_test_blocks (base_path)

    msgs <- msgs_na <- msgs_todo <- list () # nolint

    for (block in blocks) {

        msgs <- parse_one_msg_list (msgs, block, "rssr", fn_name = TRUE)

        msgs_na <- parse_one_msg_list (msgs_na, block, "rssrNA")

        msgs_todo <- parse_one_msg_list (msgs_todo, block, "rssrTODO")
    }

    if (length (msgs) > 0L | length (msgs_na) > 0L | length (msgs_todo))
    {
        txt <- "rOpenSci Statistical Software Standards"
        message (cli::rule (center = cli::col_green (txt), line_col = "green"))
    }

    if (length (msgs) > 0L |
        length (msgs_na) > 0L |
        length (msgs_todo) > 0L) {

        cli::cli_h3 ("/R files")

        print_one_msg_list (msgs)
        print_one_msg_list (msgs_na)
        print_one_msg_list (msgs_todo)
    }

    msgs <- get_test_tags (test_blocks, tag = "rssr")
    msgs_na <- get_test_tags (test_blocks, tag = "rssrNA")
    msgs_todo <- get_test_tags (test_blocks, tag = "rssrTODO")

    if (length (msgs) > 0L |
        length (msgs_na) > 0L |
        length (msgs_todo) > 0L) {

        cli::cli_h3 ("/tests files")

        print_one_msg_list (msgs)
        print_one_msg_list (msgs_na)
        print_one_msg_list (msgs_todo)
    }

    msgs <- get_src_tags (rcpp_blocks, base_path, tag = "rssr")
    msgs_na <- get_src_tags (rcpp_blocks, base_path, tag = "rssrNA")
    msgs_todo <- get_src_tags (rcpp_blocks, base_path, tag = "rssrTODO")

    if (length (msgs) > 0L |
        length (msgs_na) > 0L |
        length (msgs_todo) > 0L) {

        cli::cli_h3 ("/src files")

        print_one_msg_list (msgs)
        print_one_msg_list (msgs_na)
        print_one_msg_list (msgs_todo)
    }

    return (NULL)
}

get_verbose_flag <- function (blocks) {

    n <- vapply (blocks, function (i)
                 length (roxygen2::block_get_tags (i, "rssrVerbose")),
                 integer (1))
    if (sum (n) > 1)
        stop ("There must be only one @rssrVerbose flag in your documentation")

    if (sum (n) == 0)
        return (TRUE)

    block <- blocks [[which (n == 1)]]
    flag <- roxygen2::block_get_tags (block, "rssrVerbose") [[1]]$val

    if (is.na (as.logical (flag)))
        stop ("The @rssrVerbose tag should only have ",
              "'TRUE' or 'FALSE' after it")

    return (as.logical (flag))
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

#' process_rssr_tags
#'
#' @param fn_name Include name of calling function in message?
#' @noRd
process_rssr_tags <- function (block, fn_name = TRUE, dir = "R") {

    func_name <- block$object$alias
    standards <- roxygen2::block_get_tag_value (block, "rssr")
    if (grepl ("\\n", standards)) {
        standards <- strsplit (standards, "\\n") [[1]]
        has_commas <- grepl ("\\,", standards)
        last_entry <- has_commas [length (has_commas)]
        has_commas <- has_commas [-length (has_commas)]
        if (!all (has_commas))
            stop ("Each @rssr standard should be separated by a comma.")
        if (last_entry)
            stop ("It appears you've got a comma after the last @rssr entry")
    }
    standards <- unlist (strsplit (standards, ","))

    snum <- extract_standard_numbers (standards)

    block_backref <- get_block_backref (block)
    block_line <- block$line

    msg <- paste0 ("Standards [", paste0 (snum, collapse = ", "), "]")
    if (fn_name)
        msg <- paste0 (msg, " in function '", func_name, "()'")
    msg <- paste0 (msg, " on line#", block_line,
                   " of file [",
                   file.path (dir, basename (block_backref)), "]")

    return (msg)
}

# extract the actual standards numbers from arbitrary text strings:
extract_standard_numbers <- function (standards) {

    gptn <- "[A-Z]+[0-9]+(\\.[0-9]+)?[a-z]?(\\s||\\n\\*)"
    snum <- lapply (standards, function (i) {
                     res <- gregexpr (gptn, i) [[1]]
                     std_start <- as.integer (res)
                     std_end <- std_start + attr (res, "match.length") - 1
                     substring (i, std_start, std_end)  })
    snum <- gsub ("\\s+", "", unlist (snum))

    return (snum)
}

#' process_rssr_NA_tags
#'
#' @param fn_name Just a dummy here to allow do.call
#' @noRd
process_rssrNA_tags <- function (block, fn_name = TRUE, dir = "R") { # nolint

    block_title <- roxygen2::block_get_tag_value (block, "title")
    if (!block_title == "NA_standards")
        stop ("@rssrNA tags should only appear in ",
              "a block with a title of NA_standards")

    standards <- roxygen2::block_get_tags (block, "rssrNA")
    standards <- unlist (lapply (standards, function (i) i$val))
    snum <- extract_standard_numbers (standards)
    #standards <- gsub ("\\s.*$", "", standards)

    block_backref <- get_block_backref (block)
    block_line <- block$line

    msg <- paste0 ("NA Standards [", paste0 (snum, collapse = ", "),
                   "] on line#", block_line,
                   " of file [",
                   file.path (dir, basename (block_backref)), "]")

    return (msg)
}

#' process_rssr_TODO_tags
#'
#' @param fn_name Just a dummy here to allow do.call
#' @noRd
process_rssrTODO_tags <- function (block, fn_name = TRUE, dir = "R") { # nolint

    block_title <- roxygen2::block_get_tag_value (block, "title")
    if (!block_title == "rssr_standards")
        stop ("@rssrTODO and @rssrNA tags should only appear ",
              "in a block with a title of rssr_standards")

    standards <- roxygen2::block_get_tags (block, "rssrTODO")
    standards <- unlist (lapply (standards, function (i) i$val))
    #standards <- gsub ("\\s.*$", "", standards)
    snum <- extract_standard_numbers (standards)

    block_backref <- get_block_backref (block)
    block_line <- block$line

    msg <- paste0 ("TODO Standards [", paste0 (snum, collapse = ", "),
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

get_src_tags <- function (blocks, base_path, tag = "rssr") {

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
            msgs <- c (msgs, paste0 ("Standards [",
                                     paste0 (snum, collapse = ", "),
                                     "] in function '", this_fn,
                                     "# on line#", line_num, " of file [",
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

get_test_tags <- function (blocks, tag = "rssr") {

    msgs <- list () # nolint

    for (block in blocks) {

        msgs <- parse_one_msg_list (msgs,
                                    block,
                                    tag = tag,
                                    fn_name = FALSE,
                                    dir = "tests")

    }

    return (msgs)
}

#' @importFrom roxygen2 roclet_output
#'
#' @export
roclet_output.roclet_rssr <- function (x, results, base_path, ...) { # nolint
    return (NULL)
}
