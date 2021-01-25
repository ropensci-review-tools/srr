
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

    msgs <- msgsNA <- msgsTODO <- list () # nolint

    for (block in blocks) {

        msgs <- parse_one_msg_list (msgs, block, "rssr")

        msgsNA <- parse_one_msg_list (msgsNA, block, "rssrNA")

        msgsTODO <- parse_one_msg_list (msgsTODO, block, "rssrTODO")
    }

    if (length (msgs) > 0L | length (msgsNA) > 0L | length (msgsTODO))
        message ("rOpenSci Statistical Software Standards:")

    print_one_msg_list (msgs)
    print_one_msg_list (msgsNA)
    print_one_msg_list (msgsTODO)

    tags <- get_test_tags (base_path)

    if (length (tags$msgs) > 0L |
        length (tags$msgsNA) > 0L |
        length (tags$msgsTODO) > 0L) {

        message ("\ntests files:")

        print_one_msg_list (tags$msgs)
        print_one_msg_list (tags$msgsNA)
        print_one_msg_list (tags$msgsTODO)
    }

    return (NULL)
}

get_verbose_flag <- function (blocks) {

    n <- vapply (blocks, function (i)
                 length (roxygen2::block_get_tags (i, "rssrVerboseDoc")),
                 integer (1))
    if (sum (n) > 1)
        stop ("There must be only one @rssrVerboseDoc flag in your documentation")

    if (sum (n) == 0)
        return (TRUE)

    block <- blocks [[which (n == 1)]]
    flag <- roxygen2::block_get_tags (block, "rssrVerboseDoc") [[1]]$val

    if (is.na (as.logical (flag)))
        stop ("The @rssrVerboseDoc tag should only have 'TRUE' or 'FALSE' after it")

    return (as.logical (flag))
}

parse_one_msg_list <- function (msgs, block, tag) {

    if (length (roxygen2::block_get_tags (block, tag)) > 0L) {
        fn_name <- paste0 ("process_", tag, "_tags")
        msgs <- c (msgs, do.call (fn_name, list (block)))
    }

    return (msgs)
}

print_one_msg_list <- function (msgs) {

    if (length (msgs) > 0L) {
        message ("")
        message (paste0 ("  * ", msgs, collapse = "\n"), sep = "")
    }
}

process_rssr_tags <- function (block) {

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

    block_backref <- get_block_backref (block)
    block_line <- block$line

    msg <- paste0 ("Standards [", standards,
                   "] in function '", func_name,
                   "()' on line#", block_line,
                   " of file [", basename (block_backref), "]")

    return (msg)
}

process_rssrNA_tags <- function (block) { # nolint

    block_title <- roxygen2::block_get_tag_value (block, "title")
    if (!block_title == "NA_standards")
        stop ("@rssrNA tags should only appear in ",
              "a block with a title of NA_standards")

    standards <- roxygen2::block_get_tags (block, "rssrNA")
    standards <- unlist (lapply (standards, function (i) i$val))
    standards <- gsub ("\\s.*$", "", standards)

    block_backref <- get_block_backref (block)
    block_line <- block$line

    msg <- paste0 ("NA Standards [", paste0 (standards, collapse = ", "),
                   "] on line#", block_line,
                   " of file [", basename (block_backref), "]")

    return (msg)
}

process_rssrTODO_tags <- function (block) { # nolint

    block_title <- roxygen2::block_get_tag_value (block, "title")
    if (!block_title == "rssr_standards")
        stop ("@rssrTODO and @rssrNA tags should only appear ",
              "in a block with a title of rssr_standards")

    standards <- roxygen2::block_get_tags (block, "rssrTODO")
    standards <- unlist (lapply (standards, function (i) i$val))
    standards <- gsub ("\\s.*$", "", standards)

    block_backref <- get_block_backref (block)
    block_line <- block$line

    msg <- paste0 ("TODO Standards [", paste0 (standards, collapse = ", "),
                   "] on line#", block_line,
                   " of file [", basename (block_backref), "]")

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

get_test_tags <- function (base_path) {

    test_dir <- file.path (base_path, "tests")

    flist <- list.files (test_dir,
                         pattern = "\\.R$",
                         recursive = TRUE,
                         full.names = TRUE)
    blocks <- lapply (flist, function (i) roxygen2::parse_file (i, env = NULL))
    names (blocks) <- flist
    blocks <- do.call (c, blocks)

    msgs <- msgsNA <- msgsTODO <- list () # nolint

    for (block in blocks) {

        msgs <- parse_one_msg_list (msgs, block, "rssr")

        msgsNA <- parse_one_msg_list (msgsNA, block, "rssrNA")

        msgsTODO <- parse_one_msg_list (msgsTODO, block, "rssrTODO")
    }

    return (list (msgs = msgs,
                  msgsNA = msgsNA,
                  msgsTODO = msgsTODO))
}

#' @importFrom roxygen2 roclet_output
#'
#' @export
roclet_output.roclet_rssr <- function (x, results, base_path, ...) { # nolint
    return (NULL)
}
