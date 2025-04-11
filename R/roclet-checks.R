# functions to check that no standards are listed with multiple tags

msgs_to_standards <- function (msgs) {
    res <- lapply (msgs, function (i) {
        re <- regexpr ("(?=\\[).*?(?<=\\])", i, perl = T)
        res <- gsub ("^\\[|\\]$", "", regmatches (i, re))
        strsplit (res, ",\\s") [[1]]    })
    return (unique (unlist (res)))
}

mixed_tag_error <- function (stds1, stds2, tag1, tag2) {

    index <- which (stds1 %in% stds2)
    if (length (index) > 0) {
        stop (
            "Standards [", paste0 (stds1 [index], collapse = ", "),
            "] are listed with both ", tag1, " and ", tag2, " tags.\n",
            "Please rectify to ensure these standards are only ",
            "associated with one tag.",
            call. = FALSE
        )
    }

    index <- which (stds2 %in% stds1)
    if (length (index) > 0) {
        stop (
            "Standards [", paste0 (stds2 [index], collapse = ", "),
            "] are listed with both ", tag1, " and ", tag2, " tags.\n",
            "Please rectify to ensure these standards are only ",
            "associated with one tag.",
            call. = FALSE
        )
    }

    return (TRUE)
}

check_no_mixed_tags <- function (msgs, msgs_na, msgs_todo) {

    stds <- msgs_to_standards (msgs)
    stds_na <- msgs_to_standards (msgs_na)
    stds_todo <- msgs_to_standards (msgs_todo)

    chk <- mixed_tag_error (stds, stds_na, "@srrstats", "@srrstatsNA")
    chk <- mixed_tag_error (stds, stds_todo, "@srrstats", "@srrstatsTODO")
    chk <- mixed_tag_error (stds_na, stds_todo, "@srrstatsNA", "@srrstatsTODO")

    invisible (chk)
}

#' Check that reported text for each standard is sufficiently different from
#' source text.
#'
#' @param msgs Result of `get_all_msgs()` function.
#' @param std_txt_src Result of `get_stds_txt()` function.
#' @return Average proportion of *unchanged* text per standard.
#' @noRd
std_txt_change <- function (msgs, std_txt_src) {

    std_txt_src$comment <- std_txt_src$tag <- character (nrow (std_txt_src))

    for (i in c ("", "_na", "_todo")) {
        std_nums <- unlist (msgs [[paste0 ("std_num", i)]])
        std_txt <- unlist (msgs [[paste0 ("std_txt", i)]])
        index <- match (std_nums, std_txt_src$std)
        # remove NA's for missing standards:
        std_txt <- std_txt [which (!is.na (index))]
        index <- index [which (!is.na (index))]
        std_txt_src$comment [index] <- std_txt
        tag_ext <- ifelse (nzchar (i), gsub ("\\_", "", toupper (i)), i)
        std_txt_src$tag [index] <- paste0 ("srrstats", tag_ext)
    }

    txt_change <- apply (std_txt_src, 1, function (i) {
        i <- tolower (gsub ("[[:punct:]]", "", i))
        src <- strsplit (i [2], "[[:space:]]") [[1]]
        target <- strsplit (i [4], "[[:space:]]") [[1]]
        index <- pmatch (src, target)
        index <- index [which (!is.na (index))]
        # Then get longest sequence:
        max_len <- 0
        if (length (index) > 0) {
            index_ref <- rep (0L, length (index))
            index_ref [which (c (0L, diff (index)) < 0)] <- 1L
            max_len <- max (table (index_ref))
            max_len <- max_len / max (c (length (src), length (target)))
        }
        return (max_len)
    })

    mean (txt_change)
}
