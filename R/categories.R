#' Get categories from a list of standard previxes and numbers.
#'
#' @param stds Output of `parse_std_refs()` function, which parses standards
#' texts to retrieve vectors of single references of category code and standards
#' numbers.
#' @noRd
get_categories <- function (stds) {

    if (is.null (stds)) {
        return (NULL)
    }

    categories <- unique (vapply (
        strsplit (stds, "[0-9]"),
        function (i) i [[1]],
        character (1)
    ))

    all_cats <- srr_stats_categories ()
    if (any (!categories %in% all_cats$std_prefix)) {
        stop ("There are no standards with prefix [",
            paste0 (categories [which (!categories %in% all_cats$std_prefix)],
                collapse = ", "
            ),
            "]",
            call. = FALSE
        )
    }

    all_cats [match (categories, all_cats$std_prefix), ]
}

#' Extract all enumerated standards for a nominated category
#' @param category Full standardised string of category for which standards are
#' to be enumerated. (See `list_categories()` function for standardised category
#' nomenclature.)
#' @noRd
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
