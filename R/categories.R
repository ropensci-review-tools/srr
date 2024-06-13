#' Get details of current statistical software categories
#'
#' List all currently available categories and associated URLs to full category
#' descriptions.
#'
#' @return A `data.frame` with 3 columns of "category" (the categories to be
#' submitted to \link{srr_stats_checklist}), "title" (the full title), and
#' "url".
#' @family helper
#' @examples
#' srr_stats_categories ()
#' @export
srr_stats_categories <- function () {

    cats <- std_prefixes ()
    cat_full <- unlist (lapply (cats$category, function (i) {
        category_titles_urls (i)
    }))

    version <- stds_version ()

    index <- seq (length (cat_full) / 2) * 2

    res <- data.frame (
        category = cats$category,
        std_prefix = cats$prefix,
        title = cat_full [index - 1],
        url = cat_full [index],
        stringsAsFactors = FALSE
    )

    attr (res, "stds_version") <- version

    return (res)
}


#' @return List of all current categories as obtained from directory contents of
#' https://github.com/ropensci/statistical-software-review-book/tree/main/standards # nolint
#' @note This can be done via base_url(), "/git/trees/main?recursive=1", but
#' that requires an authorized request to the V3 API, while direct download of
#' files can be done without that, so is safer here.
#' @noRd
list_categories <- function () {

    # u <- paste0 (base_url (raw = TRUE), "main/standards.Rmd")
    u <- paste0 (base_url (raw = TRUE), "dev/standards.Rmd")
    tmp <- tempfile (fileext = ".Rmd")
    ret <- utils::download.file (u, destfile = tmp, quiet = TRUE) # nolint

    x <- readLines (tmp)
    cats <- grep ("\\`\\`\\`\\{r\\s", x, value = TRUE)
    cats <- regmatches (cats, regexpr ("standards\\-.*$+", cats))
    gsub ("standards\\-|\\}$", "", cats)
}


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

std_prefixes <- function () {

    cats <- list_categories ()
    prefixes <- rep (NA_character_, length (cats))
    prefixes [cats == "bayesian"] <- "BS"
    prefixes [cats == "eda"] <- "EA"
    prefixes [cats == "general"] <- "G"
    prefixes [cats == "ml"] <- "ML"
    prefixes [cats == "regression"] <- "RE"
    prefixes [cats == "spatial"] <- "SP"
    prefixes [cats == "time-series"] <- "TS"
    prefixes [cats == "unsupervised"] <- "UL"
    prefixes [cats == "distributions"] <- "PD"
    prefixes [cats == "networks"] <- "NW"

    return (data.frame (
        category = cats,
        prefix = prefixes,
        stringsAsFactors = FALSE
    ))
}
