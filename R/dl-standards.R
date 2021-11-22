# functions to download current standards from reference (bookdown) site
# https://github.com/ropensci/statistical-software-review-book

#' @param raw If `TRUE`, use api.github.com to access raw file contents,
#' otherwise standard github.com URL.
#' @noRd
base_url <- function (raw = FALSE) {

    if (raw)
        ret <- "https://raw.githubusercontent.com/"
    else
        ret <- "https://api.github.com/repos/"
    return (paste0 (ret, "ropensci/statistical-software-review-book/"))
}

#' @return List of all current categories as obtained from directory contents of
#' https://github.com/ropensci/statistical-software-review-book/tree/main/standards # nolint
#' @note This can be done via base_url(), "/git/trees/main?recursive=1", but
#' that requires an authorized request to the V3 API, while direct download of
#' files can be done without that, so is safer here.
#' @noRd
list_categories <- function () {

    #u <- paste0 (base_url (), "git/trees/main?recursive=1")
    u <- paste0 (base_url (raw = TRUE), "main/standards.Rmd")
    tmp <- tempfile (fileext = ".Rmd")
    ret <- utils::download.file (u, destfile = tmp, quiet = TRUE) # nolint

    x <- readLines (tmp)
    cats <- grep ("\\`\\`\\`\\{r\\s", x, value = TRUE)
    cats <- regmatches (cats, regexpr ("standards\\/(\\w|\\-)+\\.Rmd", cats))
    gsub ("standards\\/|\\.Rmd.*", "", cats)
}

#' @param category One of the names of files in above link (for
#' `list_categories`)
#' @param quiet Be quiet or not
#' @return Character vector of contents of the `.Rmd` file for nominated
#' standards
#' @noRd
dl_standards <- function (category = "general", quiet = FALSE) {

    u <- paste0 (base_url (raw = TRUE),
                 "main/standards/", category, ".Rmd")

    tmp <- file.path (tempdir (),
                      paste0 ("srr-standards-", category, ".Rmd"))
    if (!file.exists (tmp))
        ret <- utils::download.file (u, destfile = tmp, quiet = TRUE) # nolint

    if (!quiet)
        cli::cli_alert_success ("Downloaded {category} standards")

    readLines (tmp)
}

stds_version <- function () {

    u <- paste0 (base_url (raw = TRUE), "main/DESCRIPTION")

    tmp <- file.path (tempdir (), "stats-devguide-DESCRIPTION")
    if (!file.exists (tmp))
        ret <- utils::download.file (u, destfile = tmp, quiet = TRUE) # nolint

    d <- data.frame (read.dcf (tmp))

    return (d$Version)
}

#' @param s Full text describing and including a set of standards downloaded
#' with `dl_standards`
#' @return The standards only extracted from `s`, formatted as checklist items.
#' @noRd
format_standards <- function (s) {

    index1 <- grep ("\\s?-\\s\\[\\*\\*[A-Z]", s)
    index_sp <- grep ("^\\s*$", s)
    index2 <- vapply (seq_along (index1), function (i) {
                      ret <- index_sp [which (index_sp > index1 [i]) [1]]
                      if (is.na (ret)) {
                          if (i == length (index1)) {
                              ret <- length (s)
                          } else {
                              ret <- index1 [i + 1] - 1L
                          }
                      } else if (i < length (index1)) {
                          if (ret > index1 [i + 1])
                              ret <- index1 [i + 1] - 1L
                      }
                      return (ret)},
                      integer (1))

    # include 3rd- and 4th-level sub-section headings:
    index3 <- grep ("^\\#\\#\\#\\s|^\\#\\#\\#\\#\\s", s)
    index1 <- sort (c (index1, index3))
    index2 <- sort (c (index2, index3))

    s <- vapply (seq_along (index1), function (i)
                 paste0 (s [index1 [i]:index2 [i]], collapse = " "),
                 character (1))
    # rm hyperlink of standards
    r <- regexpr ("\\]\\{#[A-Z]+[0-9]+\\_[0-9]+([a-z]?)\\}", s)
    regmatches (s, r) <- ""
    # convert dot points to checklist items:
    s <- gsub ("\\s*\\-\\s+\\[\\*\\*", "- \\[ \\] **", s)

    # indent sub-standards
    index <- grep ("\\-\\s?\\[\\s\\]\\s\\*\\*[A-Z]+[0-9]\\.[0-9]+[a-z]\\*\\*", s) # nolint
    s [index] <- paste0 ("    ", s [index])

    s <- add_space_around_sections (s)

    # finally reduce white space
    s <- gsub ("\\s+", " ", s)

    return (s)
}

category_titles_urls <- function (category) {

    ret <- list ()
    u_base <- "https://stats-devguide.ropensci.org/standards.html#"

    if (category == "general")
        ret <- list (title = "General",
                     url = paste0 (u_base, "general-standards"))
    if (category == "bayesian")
        ret <- list (title = "Bayesian",
                     url = paste0 (u_base, "bayesian-and-monte-carlo-software"))
    else if (category == "eda")
        ret <- list (title = "EDA",
                     url = paste0 (u_base, "exploratory-data-analysis"))
    else if (category == "ml")
        ret <- list (title = "Machine Learning",
                     url = paste0 (u_base, "machine-learning-software"))
    else if (category == "regression")
        ret <- list (title = "Regression and Supervised Learning",
                     url = paste0 (u_base,
                                   "regression-and-supervised-learning"))
    else if (category == "time-series")
        ret <- list (title = "Time Series",
                     url = paste0 (u_base, "time-series-software"))
    else if (category == "unsupervised")
        ret <- list (title = paste0 ("Dimensionality Reduction, Clustering, ",
                                     "and Unsupervised Learning"),
                     url = paste0 (u_base, "dimensionality-reduction-",
                                   "clustering-and-unsupervised-learning"))
    else if (category == "spatial")
        ret <- list (title = "Spatial",
                     url = paste0 (u_base, "spatial-software"))

    return (ret)
}

#' Download checklists of statistical software standards
#'
#' Obtain rOpenSci standards for statistical software, along with one or more
#' category-specific standards, as a checklist, and store the result in the
#' local clipboard ready to paste.
#'
#' @param category One of the names of files given in the directory contents of
#' \url{https://github.com/ropensci/statistical-software-review-book/tree/main/standards},
#' each of which is ultimately formatted into a sub-section of the standards.
#' @param filename Optional name of local file to save markdown-formatted
#' checklist. A suffix of `.md` will be automatically appended.
#' @return A character vector containing a markdown-style checklist of general
#' standards along with standards for any additional categories.
#' @family helper
#'
#' @examples
#' \dontrun{
#' x <- srr_stats_checklist (category = "regression")
#' # or write to specified file:
#' f <- tempfile (fileext = ".md")
#' x <- srr_stats_checklist (category = "regression", filename = f)
#' }
#' @export
srr_stats_checklist <- function (category = NULL, filename = NULL) {

    s <- get_standards_checklists (category = category)

    cli::cli_alert_info ("Markdown-formatted checklist copied to clipboard")

    if (!is.null (filename)) {
        filename <- paste0 (tools::file_path_sans_ext (filename), ".md")
        writeLines (text = s, con = filename)
    }

    if (!Sys.getenv ("NOCLIPR") == "TRUE") # used to turn off clipr in tests
        clipr::write_clip (s)

    invisible (s)
}

#' Insert standards into code in \pkg{roxygen2} format
#'
#' Obtain rOpenSci standards for statistical software, along with one or more
#' category-specific standards, as a checklist, convert to project-specific
#' \pkg{roxygen2} format, and save in nominated file.
#'
#' @inheritParams srr_stats_checklist
#' @param filename Name of 'R' source file in which to write
#' \pkg{roxygen2}-formatted lists of standards.
#' @param overwrite If `FALSE` (default) and `filename` already exists, a dialog
#' will ask whether file should be overwritten.
#' @return Nothing
#' @family roxygen
#' @examples
#' \dontrun{
#' path <- srr_stats_pkg_skeleton ()
#' # contains a few standards; insert all with:
#' f <- file.path (path, "R", "srr-stats-standards.R")
#' file.exists (f)
#' length (readLines (f)) # only 14 lines
#' srr_stats_roxygen (category = "regression",
#'                    file = f,
#'                    overwrite = TRUE)
#' length (readLines (f)) # now much longer
#' }
#' @export
srr_stats_roxygen <- function (category = NULL,
                               filename = "srr-stats-standards.R",
                               overwrite = FALSE) {

    loc <- here::here ()
    if (dirname (filename) != ".") {
        loc <- path.expand (dirname (filename))
        if (substring (loc, nchar (loc), nchar (loc)) == "R")
            loc <- gsub (paste0 (.Platform$file.sep, "R$"), "", loc)
    }

    if (!"DESCRIPTION" %in% list.files (loc))
        stop ("This function must be called within an R package directory")

    filename <- file.path (loc, "R", basename (filename))

    if (!overwrite & interactive () & file.exists (filename)) {
        x <- readline ("Overwrite current file (y/n)? ")        # nocov
        if (tolower (substring (x, 1, 1) != "y"))               # nocov
            stop ("Okay, we'll stop there")                     # nocov
    }

    s <- get_standards_checklists (category = category)

    # remove all blank lines, section titles, and separators
    s <- s [-which (s == "" | grepl ("^\\#|^\\-+$", s))]
    # replace initial checklist characters
    s <- gsub ("^\\s?\\-\\s+\\[\\s\\]\\s+", "", s)
    # replace bold/italic formatting characters with curly braces.
    # This uses regexpr so only first match is modified
    gptn <- "\\*\\*[A-Z]+[0-9]+\\.([0-9]+)?[a-z]?\\*\\*"
    g <- regexpr (gptn, s)
    s_end <- substring (s, g + attr (g, "match.length"), nchar (s))
    m <- regmatches (s, g)
    s_start <- gsub ("\\*\\*$", "}", gsub ("^\\*\\*", "{", m))
    s <- paste0 (s_start, s_end)

    # nolint start -------- lines > 80 character --------
    x <- c ("#' srr_stats",
            "#'",
            "#' All of the following standards initially have `@srrstatsTODO` tags.",
            "#' These may be moved at any time to any other locations in your code.",
            "#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,",
            "#' or `@srrstatsNA`, ensuring that references to every one of the following",
            "#' standards remain somewhere within your code.",
            "#' (These comments may be deleted at any time.)",
            "#'",
            "#' @srrstatsVerbose TRUE",
            "#'",
            paste0 ("#' @srrstatsTODO ", s),
            "#' @noRd",
            "NULL")

    # Then add demo NA_standards
    x <- c (x,
            "",
            "#' NA_standards",
            "#'",
            "#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`",
            "#' to `@srrstatsNA`, and placed together in this block, along with explanations",
            "#' for why each of these standards have been deemed not applicable.",
            "#' (These comments may also be deleted at any time.)",
            "#' @noRd",
            "NULL")
    # nolint end

    writeLines (x, con = filename)
    cli::cli_alert_info (paste0 ("Roxygen2-formatted standards written to [",
                                 basename (filename), "]"))
}

get_standards_checklists <- function (category = NULL) {

    s <- dl_standards (category = "general")
    s <- format_standards (s)
    u <- "https://stats-devguide.ropensci.org/standards.html#general-standards"
    s <- c (paste0 ("## [General Standards](", u, ")"),
            "", s, "")

    if (any (grepl ("general", category, ignore.case = TRUE)))
        category <- category [-grep ("general", category, ignore.case = TRUE)]

    if (!is.null (category)) {
        categories <- tolower (list_categories ())
        for (i in seq_along (category)) {
            category [i] <- match.arg (tolower (category [i]), categories)
            cat_title <- category_titles_urls (category [i])
            s_cat <- dl_standards (category = category [i])
            s_cat <- format_standards (s_cat)
            stitle <- paste0 ("## [",
                              cat_title$title,
                              " Standards](",
                              cat_title$url,
                              ")")
            s <- c (s, "", "---", "", stitle, "", s_cat)
        }
    }

    return (s)
}

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
    cat_full <- unlist (lapply (cats$category, function (i)
                                category_titles_urls (i)))

    version <- stds_version ()

    index <- seq (length (cat_full) / 2) * 2

    res <- data.frame (category = cats$category,
                       std_prefix = cats$prefix,
                       title = cat_full [index - 1],
                       url = cat_full [index],
                       stringsAsFactors = FALSE)

    attr (res, "stds_version") <- version

    return (res)
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

    return (data.frame (category = cats,
                        prefix = prefixes,
                        stringsAsFactors = FALSE))
}

#' @param s One set of standards with no spaces between sections or lines.
#' @return Expanded version with each sub-section having a blank line either
#' side of it.
#' @noRd
add_space_around_sections <- function (s) {
    index_hdr <- grep ("^\\#\\#\\#\\s|^\\#\\#\\#\\#\\s", s)
    index_hdr_pre <- index_hdr [index_hdr > 1]
    index_hdr_post <- index_hdr [index_hdr < length (s)]
    index_hdr <- sort (c (index_hdr_pre, index_hdr_post))
    # index_hdr has two values for the position of each sub-section header
    index <- sort (c (seq (s), index_hdr))
    len <- length (index) # length of final version

    s_index <- which (!duplicated (index))
    # all section breaks are then first element of index triplets; these need to
    # be moved to second position to give blank line both before and after
    index <- which (diff (s_index) > 1)
    index <- index [index > 1] # don't move opening sub-section
    s_index [index] <- s_index [index] + 1

    snew <- rep ("", len)
    snew [s_index] <- s

    # that can result in double empty lines which are then reduced to singles
    # only
    index1 <- which (snew == "")
    index2 <- which (diff (index1) == 1)
    snew <- snew [- (index1 [index2])]

    return (snew)
}
