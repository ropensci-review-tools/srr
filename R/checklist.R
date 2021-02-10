
#' rrr_stats_checklist_check
#'
#' Correct any potential formatting issues in a completed standards checklist
#'
#' @param file Name of local file containing a completed checklist. Must be a
#' markdown document in `.md` format, not `.Rmd` or anything else.
#' @export
rrr_stats_checklist_check <- function (file) {

    if (!file.exists (file))
        stop ("File [", file, "] does not exist")

    if (!tools::file_ext (file) == "md")
        stop ("file must be in '.md' format")

    x0 <- readLines (file)

    x <- rebalance_bold (x0)
    x <- fix_sequences (x)
    x <- fix_nas (x, sym = "*")
    x <- fix_nas (x, sym = "_")

    if (!identical (x0, x)) {
        cli::cli_alert (paste0 ("file contained incorrect ",
                                "formatting and has been modified"))
        writeLines (x, file)
    } else {
        cli::cli_alert_success ("No formatting issues found in file")
    }

    cli::cli_alert_info ("Checklist copied to clipboard")
    invisible (clipr::write_clip (x))
}

#' fix imbalanced one-versus-two asterices or underscores
#' @param x markdown text
#' @noRd
rebalance_bold <- function (x) {

    fix_start <- function (x, sym = "*") {
        regex <- paste0 ("\\s\\", sym, "[A-Z]+[0-9]+\\.[0-9]+\\", sym)

        n <- regexpr (regex, x)
        if (any (n > 0)) {
            index <- which (n > 0)
            x [index] <- paste0 (substring (x [index], 1, n [index]),
                                 sym,
                                 substring (x [index], n [index] + 1))
        }
        return (x)
    }

    fix_end <- function (x, sym = "*") {
        regex <- paste0 ("\\", sym, "[A-Z]+[0-9]+\\.[0-9]+\\", sym, "\\s")
        n <- regexpr (regex, x)
        len <- attr (n, "match.length")
        if (any (n > 0)) {
            index <- which (n > 0)
            ends <- n [index] + len [index] - 2
            x [index] <- paste0 (substring (x [index], 1, ends),
                                 sym,
                                 substring (x [index], ends + 1))
        }
        return (x)
    }

    x <- fix_start (x, "*")
    x <- fix_start (x, "_")
    x <- fix_end (x, "*")
    x <- fix_end (x, "_")

    return (x)
}

#' Fix sequences of standards to have each enclosed in bold with separator
#' dashes not bold
#'
#' The "{Pd}" bit is from
#' https://stackoverflow.com/questions/44353306/r-regex-not-matching-all-hyphens
#' @param sym Either "*" or "_"
#' @noRd
fix_sequences <- function (x, sym = "*") {

    # Add any missing symbols before dashes:
    regex <- paste0 ("\\", sym, "[A-Z]+[0-9]+\\.[0-9]+\\p{Pd}")
    n <- regexpr (regex, x, perl = TRUE)
    len <- attr (n, "match.length")
    if (any (n > 0)) {
        index <- which (n > 0)
        ends <- n [index] + len [index] - 2
        x [index] <- paste0 (substring (x [index], 1, ends),
                             sym, sym,
                             substring (x [index], ends + 1))
    }

    # And missing symbols after dashes before standard identifier:
    regex <- paste0 ("\\p{Pd}[A-Z]+[0-9]+\\.[0-9]+")
    n <- regexpr (regex, x, perl = TRUE)
    len <- attr (n, "match.length")
    if (any (n > 0)) {
        index <- which (n > 0)
        x [index] <- paste0 (substring (x [index], 1, n [index]),
                             sym, sym,
                             substring (x [index], n [index] + 1))
    }

    return (x)
}

fix_nas <- function (x, sym = "*") {
    # replace "NA" with "N/A":
    regex <- paste0 ("\\", sym, "NA\\", sym)
    index <- grep (regex, x)
    if (length (index) > 0)
        x [index] <- gsub (regex,
                           paste0 (sym, "N/A", sym),
                           x [index])

    # replace single preceding symbol with double
    regex <- paste0 ("\\s\\", sym, "N/A")
    index <- grep (regex, x)
    if (length (index) > 0)
        x [index] <- gsub (regex,
                           paste0 (" ", sym, sym, "N/A"),
                           x [index])

    # replace single end symbol with double
    regex <- paste0 ("\\", sym, "N\\/A\\", sym, "(\\s|$)")
    index <- grep (regex, x)
    if (length (index) > 0) {
        x [index] <- gsub (regex,
                           paste0 (sym, "N/A", sym, sym, " "),
                           x [index])
        x [index] <- gsub ("\\s$", "", x [index])
    }

    return (x)
}
