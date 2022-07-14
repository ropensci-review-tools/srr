
make_pkg_path <- function (base_dir = tempdir (), pkg_name = "demo") {

    d <- file.path (base_dir, pkg_name)
    if (!file.exists (d)) {
        dir.create (d, recursive = TRUE)
    }

    return (d)
}

get_roxygen_version <- function () {

    ip <- as.data.frame (utils::installed.packages ())
    if (!"roxygen2" %in% ip$Package) {
        return (NULL)
    } # nocov

    return (ip$Version [ip$Package == "roxygen2"])
}

write_desc <- function (d, pkg_name) {

    desc <- c (
        paste0 ("Package: ", pkg_name),
        "Title: What the Package Does (One Line, Title Case)",
        "Version: 0.0.0.9000",
        "Authors@R: ",
        "  person(given = \"First\",",
        "         family = \"Last\",",
        "         role = c(\"aut\", \"cre\"),",
        "         email = \"first.last@example.com\")",
        "Description: What the package does (one paragraph).",
        "Imports:",
        "    Rcpp",
        "Suggests:",
        "    testthat",
        "LinkingTo:",
        "    Rcpp",
        "License: GPL-3",
        "Encoding: UTF-8"
    )

    rv <- get_roxygen_version ()
    if (!is.null (rv)) {

        desc <- c (
            desc,
            # paste0 ("RoxygenNote: ", rv),
            paste0 (
                "Roxygen: list(markdown = TRUE, ",
                "roclets = c (\"rd\", \"namespace\", ",
                "\"srr::srr_stats_roclet\"))"
            )
        )
    }

    writeLines (desc, con = file.path (d, "DESCRIPTION"))
}

write_r_fn <- function (d, pkg_name) {

    rfile <- c (
        "#' test_fn",
        "#'",
        "#' A test funtion",
        "#'",
        "#' @srrstats {G1.1, G1.2, G1.3} with some text",
        "#' @srrstats Text can appear before standards {G2.0, G2.1}",
        "#' @srrstatsTODO {RE1.1} standards which are still to be",
        "#' addressed are tagged 'srrstatsTODO'",
        "#'",
        "#' @export",
        "test_fn <- function() {",
        "  message(\"This function does nothing\")",
        "}"
    )
    dr <- file.path (d, "R")
    if (!file.exists (dr)) {
        dir.create (dr)
    }
    writeLines (rfile, con = file.path (dr, "test.R"))

    rfile <- c (
        "#' @keywords internal",
        "#' @importFrom Rcpp evalCpp",
        "\"_PACKAGE\"",
        "",
        paste0 (
            "# The following block is used by ",
            "usethis to automatically manage"
        ),
        "# roxygen namespace tags. Modify with care!",
        "## usethis namespace: start",
        paste0 ("#' @useDynLib ", pkg_name, ", .registration = TRUE"),
        "## usethis namespace: end",
        "NULL"
    )
    writeLines (rfile, con = file.path (dr, paste0 (pkg_name, "-package.R")))

    rfile <- c (
        "#' NA_standards",
        "#'",
        "#' @srrstatsNA {RE3.3} is not applicable",
        "#' @noRd",
        "NULL",
        "",
        "#' srrstats_standards",
        "#'",
        "#' @srrstatsVerbose TRUE",
        "#' @srrstatsTODO Here is {RE4.4} as TODO, noting that text can",
        "#' precede the standard number, as long as standards are",
        "#' given within the first set of square brackets.",
        "#' @noRd",
        "NULL"
    )
    writeLines (rfile, con = file.path (dr, "srr-stats-standards.R"))
}

write_src_fn <- function (d) {

    sfile <- c (
        "#include <Rcpp.h>",
        "",
        "//' src_fn",
        "//'",
        "//' A test C++ function",
        "//' @srrstats {G2.3} in src directory",
        "//' @noRd",
        "// [[Rcpp::export]]",
        "int test () {",
        "    return 1L; }"
    )

    ds <- file.path (d, "src")
    if (!file.exists (ds)) {
        dir.create (ds)
    }

    writeLines (sfile, con = file.path (ds, "cpptest.cpp"))
}

write_readme <- function (d, pkg_name) {

    # nolint start --- lines > 80 characters
    rfile <- c (
        paste0 ("# ", pkg_name),
        "",
        paste0 (
            "This is a skeleton of an [`srr` statistics]",
            "(https://github.com/ropensci-review-tools/srr)"
        ),
        "package, intended developers to tweak as they like, in order to understand ",
        "how the package's roclets work.",
        "",
        "This `README.Rmd` file is here to demonstrate how to embed `srr` roclet tags.",
        "These tags need to be within dedicated *code chunks*, like the following:",
        "",
        "```{r srr-tags, eval = FALSE, echo = FALSE}",
        "#' roxygen_block_name",
        "#'",
        "#' (Add some text if you like)",
        "#'",
        "#' @srrstats {G1.4} Here is a reference to a standard",
        "#' @srrstatsTODO {G1.5} And here is a reference to a standard yet to be addressed",
        "```",
        "",
        "Note the chunk contains only [`roxygen2`](https://roxygen2.r-lib.org) lines,",
        "and nothing else at all. Please change the `eval` and `echo` parameters to",
        "see what happens when you knit the document."
    )
    # nolint end

    writeLines (rfile, con = file.path (d, "README.Rmd"))
}

write_namespace <- function (d, pkg_name) {

    nfile <- c (
        "# Generated by roxygen2: do not edit by hand",
        "",
        paste0 ("useDynLib(", pkg_name, ", .registration=TRUE)"),
        "importFrom(Rcpp, evalCpp)"
    )
    writeLines (nfile, con = file.path (d, "NAMESPACE"))
}

write_test_files <- function (d, pkg_name) {

    tfile <- c (
        "library(testthat)",
        paste0 ("library(", pkg_name, ")"),
        "",
        paste0 ("test_check(\"", pkg_name, "\")")
    )

    dt <- file.path (d, "tests", "testthat")
    if (!file.exists (dt)) {
        dir.create (dt, recursive = TRUE)
    }
    writeLines (tfile, con = file.path (d, "tests", "testthat.R"))

    tfile <- c (
        "#' @srrstats {RE2.2} is addressed here",
        "test_that(\"dummy test\", {",
        "    expect_true (TRUE)",
        "})"
    )
    writeLines (tfile, con = file.path (d, "tests", "testthat", "test-a.R"))

}


#' Make skeleton package to test roclet system
#'
#' Make a dummy package skeleton including 'srr' \pkg{roxygen2} tags which can
#' be used to try out the functionality of this package. Running the example
#' lines below which activate the 'srr' roclets, and show you what the output
#' of those roclets looks like. Feel free to examine the effect of modifying any
#' of the `@srrstats` tags within the code as identified by running those lines.
#'
#' @param base_dir The base directory where the package should be constructed.
#' @param pkg_name The name of the package. The final location of this package
#' will be in `file.path(base_dir, pkg_name)`.
#' @return The path to the directory holding the newly created package
#' @family helper
#' @examples
#' d <- srr_stats_pkg_skeleton (pkg_name = "mystatspkg")
#' # (capture.output of initial compliation messages)
#' x <- capture.output (roxygen2::roxygenise (d), type = "output")
#' @export
srr_stats_pkg_skeleton <- function (base_dir = tempdir (), pkg_name = "demo") {

    d <- make_pkg_path (base_dir, pkg_name)

    if (length (list.files (d)) > 0L) {
        stop (
            "The path [", d, "] is not empty; ",
            "can only make a package in an empty directory\n",
            "  Directory can be cleared with ",
            "'unlink(<dir>, recursive = TRUE)'"
        )
    }

    write_desc (d, pkg_name)
    write_r_fn (d, pkg_name)
    write_src_fn (d)
    write_test_files (d, pkg_name)
    write_readme (d, pkg_name)
    write_namespace (d, pkg_name)
    Rcpp::compileAttributes (pkgdir = d)

    return (d)
}
