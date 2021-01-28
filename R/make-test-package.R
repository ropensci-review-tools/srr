
make_pkg_path <- function (base_dir = tempdir (), pkg_name = "demo") {

    d <- file.path (base_dir, pkg_name)
    if (!file.exists (d))
        dir.create (d, recursive = TRUE)

    return (d)
}

get_roxygen_version <- function () {

    ip <- as.data.frame (installed.packages ())
    if (!"roxygen2" %in% ip$Package)
        return (NULL)

    return (ip$Version [ip$Package == "roxygen2"])
}

make_desc <- function (d, pkg_name) {

    desc <- c (paste0 ("Package: ", pkg_name),
               "Title: What the Package Does (One Line, Title Case)",
               "Version: 0.0.0.9000",
               "Authors@R: ",
               "  person(given = \"First\",",
               "         family = \"Last\",",
               "         role = c(\"aut\", \"cre\"),",
               "         email = \"first.last@example.com\")",
               "Description: What the package does (one paragraph).",
               "Suggests:",
               "    testthat",
               "License: GPL-3",
               "Encoding: UTF-8",
               "Roxygen: list(markdown = TRUE, roclets = c (\"rd\", \"namespace\", \"rssr::rssr_roclet\"))")

    rv <- get_roxygen_version ()
    if (!is.null (rv))
        desc <- c (desc, paste0 ("RoxygenNote: ", rv))

    writeLines (desc, con = file.path (d, "DESCRIPTION"))
}

make_R_fn <- function (d) {

    rfile <- c ("#' test_fn",
                "#' A test funtion",
                "#' @rssr G1.1",
                "#' @export",
                "test_fn <- function() {",
                "  message(\"This function does nothing\")",
                "  }")
    dr <- file.path (d, "R")
    if (!file.exists (dr))
        dir.create (dr)
    writeLines (rfile, con = file.path (dr, "test.R"))
}

make_test_files <- function (d, pkg_name) {

    tfile <- c ("library(testthat)",
                paste0 ("library(", pkg_name, ")"),
                "",
                paste0 ("test_check(\"", pkg_name, "\")"))

    dt <- file.path (d, "tests", "testthat")
    if (!file.exists (dt))
        dir.create (dt, recursive = TRUE)
    writeLines (tfile, con = file.path (d, "tests", "testthat.R"))

    tfile <- c ("#' @rssr G2.2 is addressed here",
                "test_that(\"dummy test\", {",
                "    expect_true (TRUE)",
                "})")
    writeLines (tfile, con = file.path (d, "tests", "testthat", "test-a.R"))

}


make_pkg <- function (base_dir = tempdir (), pkg_name = "demo") {

    d <- make_pkg_path (base_dir, pkg_name)

    if (length (list.files (d)) > 0L)
        stop ("The path [", d, "] is not empty; ",
              "can only make a package in an empty directory\n",
              "  Directory can be cleared with 'unlink(<dir>, recursive = TRUE)'")

    make_desc (d, pkg_name)
    make_R_fn (d)
    make_test_files (d, pkg_name)

    return (d)
}
