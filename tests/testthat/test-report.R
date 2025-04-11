skip_on_os ("windows")

test_that ("srr_report", {

    path <- srr_stats_pkg_skeleton ()

    categories <- srr_stats_categories ()$category
    categories <- categories [-which (categories == "general")]
    expect_length (categories, 8L)

    f <- file.path (path, "R", "srr-stats-standards.R")
    expect_true (file.exists (f))
    s <- srr_stats_roxygen (
        category = categories,
        filename = f,
        overwrite = TRUE
    )

    r <- srr_report (path, view = FALSE)
    # Expect three errors, with HTML-formatted red crosses:
    errs <- grep ("&#10060; Error:", r, fixed = TRUE)
    expect_length (errs, 3L)

    msg <- "Package must comply with at least 50% of all standards"
    expect_length (grep (msg, r), 1L)
    msg <- "must comply with at least 50% of category-specific standards"
    expect_length (grep (msg, r), 1L)
    msg <- "should document how package complies, not just copy original"
    expect_length (grep (msg, r), 1L)

    # rm duplicated stds from TODO list:
    x <- readLines (f)
    rm1 <- function (x, s = "G1.1") {
        ptn <- paste0 ("\\{", gsub ("\\.", "\\\\.", s), "\\}")
        x [-grep (ptn, x)]
    }
    s <- c ("G1.1", "G1.2", "G1.3", "G2.0", "G2.1", "RE2.2", "G2.3", "G1.4")
    for (i in s) {
        x <- rm1 (x, i)
    }
    writeLines (x, f)

    expect_message (
        r <- srr_report (path, view = FALSE)
    )
    errs <- grep ("&#10060; Error:", r, fixed = TRUE)
    expect_length (errs, 3L)

    f <- attr (r, "file")
    expect_equal (tools::file_ext (f), "html")
    expect_true (file.exists (f))

    expect_type (r, "character")
    skip_on_os ("mac")
    expect_true (length (r) > 400L)

    skip_on_os ("windows")
    expect_false (any (grepl ("Missing Standards", r)))
})
