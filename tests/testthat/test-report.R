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

    expect_error (
        r <- srr_report (path, view = FALSE),
        paste0 (
            "Please rectify to ensure these standards are only ",
            "associated with one tag"
        )
    )

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

    f <- attr (r, "file")
    expect_equal (tools::file_ext (f), "html")
    expect_true (file.exists (f))

    expect_type (r, "character")
    skip_on_os ("mac")
    expect_true (length (r) > 400L)

    skip_on_os ("windows")
    expect_false (any (grepl ("Missing Standards", r)))
})
