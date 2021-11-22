
test_that("srr_report", {

    path <- srr_stats_pkg_skeleton ()

    categories <- srr_stats_categories ()$category
    categories <- categories [-which (categories == "general")]
    expect_length (categories, 7L)

    f <- file.path (path, "R", "srr-stats.R")
    expect_false (file.exists (f))
    s <- srr_stats_roxygen (category = categories, filename = f)
    expect_true (file.exists (f))

    r <- srr_report (path, view = FALSE)
    f <- attr (r, "file")
    expect_equal (tools::file_ext (f), "html")
    expect_true (file.exists (f))

    expect_type (r, "character")
    expect_true (length (r) > 400L)

    # Should be no missing standards:
    expect_false (any (grepl ("Missing Standards", r)))
})
