skip_on_os ("mac")
skip_on_os ("windows")

test_that ("release", {

    Sys.setenv ("CLIPR_ALLOW" = TRUE)
    Sys.setenv ("NOCLIPR" = TRUE)

    pkgname <- paste0 (sample (letters, 8), collapse = "")
    d <- srr_stats_pkg_skeleton (pkg_name = pkgname)
    file.remove (file.path (d, "R", "test.R"))
    file.remove (file.path (d, "README.Rmd"))

    x <- utils::capture.output (
        srr_stats_pre_submit (d),
        type = "message"
    )
    expect_true (length (x) > 100) # > 100 standards are missing

    errs <- grep ("^\\!", x)
    expect_length (errs, 4L)

    msg <- "package still has TODO standards and can not be submitted"
    expect_length (grep (msg, x), 1L)
    msg <- "the following standards (.*) are missing from your code"
    expect_length (grep (msg, x), 1L)

    msg <- "must comply with at least 50% of all standards"
    expect_length (grep (msg, x), 1L)
    msg <- "must comply with at least 50% of category-specific standards"
    expect_length (grep (msg, x), 1L)

    missing_stds <- grep ("^[0-9]+", x)
    expect_true (length (missing_stds) > 100L)
})
