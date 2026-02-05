skip_on_os ("mac")
skip_on_os ("windows")

test_that ("release", {

    Sys.setenv ("CLIPR_ALLOW" = TRUE)
    Sys.setenv ("NOCLIPR" = TRUE)

    pkgname <- paste0 (sample (letters, 8), collapse = "")
    d <- srr_stats_pkg_skeleton (pkg_name = pkgname)
    fs::file_delete (fs::path (d, "R", "test.R"))
    fs::file_delete (fs::path (d, "README.Rmd"))

    x <- utils::capture.output (
        rep <- srr_stats_pre_submit (d),
        type = "message"
    )
    expect_true (length (x) > 100) # > 100 standards are missing

    errs_output <- grep ("^\\!", x)
    expect_length (errs_output, 4L)
    # Report has error text only, no symbols or exclamations:
    errs_rep <- grep ("^\\!", rep)
    expect_length (errs_rep, 0L)

    msg <- "package still has TODO standards and can not be submitted"
    expect_length (grep (msg, x), 1L)
    expect_length (grep (msg, rep), 1L)
    msg <- "the following standards (.*) are missing from your code"
    expect_length (grep (msg, x), 1L)
    expect_length (grep (msg, rep), 1L)

    msg <- "must comply with at least 50% of all standards"
    expect_length (grep (msg, x), 1L)
    expect_length (grep (msg, rep), 1L)
    msg <- "must comply with at least 50% of category-specific standards"
    expect_length (grep (msg, x), 1L)
    expect_length (grep (msg, rep), 1L)

    missing_stds_output <- grep ("^[0-9]+", x)
    missing_stds_rep <- grep ("[A-Z][0-9]+\\.[0-9]", rep, value = T)
    expect_true (length (missing_stds_output) > 100L)
    expect_true (length (missing_stds_rep) > 100L)
    expect_equal (length (missing_stds_output), length (missing_stds_rep))
})
