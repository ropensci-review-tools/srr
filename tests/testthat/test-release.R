

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
    msg <- "the following standards (.*) are missing from your code"
    expect_true (any (grepl (msg, x, ignore.case = TRUE)))
})
