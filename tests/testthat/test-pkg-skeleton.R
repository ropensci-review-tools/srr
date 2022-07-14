test_that ("dummy package", {

    pkg_name <- paste0 (sample (c (letters, LETTERS), size = 8),
        collapse = ""
    )
    d <- srr_stats_pkg_skeleton (pkg_name = pkg_name)
    expect_true (file.exists (d))
    files <- list.files (d)
    expect_true ("DESCRIPTION" %in% files)
    expect_true ("R" %in% files)
    expect_true ("tests" %in% files)

    x <- utils::capture.output (roxygen2::roxygenise (d), type = "message")

    expect_true (any (grepl ("@srrstats standards \\(", x)))
    expect_true (any (grepl ("\\s\\*\\s\\[G1\\.1,", x)))

    expect_true (any (grepl ("@srrstatsNA standards \\(", x)))
    expect_true (any (grepl ("\\s\\*\\s\\[RE3\\.3\\]", x)))
    # S3.3 is an @srrstatsNA tag
    expect_true (grep ("\\s\\*\\s\\[RE3\\.3\\]", x) [1] >
        grep ("@srrstatsNA standards \\(", x) [1])

    expect_true (any (grepl ("@srrstatsTODO standards \\(", x)))
    expect_true (any (grepl ("\\s\\*\\s\\[RE4\\.4\\]", x)))
    expect_true (grep ("\\s\\*\\s\\[RE4\\.4\\]", x) [1] >
        grep ("@srrstatsTODO standards \\(", x) [1])
    unlink (d, recursive = TRUE)
})

test_that ("skeleton errors", {

    pkg_name <- paste0 (sample (letters, size = 7), collapse = "")
    d <- file.path (tempdir (), pkg_name)
    dir.create (d)
    writeLines ("aaa", con = file.path (d, "aaa"))

    # This test fails on GitHub Windows runners:
    this_os <- Sys.info () [["sysname"]]
    if (this_os != "Windows") {
        expect_error (
            s <- srr_stats_pkg_skeleton (pkg_name = pkg_name),
            paste0 ("The path \\[", d, "\\] is not empty")
        )
    }
    unlink (d, recursive = TRUE)
})
