test_that ("dummy package", {

    skip_on_os ("windows")

    pkg_name <- paste0 (sample (c (letters, LETTERS), size = 8),
        collapse = ""
    )

    # If directories can not be unlinked on any test systems, this test must be
    # skipped.
    skip_if (fs::dir_exists (fs::path (fs::path_temp (), pkg_name)))

    d <- srr_stats_pkg_skeleton (pkg_name = pkg_name)
    expect_true (fs::file_exists (d))
    files <- fs::path_file (fs::dir_ls (d))
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

    # detach is critical here, because testthat uses `utils::sessionInfo()`,
    # which checks namespaces and tries to load DESC file from pkg location.
    pos <- match (paste0 ("package:", pkg_name), search ())
    if (!is.na (pos)) {
        detach (pos = pos, unload = TRUE)
    }
    tryCatch (
        fs::dir_delete (d),
        error = function (e) NULL
    )
})

test_that ("rust code", {

    skip_on_os ("windows")

    pkg_name <- paste0 (sample (c (letters, LETTERS), size = 8),
        collapse = ""
    )
    # If directories can not be unlinked on any test systems, this test must be
    # skipped.
    skip_if (fs::dir_exists (fs::path (fs::path_temp (), pkg_name)))

    d <- srr_stats_pkg_skeleton (pkg_name = pkg_name)
    f_rs <- add_extra_skeleton_code (d)
    expect_true (fs::file_exists (f_rs))
    x <- utils::capture.output (roxygen2::roxygenise (d), type = "message")

    # split 'x' into srr stats sections.
    # cli unicode chars can be seen with 'stringi::stri_escape_unicode':
    dashes <- "\u2500\u2500"
    index <- rep (0L, length (x))
    index [cli::ansi_grep (dashes, x)] <- 1L
    index <- cumsum (index)
    x <- split (x, f = as.factor (index))
    index <- which (vapply (
        x,
        function (i) any (grepl ("srrstats.*standards", i)),
        logical (1L)
    ))
    x <- x [index]
    tags <- unname (unlist (lapply (
        x,
        function (i) regmatches (i, regexpr ("@srrstats[A-Z]*", i))
    )))
    x_srrstats <- x [[which (tags == "@srrstats")]]
    x_contents <- regmatches (x_srrstats, gregexpr ("\\[.*?\\]", x_srrstats))
    # That has [standards numbers] followed by [files], so:
    x_files <- vapply (x_contents, function (i) i [2], character (1L))
    x_files <- gsub ("^\\[|\\]$", "", x_files [which (!is.na (x_files))])
    x_files <- fs::path_file (x_files)

    expect_true ("file.rs" %in% x_files)
    expect_true ("cpptest.cpp" %in% x_files)

    pos <- match (paste0 ("package:", pkg_name), search ())
    if (!is.na (pos)) {
        detach (pos = pos, unload = TRUE)
    }
    tryCatch (
        fs::dir_delete (d),
        error = function (e) NULL
    )
})

test_that ("skeleton errors", {

    pkg_name <- paste0 (sample (letters, size = 7), collapse = "")
    d <- fs::path (fs::path_temp (), pkg_name)

    skip_if (fs::dir_exists (d))

    fs::dir_create (d)
    writeLines ("aaa", con = fs::path (d, "aaa"))

    # This test fails on GitHub Windows runners:
    this_os <- Sys.info () [["sysname"]]
    if (this_os != "Windows") {
        expect_error (
            s <- srr_stats_pkg_skeleton (pkg_name = pkg_name),
            paste0 ("The path \\[", d, "\\] is not empty")
        )
    }
    p <- paste0 ("package:", pkg_name)
    if (p %in% search ()) {
        detach (p, unload = TRUE)
    }
    tryCatch (
        fs::dir_delete (d),
        error = function (e) NULL
    )
})
