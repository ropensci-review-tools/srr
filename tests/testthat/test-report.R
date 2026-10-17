test_that ("srr_report with missing standards", {

    path <- srr_stats_pkg_skeleton ()
    f <- fs::path (path, "R", "srr-stats-standards.R")
    expect_true (fs::file_exists (f))

    r <- srr_report (path, view = FALSE)
    # Expect three errors, with HTML-formatted red crosses:
    errs <- grep (":heavy_multiplication_x: Error:", r, fixed = TRUE)
    expect_length (errs, 2L)

    msg <- "Package must comply with at least 50% of all standards"
    expect_length (grep (msg, r), 1L)
    msg <- "must comply with at least 50% of category-specific standards"
    expect_length (grep (msg, r), 1L)

    expect_true (any (grepl ("Missing Standards", r, fixed = TRUE)))

    tryCatch (
        fs::dir_delete (path),
        error = function (e) NULL
    )
})

test_that ("srr_report", {

    path <- srr_stats_pkg_skeleton ()

    categories <- srr_stats_categories ()$category
    categories <- categories [-which (categories == "general")]
    expect_length (categories, 9L)

    f <- fs::path (path, "R", "srr-stats-standards.R")
    expect_true (fs::file_exists (f))
    s <- srr_stats_roxygen (
        category = categories,
        filename = f,
        overwrite = TRUE
    )

    r <- srr_report (path, view = FALSE)
    # Expect three errors, with HTML-formatted red crosses:
    errs <- grep (":heavy_multiplication_x: Error:", r, fixed = TRUE)
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
        x [-grep (paste0 ("{", s, "}"), x, fixed = TRUE)]
    }
    s <- c ("G1.1", "G1.2", "G1.3", "G2.0", "G2.1", "RE2.2", "G2.3", "G1.4")
    for (i in s) {
        x <- rm1 (x, i)
    }
    writeLines (x, f)

    expect_message (
        r <- srr_report (path, view = FALSE)
    )
    errs <- grep (":heavy_multiplication_x: Error:", r, fixed = TRUE)
    expect_length (errs, 3L)

    f <- attr (r, "file")
    expect_identical (tools::file_ext (f), "html")
    expect_true (fs::file_exists (f))

    expect_type (r, "character")
    expect_gt (length (r), 400L)

    expect_false (any (grepl ("Missing Standards", r, fixed = TRUE)))

    tryCatch (
        fs::dir_delete (path),
        error = function (e) NULL
    )
})

test_that ("update_msg_paths guards messages with no file suffix", {

    path <- fs::path_temp ("pkg")
    path_root <- fs::path_temp ()

    msgs <- list (msgs_missing = list ("Standard 'G1.0' is missing"))
    out <- update_msg_paths (msgs, path, path_root)
    expect_identical (out$msgs_missing [[1]], "Standard 'G1.0' is missing")

    msgs2 <- list (msgs_line = list ("'G2.0' on line 3 of file [R/x.R]"))
    out2 <- update_msg_paths (msgs2, path, path_root)
    expect_match (out2$msgs_line [[1]], "pkg/R/x\\.R")
    expect_false (grepl ("[R/x.R]", out2$msgs_line [[1]], fixed = TRUE))
})
