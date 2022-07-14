test_that ("roxygen standards", {

    # Sys.setenv("CLIPR_ALLOW" = TRUE)

    pkg_name <- paste0 (sample (letters, size = 7), collapse = "")
    d <- srr_stats_pkg_skeleton (pkg_name = pkg_name)
    expect_true (file.exists (file.path (tempdir (), pkg_name)))
    fp <- file.path (tempdir (), pkg_name, "DESCRIPTION")
    expect_true (file.exists (fp))
    fp <- file.path (tempdir (), pkg_name, "NAMESPACE")
    expect_true (file.exists (fp))
    fp <- file.path (tempdir (), pkg_name, "R")
    expect_true (file.exists (fp))
    fp <- file.path (tempdir (), pkg_name, "src")
    expect_true (file.exists (fp))
    fp <- file.path (tempdir (), pkg_name, "tests")
    expect_true (file.exists (fp))

    x <- utils::capture.output (
        roxygen2::roxygenise (d),
        type = "message"
    )

    expect_true (length (x) > 10)
    expect_true (length (grep ("Re-compiling", x)) == 1)
    txt <- "rOpenSci Statistical Software Standards"
    expect_true (length (grep (txt, x)) == 1)
    expect_true (length (grep ("@srrstats standards \\(", x)) == 1)
    expect_true (length (grep ("@srrstatsNA standards \\(", x)) == 1)
    expect_true (length (grep ("@srrstatsTODO standards \\(", x)) == 1)
    expect_true (grep ("@srrstatsTODO standards \\(", x) >
        grep ("@srrstatsNA standards \\(", x))
    expect_true (grep ("@srrstatsNA standards \\(", x) >
        grep ("@srrstats standards \\(", x))

    filename <- file.path (d, "R", "srr-stats-standards.R")
    # remove DESC file from directory should error
    desc <- file.path (d, "DESCRIPTION")
    temp <- file.path (d, "temp")
    chk <- file.rename (desc, temp)
    if (chk) {
        expect_error (
            srr_stats_roxygen (filename = filename),
            paste0 (
                "This function must be called ",
                "within an R package"
            )
        )
        chk <- file.rename (temp, desc)
    }
    # writes all standards with "@srrstatsTODO" tags. Some of these
    # standards already exist as `@srrstats` in the skeleton, in
    # R/test.R, so initial attempt to `toxygenise` will trigger error:
    srr_stats_roxygen (
        category = "unsupervised",
        filename = filename,
        overwrite = TRUE
    )
    expect_error (
        roxygen2::roxygenise (d),
        "Standards .* are listed with both .* tags"
    )

    if (file.remove (file.path (d, "R", "test.R")) &
        file.remove (file.path (d, "README.Rmd"))) {
        # There is then one standard (G2.3) in src/cpptest.cpp plus TODO,
        # so:
        expect_error (
            suppressWarnings (roxygen2::roxygenise (d)),
            "Standards .* are listed with both .* tags"
        )
        f <- file.path (d, "src", "cpptest.cpp")
        cpptest <- gsub ("srrstats", "srrstatsTODO", readLines (f))
        writeLines (cpptest, con = f)
        # After fixing that and removing the file with duplicated
        # standards with mixed tags, things should once again work:
        expect_warning (
            x2 <- utils::capture.output (
                roxygen2::roxygenise (d),
                type = "message"
            ),
            "Objects listed as exports, but not present in namespace"
        )
        # That deletes former test_fn, so re-run to remove that
        # message from output
        x2 <- utils::capture.output (
            roxygen2::roxygenise (d),
            type = "message"
        )

        # -1 at end because they finish with a cli::rule line
        index <- (grep ("@srrstatsTODO", x) + 1):(length (x) - 1)
        todo_old <- x [index]
        index2 <- (grep ("@srrstatsTODO", x2) + 1):(length (x2) - 1)
        todo_new <- x2 [index2]
        expect_true (length (todo_old) >= 3L)
        expect_length (todo_new, 2L)

        # get only those from the srr-stats-standards.R file:
        todo_old <- grep ("srr-stats-standards\\.R",
            todo_old,
            value = TRUE
        )
        todo_new <- grep ("srr-stats-standards\\.R",
            todo_new,
            value = TRUE
        )

        expect_true (nchar (todo_new) > nchar (todo_old))
        ptn <- "[A-Z]+[0-9]+\\.[0-9]"
        standards_old <- gregexpr (ptn, todo_old) [[1]]
        standards_new <- gregexpr (ptn, todo_new) [[1]]
        expect_length (standards_old, 1)
        expect_true (length (standards_new) > 50)
    }
})

test_that ("roclet errors", {

    nm <- paste0 (sample (letters, size = 7), collapse = "")
    d <- srr_stats_pkg_skeleton (pkg_name = nm)

    # ------1. Adding extract @srrstatsVerbose tag should error:
    f <- file.path (d, "R", "test.R")
    x <- c (
        readLines (f),
        "",
        "#' @srrstatsVerbose TRUE",
        "NULL"
    )
    writeLines (x, f)

    out <- tryCatch (roxygen2::roxygenise (d),
        error = function (e) e
    )
    expect_s3_class (out, "simpleError")
    txt <- "There must be only one @srrstatsVerbose flag"
    expect_true (grepl (txt, out$message))

    x <- x [1:(length (x) - 3)]
    writeLines (x, f)

    # ------2. Docs should be auto-verbose when @srrstatsVerbose flag
    # ------   is removed
    f <- file.path (d, "R", "srr-stats-standards.R")
    x0 <- readLines (f)
    x <- x0 [-grep ("@srrstatsVerbose", x0)]
    writeLines (x, f)
    x <- utils::capture.output (
        roxygen2::roxygenise (d),
        type = "message"
    )
    expect_true (length (x) > 5) # output is verbose
    writeLines (x0, f)

    # ------3. @srrstatsVerbose value must be logical
    i <- grep ("@srrstatsVerbose", x0)
    x <- x0
    x [i] <- gsub ("TRUE", "junk", x0 [i])
    writeLines (x, f)
    out <- tryCatch (roxygen2::roxygenise (d),
        error = function (e) e
    )
    expect_s3_class (out, "simpleError")
    txt <- paste0 (
        "The @srrstatsVerbose tag should only have ",
        "'TRUE' or 'FALSE'"
    )
    expect_true (grepl (txt, out$message))
    writeLines (x0, f)

    # --------4. @srrstatsNA tags should only be in a block named
    # --------   "NA_standards"
    f <- file.path (d, "R", "srr-stats-standards.R")
    x <- x0 <- readLines (f)
    i <- grep ("NA\\_standards", x)
    x [i] <- "#' not_NA_standards"
    writeLines (x, con = f)
    out <- tryCatch (roxygen2::roxygenise (d),
        error = function (e) e
    )
    expect_s3_class (out, "simpleError")
    txt <- paste0 (
        "@srrstatsNA tags should only appear in a ",
        "block with a title of NA_standards"
    )
    expect_true (grepl (txt, out$message))
})
