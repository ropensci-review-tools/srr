test_that("roxygen standards", {

              #Sys.setenv("CLIPR_ALLOW" = TRUE)

              pkg_name <- paste0 (sample (letters, size = 7), collapse = "")
              d <- rrr_stats_pkg_skeleton (pkg_name = pkg_name)
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

              x <- capture.output (
                  roxygen2::roxygenise (d),
                  type = "message"
                  )

              expect_true (length (x) > 10)
              expect_true (length (grep ("Re-compiling", x)) == 1)
              txt <- "rOpenSci Statistical Software Standards"
              expect_true (length (grep (txt, x)) == 1)
              expect_true (length (grep ("@rrrstats standards:", x)) == 1)
              expect_true (length (grep ("@rrrstatsNA standards:", x)) == 1)
              expect_true (length (grep ("@rrrstatsTODO standards:", x)) == 1)
              expect_true (grep ("@rrrstatsTODO standards:", x) >
                           grep ("@rrrstatsNA standards:", x))
              expect_true (grep ("@rrrstatsNA standards:", x) >
                           grep ("@rrrstats standards:", x))

              filename <- file.path (d, "R", "rrr-stats-standards.R")
              # remove DESC file from directory should error
              desc <- file.path (d, "DESCRIPTION")
              temp <- file.path (d, "temp")
              chk <- file.rename (desc, temp)
              if (chk) {
                  expect_error (rrr_stats_roxygen (filename = filename),
                                paste0 ("This function must be called ",
                                        "within an R package"))
                  chk <- file.rename (temp, desc)
              }
              # writes all standards with "@rrrstatsTODO" tags:
              rrr_stats_roxygen (category = "regression",
                                 filename = filename,
                                 overwrite = TRUE)
              x2 <- capture.output (
                  roxygen2::roxygenise (d),
                  type = "message"
                  )

              # -1 at end because they finish with a cli::rule line
              index <- (grep ("@rrrstatsTODO", x) + 1):(length (x) - 1)
              todo_old <- x [index]
              index2 <- (grep ("@rrrstatsTODO", x2) + 1):(length (x2) - 1)
              todo_new <- x2 [index2]
              expect_length (todo_old, 3L)
              expect_length (todo_new, 3L)

              # get only those from the rrr-stats-standards.R file:
              todo_old <- grep ("rrr-stats-standards\\.R",
                                todo_old, value = TRUE)
              todo_new <- grep ("rrr-stats-standards\\.R",
                                todo_new, value = TRUE)

              expect_true (nchar (todo_new) > nchar (todo_old))
              standards_old <- gregexpr ("[A-Z]+[0-9]+\\.[0-9]", todo_old) [[1]]
              standards_new <- gregexpr ("[A-Z]+[0-9]+\\.[0-9]", todo_new) [[1]]
              expect_length (standards_old, 1)
              expect_true (length (standards_new) > 100)
})

test_that ("roclet errors", {

               nm <- paste0 (sample (letters, size = 7), collapse = "")
               d <- rrr_stats_pkg_skeleton (pkg_name = nm)

               # ------1. Adding extract @rrrstatsVerbose tag should error:
               f <- file.path (d, "R", "test.R")
               x <- c (readLines (f),
                       "",
                       "#' @rrrstatsVerbose TRUE",
                       "NULL")
               writeLines (x, f)

               out <- tryCatch (roxygen2::roxygenise (d),
                                error = function (e) e)
               expect_s3_class (out, "simpleError")
               txt <- "There must be only one @rrrstatsVerbose flag"
               expect_true (grepl (txt, out$message))

               x <- x [1:(length (x) - 3)]
               writeLines (x, f)

               # ------2. Docs should be auto-verbose when @rrrstatsVerbose flag
               # ------   is removed
               f <- file.path (d, "R", "rrr-stats-standards.R")
               x0 <- readLines (f)
               x <- x0 [-grep ("@rrrstatsVerbose", x0)]
               writeLines (x, f)
               x <- capture.output (
                                    roxygen2::roxygenise (d),
                                    type = "message"
               )
               expect_true (length (x) > 5) # output is verbose
               writeLines (x0, f)

               # ------3. @rrrstatsVerbose value must be logical
               i <- grep ("@rrrstatsVerbose", x0)
               x <- x0
               x [i] <- gsub ("TRUE", "junk", x0 [i])
               writeLines (x, f)
               out <- tryCatch (roxygen2::roxygenise (d),
                                error = function (e) e)
               expect_s3_class (out, "simpleError")
               txt <- paste0 ("The @rrrstatsVerbose tag should only have ",
                              "'TRUE' or 'FALSE'")
               expect_true (grepl (txt, out$message))
               writeLines (x0, f)

               # --------4. @rrrstatsNA tags should only be in a block named
               # --------   "NA_standards"
               f <- file.path (d, "R", "rrr-stats-standards.R")
               x <- x0 <- readLines (f)
               i <- grep ("NA\\_standards", x)
               x [i] <- "#' not_NA_standards"
               writeLines (x, con = f)
               out <- tryCatch (roxygen2::roxygenise (d),
                                error = function (e) e)
               expect_s3_class (out, "simpleError")
               txt <- paste0 ("@rrrstatsNA tags should only appear in a ",
                              "block with a title of NA_standards")
               expect_true (grepl (txt, out$message))
})
