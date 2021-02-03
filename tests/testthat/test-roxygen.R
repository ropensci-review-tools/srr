test_that("roxygen standards", {

              #Sys.setenv("CLIPR_ALLOW" = TRUE)

              pkg_name <- paste0 (sample (letters, size = 7), collapse = "")
              d <- rssr_pkg_skeleton (pkg_name = pkg_name)
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
              expect_true (length (grep ("@rssr tags:", x)) == 1)
              expect_true (length (grep ("@rssrNA tags:", x)) == 1)
              expect_true (length (grep ("@rssrTODO tags:", x)) == 1)
              expect_true (grep ("@rssrTODO tags:", x) >
                           grep ("@rssrNA tags:", x))
              expect_true (grep ("@rssrNA tags:", x) >
                           grep ("@rssr tags:", x))

              filename <- file.path (d, "R", "rssr-standards.R")
              # remove DESC file from directory should error
              desc <- file.path (d, "DESCRIPTION")
              temp <- file.path (d, "temp")
              chk <- file.rename (desc, temp)
              if (chk) {
                  expect_error (rssr_standards_roxygen (filename = filename),
                                "This function must be called within an R package")
                  chk <- file.rename (temp, desc)
              }
              # writes all standards with "@rssrTODO" tags:
              rssr_standards_roxygen (category = "regression",
                                      filename = filename,
                                      overwrite = TRUE)
              x2 <- capture.output (
                  roxygen2::roxygenise (d),
                  type = "message"
                  )

              todo_old <- x [(grep ("@rssrTODO", x) + 1):length (x)]
              todo_new <- x2 [(grep ("@rssrTODO", x2) + 1):length (x2)]
              expect_length (todo_old, 1L)
              expect_length (todo_new, 1L)

              expect_true (nchar (todo_new) > nchar (todo_old))
              standards_old <- gregexpr ("[A-Z]+[0-9]+\\.[0-9]", todo_old) [[1]]
              standards_new <- gregexpr ("[A-Z]+[0-9]+\\.[0-9]", todo_new) [[1]]
              expect_length (standards_old, 1)
              expect_true (length (standards_new) > 100)
})

test_that ("roclet errors", {

               nm <- paste0 (sample (letters, size = 7), collapse = "")
               d <- rssr_pkg_skeleton (pkg_name = nm)

               # ------1. Adding extract @rssrVerbose tag should error:
               f <- file.path (d, "R", "test.R")
               x <- c (readLines (f),
                       "",
                       "#' @rssrVerbose TRUE",
                       "NULL")
               writeLines (x, f)

               out <- tryCatch (roxygen2::roxygenise (d),
                                error = function (e) e)
               expect_s3_class (out, "simpleError")
               expect_true (grepl ("There must be only one @rssrVerbose flag",
                                   out$message))

               x <- x [1:(length (x) - 3)]
               writeLines (x, f)

               # ------2. Docs should be auto-verbose when @rssrVerbose flag is
               # ------   removed
               f <- file.path (d, "R", "rssr-standards.R")
               x0 <- readLines (f)
               x <- x0 [-grep ("@rssrVerbose", x0)]
               writeLines (x, f)
               x <- capture.output (
                                    roxygen2::roxygenise (d),
                                    type = "message"
               )
               expect_true (length (x) > 5) # output is verbose
               writeLines (x0, f)

               # ------3. @rssrVerbose value must be logical
               i <- grep ("@rssrVerbose", x0)
               x <- x0
               x [i] <- gsub ("TRUE", "junk", x0 [i])
               writeLines (x, f)
               out <- tryCatch (roxygen2::roxygenise (d),
                                error = function (e) e)
               expect_s3_class (out, "simpleError")
               txt <- "The @rssrVerbose tag should only have 'TRUE' or 'FALSE'"
               expect_true (grepl (txt, out$message))
               writeLines (x0, f)

               # -------4. Should not be a comma after last listed standard
               f <- file.path (d, "R", "test.R")
               x0 <- x <- readLines (f)
               i1 <- grep ("@rssr", x)
               i2 <- grep ("@export", x)
               i <- which (seq (i) > max (i1) & seq (i) < min (i2))
               x [i] <- paste0 (x [i], ",")
               writeLines (x, con = f)
               out <- tryCatch (roxygen2::roxygenise (d),
                                error = function (e) e)
               expect_s3_class (out, "simpleError")
               txt <- "It appears you've got a comma after the last @rssr entry"
               expect_true (grepl (txt, out$message))
               writeLines (x0, f)

               # --------5. Multi-line standards should end each line but last
               # --------   with comma
               x <- x0
               i <- grep ("@rssr", x)
               x [i] <- gsub (",$", "", x [i])
               writeLines (x, con = f)
               out <- tryCatch (roxygen2::roxygenise (d),
                                error = function (e) e)
               expect_s3_class (out, "simpleError")
               txt <- "Each @rssr standard should be separated by a comma"
               expect_true (grepl (txt, out$message))
               writeLines (x0, f)

               # --------6. @rssrNA tags should only be in a block named
               # --------   "NA_standards"
               f <- file.path (d, "R", "rssr-standards.R")
               x <- x0 <- readLines (f)
               i <- grep ("NA\\_standards", x)
               x [i] <- "#' not_NA_standards"
               writeLines (x, con = f)
               out <- tryCatch (roxygen2::roxygenise (d),
                                error = function (e) e)
               expect_s3_class (out, "simpleError")
               txt <- paste0 ("@rssrNA tags should only appear in a block with a ",
                              "title of NA_standards")
               expect_true (grepl (txt, out$message))
})
