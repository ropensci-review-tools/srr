test_that("roxygen standards", {

              #Sys.setenv("CLIPR_ALLOW" = TRUE)

              d <- rssr_pkg_skeleton (pkg_name = "demo")
              expect_true (file.exists (file.path (tempdir (), "demo")))
              fp <- file.path (tempdir (), "demo", "DESCRIPTION")
              expect_true (file.exists (fp))
              fp <- file.path (tempdir (), "demo", "NAMESPACE")
              expect_true (file.exists (fp))
              fp <- file.path (tempdir (), "demo", "R")
              expect_true (file.exists (fp))
              fp <- file.path (tempdir (), "demo", "src")
              expect_true (file.exists (fp))
              fp <- file.path (tempdir (), "demo", "tests")
              expect_true (file.exists (fp))

              x <- capture.output (
                  roxygen2::roxygenise (d),
                  type = "message"
                  )

              expect_true (length (x) > 10)
              expect_true (length (grep ("Re-compiling demo", x)) == 1)
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
})
