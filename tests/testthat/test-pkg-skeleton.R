test_that("dummy package", {

              pkg_name <- paste0 (sample (c (letters, LETTERS), size = 8),
                                  collapse = "")
              d <- rrr_stats_pkg_skeleton (pkg_name = pkg_name)
              expect_true (file.exists (d))
              files <- list.files (d)
              expect_true ("DESCRIPTION" %in% files)
              expect_true ("R" %in% files)
              expect_true ("tests" %in% files)

              x <- capture.output (roxygen2::roxygenise (d), type = "message")

              expect_true (any (grepl ("@rrrstats standards:", x)))
              expect_true (any (grepl ("\\s\\*\\s\\[G1\\.1,", x)))

              expect_true (any (grepl ("@rrrstatsNA standards:", x)))
              expect_true (any (grepl ("\\s\\*\\s\\[S3\\.3\\]", x)))
              # S3.3 is an @rrrstatsNA tag
              expect_true (grep ("\\s\\*\\s\\[S3\\.3\\]", x) [1] >
                           grep ("@rrrstatsNA standards:", x) [1])

              expect_true (any (grepl ("@rrrstatsTODO standards:", x)))
              expect_true (any (grepl ("\\s\\*\\s\\[S4\\.4\\]", x)))
              expect_true (grep ("\\s\\*\\s\\[S4\\.4\\]", x) [1] >
                           grep ("@rrrstatsTODO standards:", x) [1])
})

test_that ("skeleton errors", {

               pkg_name <- paste0 (sample (letters, size = 7), collapse = "")
               d <- file.path (tempdir (), pkg_name)
               dir.create (d)
               writeLines ("aaa", con = file.path (d, "aaa"))
               expect_error (
                             s <- rrr_stats_pkg_skeleton (pkg_name = pkg_name),
                             paste0 ("The path \\[", d, "\\] is not empty"))
})
