test_that("dummy package", {

              pkg_name <- paste0 (sample (c (letters, LETTERS), size = 8),
                                  collapse = "")
              d <- rssr_pkg_skeleton (pkg_name = pkg_name)
              expect_true (file.exists (d))
              files <- list.files (d)
              expect_true ("DESCRIPTION" %in% files)
              expect_true ("R" %in% files)
              expect_true ("tests" %in% files)

              x <- capture.output (roxygen2::roxygenise (d), type = "message")

              expect_true (any (grepl ("@rssr tags:", x)))
              expect_true (any (grepl ("Standards \\[G1\\.1\\]", x)))

              expect_true (any (grepl ("@rssrNA tags:", x)))
              expect_true (any (grepl ("Standards \\[S3\\.3\\]", x)))
              # S3.3 is an @rssrNA tag
              expect_true (grep ("Standards \\[S3\\.3\\]", x) [1] >
                           grep ("@rssrNA tags:", x) [1])

              expect_true (any (grepl ("@rssrTODO tags:", x)))
              expect_true (any (grepl ("Standards \\[S4\\.4\\]", x)))
              expect_true (grep ("Standards \\[S4\\.4\\]", x) [1] >
                           grep ("@rssrTODO tags:", x) [1])
})
