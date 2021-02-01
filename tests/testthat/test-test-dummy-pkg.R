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

              expect_true (any (grepl ("R files", x)))
              expect_true (any (grepl ("Standards \\[G1\\.1\\]", x)))

              expect_true (any (grepl ("tests files", x)))
              expect_true (any (grepl ("Standards \\[G2\\.2\\]", x)))
              # G2.2 is in "tests files":
              expect_true (grep ("Standards \\[G2\\.2\\]", x) [1] >
                           grep ("tests files", x) [1])

              expect_true (any (grepl ("src files", x)))
              expect_true (any (grepl ("Standards \\[G3\\.3\\]", x)))
              expect_true (grep ("Standards \\[G3\\.3\\]", x) [1] >
                           grep ("src files", x) [1])
})
