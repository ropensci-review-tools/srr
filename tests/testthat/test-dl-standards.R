test_that("download standards", {

              Sys.setenv("CLIPR_ALLOW" = TRUE)

              filename <- tempfile (fileext = ".md")
              expect_false (file.exists (filename))
              expect_message (
                  s <- rssr_standards_checklist (c ("regression", "ml"),
                                                 filename = filename)
              )
              expect_true (file.exists (filename))

              expect_type (s, "character")
              expect_true (length (s) > 100)
              expect_true (grepl ("General Standards", s [1]))
              expect_true (any (grepl ("Regression and Supervised Learning Standards", s)))
              expect_true (any (grepl ("Machine Learning Standards", s)))

              expect_identical (s, readLines (filename))

              x <- capture.output (
                  s2 <- rssr_checklist_check (filename),
                  type = "message"
              )
              expect_identical (s, s2)
              expect_true (any (grepl ("No formatting issues found in file", x)))
})
