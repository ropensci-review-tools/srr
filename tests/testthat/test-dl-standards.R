test_that("download standards", {

              filename <- tempfile ()
              mdname <- paste0 (filename, ".md")
              expect_false (file.exists (mdname))
              expect_message (
                  s <- rssr_standards_checklist (c ("regression", "ml"),
                                                 filename = filename)
              )
              expect_true (file.exists (mdname))

              expect_type (s, "character")
              expect_true (length (s) > 100)
              expect_true (grepl ("General Standards", s [1]))
              expect_true (any (grepl ("Regression and Supervised Learning Standards", s)))
              expect_true (any (grepl ("Machine Learning Standards", s)))

              expect_identical (s, readLines (mdname))
})
