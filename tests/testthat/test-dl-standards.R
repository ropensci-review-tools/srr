

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
              txt <- "Regression and Supervised Learning Standards"
              expect_true (any (grepl (txt, s)))
              expect_true (any (grepl ("Machine Learning Standards", s)))

              expect_identical (s, readLines (filename))

              x <- capture.output (
                  s2 <- rssr_checklist_check (filename),
                  type = "message"
              )
              expect_identical (s, s2)
              txt <- "No formatting issues found in file"
              expect_true (any (grepl (txt, x)))

              # ------- test checklist_check: -------
              i <- grep ("\\*\\*G", s2) [1]
              # change one bold markdown format to single "*":
              s2 [i] <- gsub ("\\*\\*G", "\\*G", s2 [i])
              writeLines (s2, filename)
              x <- capture.output (
                  s3 <- rssr_checklist_check (filename),
                  type = "message"
              )
              txt <- "file contained incorrect formatting and has been modified"
              expect_true (any (grep (txt, x)))
              expect_true (s3 [i] != s2 [i]) # file has been fixed
              expect_true (grepl ("\\*\\*G", s3 [i]))
})
