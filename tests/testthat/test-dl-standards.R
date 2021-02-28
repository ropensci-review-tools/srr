

test_that("download standards", {

              Sys.setenv ("CLIPR_ALLOW" = TRUE)
              Sys.setenv ("NOCLIPR" = TRUE)

              filename <- tempfile (fileext = ".md")
              expect_false (file.exists (filename))
              expect_message (
                  s <- srr_stats_checklist (c ("regression", "ml"),
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
                  s2 <- srr_stats_checklist_check (filename),
                  type = "message"
              )
              expect_identical (s, s2)
              txt <- "No formatting issues found in file"
              expect_true (any (grepl (txt, x)))

              # ------- test checklist_check: -------
              expect_error (srr_stats_checklist_check (),
                            "argument \"file\" is missing")
              tf <- tempfile (fileext = ".txt")
              writeLines ("blah", con = tf)
              expect_error (srr_stats_checklist_check (tf),
                            "file must be in '.md' format")

              # modify start of standard: by changing one bold markdown format
              # to single "*":
              i <- grep ("\\*\\*G", s2) [1]
              s2 [i] <- gsub ("\\*\\*G", "\\*G", s2 [i])
              writeLines (s2, filename)
              x <- capture.output (
                  s3 <- srr_stats_checklist_check (filename),
                  type = "message"
              )
              txt <- "file contained incorrect formatting and has been modified"
              expect_true (any (grep (txt, x)))
              expect_true (s3 [i] != s2 [i]) # file has been fixed
              expect_true (grepl ("\\*\\*G", s3 [i]))

              # modify end of standard
              i <- grep ("\\*\\*G", s2) [2]
              j <- regexpr ("[0-9]\\*\\*", s2 [i])
              num <- substring (s2 [i], j, j)
              s2 [i] <- gsub (paste0 (num, "\\*\\*"),
                              paste0 (num, "\\*"), s2 [i])
              writeLines (s2, filename)
              x <- capture.output (
                  s4 <- srr_stats_checklist_check (filename),
                  type = "message"
              )
              txt <- "file contained incorrect formatting and has been modified"
              expect_true (any (grep (txt, x)))
              expect_true (s4 [i] != s2 [i]) # file has been fixed
              expect_true (grepl (paste0 (num, "\\*\\*"), s4 [i]))

              # join standards with hyphen which not NOT be within bold markers
              s2 <- s
              s2 [i] <- gsub (paste0 (num, "\\*\\*"),
                              paste0 (num, "--G5.5\\*\\*"), s2 [i])
              # The function which repairs those by re-inserting "**":
              tmp <- fix_sequences (s2 [i], sym = "*")
              expect_true (grepl (paste0 (num, "\\*\\*--\\*\\*G5.5"), tmp))
              writeLines (s2, filename)
              x <- capture.output (
                  s5 <- srr_stats_checklist_check (filename),
                  type = "message"
              )
              expect_identical (s5 [i], tmp)

              # fix NA -> N/A
              s2 <- s
              s2 [i] <- paste0 (strsplit (s [i], "\\*\\*\\s") [[1]] [1],
                                "** *NA*")
              tmp <- fix_nas (s2 [i])
              expect_true (grepl ("\\*\\*N\\/A\\*\\*$", tmp))
              writeLines (s2, filename)
              x <- capture.output (
                  s6 <- srr_stats_checklist_check (filename),
                  type = "message"
              )
              expect_identical (s6 [i], tmp)
})

test_that ("checklist_check", {
               expect_error (srr_stats_checklist_check (),
                             "argument \"file\" is missing")
})
