test_that("available categories", {

              expect_silent (
                  x <- srr_stats_categories ()
                  )
              expect_s3_class (x, "data.frame")
              expect_equal (ncol (x), 4)
              expect_identical (names (x),
                                c ("category", "std_prefix", "title", "url"))
              expect_true (nrow (x) >= 8)
              expect_true (all (c ("bayesian", "eda", "general", "ml",
                                   "regression", "spatial", "time-series",
                                   "unsupervised") %in%
                            x$category))

})
