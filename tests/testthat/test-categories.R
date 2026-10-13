test_that ("available categories", {

    expect_silent (
        x <- srr_stats_categories ()
    )
    expect_s3_class (x, "data.frame")
    expect_identical (ncol (x), 4L)
    expect_named (x, c ("category", "std_prefix", "title", "url"))
    expect_gte (nrow (x), 8)
    expect_true (all (c (
        "bayesian", "eda", "general", "ml",
        "regression", "spatial", "time-series",
        "unsupervised"
    ) %in%
        x$category))

})
