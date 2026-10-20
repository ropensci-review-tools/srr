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

test_that ("categories added to the dev guide after release", {

    # A released srr knows the prefixes and titles of the categories that
    # existed when it was built. When the dev guide gains a category the
    # table must still have one row per category (ropensci-review-tools/srr#71).
    cats <- srr:::list_categories ()
    testthat::local_mocked_bindings (
        list_categories = function () c (cats, "zzz-new-category")
    )
    expect_silent (
        x <- srr_stats_categories ()
    )
    expect_identical (nrow (x), length (cats) + 1L)
    i <- which (x$category == "zzz-new-category")
    expect_length (i, 1L)
    expect_true (is.na (x$std_prefix [i]))
    expect_identical (x$title [i], "zzz-new-category")
    expect_identical (
        x$url [i],
        "https://stats-devguide.ropensci.org/standards.html#standards-zzz-new-category"
    )
})

test_that ("category title read from the dev guide heading", {

    expect_identical (
        srr:::category_title_from_guide ("networks"),
        "Network Software"
    )
    expect_identical (
        srr:::category_title_from_guide ("zzz-new-category"),
        "zzz-new-category"
    )
})
