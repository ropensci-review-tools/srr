<!-- badges: start -->

[![R build
status](https://github.com/ropenscilabs/rrr/workflows/R-CMD-check/badge.svg)](https://github.com/ropenscilabs/rrr/actions)
[![codecov](https://codecov.io/gh/ropenscilabs/rrr/branch/master/graph/badge.svg)](https://codecov.io/gh/ropenscilabs/rrr)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# rrr ([**r**OpenSci](https://ropensci.org) **r**eview **r**oclets)

This package contains roclets for use with
[rOpenSci](https://ropensci.org)’s software review systems. At present,
the package only contains roclets and associated functions to help those
developing and reviewing packages submitted to rOpenSci’s system for
[Statistical Software
Review](https://ropenscilabs.github.io/statistical-software-review-book/index.html).
The functions are mostly intended to ease alignment and assessment of
software against the standards detailed in the [main project
book](https://ropenscilabs.github.io/statistical-software-review-book/index.html)
(from here on referred to as the “SSR Book”).

This package can be installed with,

``` r
# install.packages("remotes")
remotes::install_github("ropenscilabs/rrr")
```

and loaded for use with,

``` r
library (rrr)
```

Both this `README`, and the main package vignette, describe the
functionality of the package in the specific context of the statistical
software review project. All functions intended for use in this context
are prefixed with `rssr_`. The remainder of this document is in two main
sections. If you’re developing a package for submission to our peer
review system, keep straight on reading. If you’ve been invited to
review a package, you may skip the following section and just read the
subsequent section. The general procedures for both developers and
reviewers are described at length in the [SSR
book](https://ropenscilabs.github.io/statistical-software-review-book/index.html),
with this `README` intended to provide supporting technical details.

## For Package Developers

People intending to develop packages for submission to our system for
peer reviewing statistical software will need to follow the following
general steps. Note that, while the `rrr` package has a few functions
which developers may call directly to aid their submission process, most
functionality of this package is implemented via custom [`roxygen2`
“roclets”](https://roxygen2.r-lib.org). The third of the following steps
describes how to link your package with `rrr` in order to use these
roclets.

1.  Ensure the package successfully passes all
    [`autotest`](https://github.com/ropenscilabs/autotest) tests,
    including setting `test = FALSE` flags to switch off any particular
    tests. For details, please see the [package documentation for
    `autotest`](https://ropenscilabs.github.io/autotest/).

2.  Decide which of our in-scope categories of statistical software best
    describe your package. The function
    [`rssr_available_categories()`](https://ropenscilabs.github.io/rrr/reference/rssr_available_categories.html)
    provides a list of currently developed categories for which
    standards have been developed, along with links to the online
    standards for each category:

    ``` r
    rssr_available_categories ()$title
    ```

        ## [1] "General"                                                        
        ## [2] "Bayesian"                                                       
        ## [3] "EDA"                                                            
        ## [4] "Machine Learning"                                               
        ## [5] "Regression and Supervised Learning"                             
        ## [6] "Spatial"                                                        
        ## [7] "Time Series"                                                    
        ## [8] "Dimensionality Reduction, Clustering, and Unsupervised Learning"

    That function also returns links to the full descriptions of each
    category in the [main project
    book](https://ropenscilabs.github.io/statistical-software-review-book/index.html).
    Any software within one or more of these categories may be
    considered for review.

3.  Enable your package to use the `rssr` roclets by modifying the
    package’s `DESCRIPTION` file so that the `Roxygen` line looks like
    this:

    ``` r
    Roxygen: list(markdown = TRUE, roclets = c ("namespace", "rd", "rrr::rssr_roclet"))
    ```

    That will load the [“roclet”](https://roxygen2.r-lib.org) used by
    this package to process statistical standards as documented within
    your actual code. Note that you do not need to add, import, or
    depend upon the `rrr` package anywhere else within the `DESCRIPTION`
    file.

4.  Load the `rrr` package and generate lists of standards within your
    package’s `/R` folder by running,
    [`rrr_stats_roxygen(category = c("<my-category-1>", "<my-category-2>"))`](https://ropenscilabs.github.io/rrr/reference/rrr_stats_roxygen.html).
    This will by default create a new file called by default
    `R/rssr_standards.R`, the first few lines of which will look like
    this:

        ## [1] "#' rrr_stats"                                                           
        ## [2] "#'"                                                                     
        ## [3] "#' All of the following standards initially have `@rssrTODO` tags."     
        ## [4] "#' These may be moved at any time to any other locations in your code." 
        ## [5] "#' Once addressed, please modify the tag from `@rssrTODO` to `@rssr`,"  
        ## [6] "#' or `@rssrNA`, ensuring that references to every one of the following"

    The file will contain a list of all standards from your nominated
    categories. This file may be renamed, and the individual items moved
    to other locations in other files, but all nominated standards
    should remain somewhere in [`roxygen2`](https://roxygen2.r-lib.org)
    blocks somewhere in your source code.

    The `@rssrVerbose` line defines a variable which may be used to
    suppress output from the `rssr` roclet when updating package
    documentation (by setting to `FALSE`). After that comes the list of
    standards, each of which is prefixed by a
    [`roxygen2`](https://roxygen2.r-lib.org) tag, `@rssrTODO`. A package
    can only be submitted once all of these `TODO` items have been
    addressed via one of the options described in the following two
    items.

5.  A standard may be addressed by moving the item in the
    `rssr_standards.R` file (or wherever you’ve chosen to list these
    within your own package) to one or more places in your code where
    these standards have been addressed. In doing so, the
    [`roxygen2`](https://roxygen2.r-lib.org) tag should be changed from
    `@rssrTODO` to `@rssr`, and the text which initially lists the
    actual standard should be changed to provide a brief description of
    how that standard has been met. Tags for one particular standard may
    be repeated in multiple places within your code, and we encourage
    locating an `@rssr` tag which refers to a particular standard at all
    locations which directly address that standard.

6.  Alternatively, any standards which you consider not applicable to
    your software may remain listed in the main `rssr_standards.R`
    document, with their tag changed from `@rssrTODO` to `@rssrNA`, and
    the description of the standard removed and replaced by an
    explanation of why you consider that standard not applicable to your
    software. These `@rssrNA` tags should be collected together within a
    single `roxygen2` block with a title of `NA_standards`. The
    [`rrr_stats_roxygen()`](https://ropenscilabs.github.io/rrr/reference/rrr_stats_roxygen.html)
    function generates a blank template for this block. Any
    non-applicable standards can then just be moved into this block,
    with their `@rssrTODO` tags changed to `@rssrNA`

7.  Each time you run
    [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
    or the equivalent
    [`roxygen2::roxygenise()`](https://roxygen2.r-lib.org/reference/roxygenize.html),
    the roclet will scan your package’s documentation for the state of
    standards, and will generate a summary of the result on your screen.

To help developers understand how to use these roclets, this package
includes a function,
[`rssr_pkg_skeleton()`](https://ropenscilabs.github.io/rrr/reference/rssr_pkg_skeleton.html),
which can be used to generate a skeleton of a package with several
[`roxygen2`](https://roxygen2.r-lib.org) tags inserted throughout the
code. This function returns the directory where the skeleton package has
been created, so running the following two lines will illustrate the
roclets in action:

``` r
d <- rssr_pkg_skeleton ()
roxygen2::roxygenise (d)
```

Note that the skeleton package also includes C++ code in a `src/`
directory, so will be compiled the first time your run
[`roxygensise()`](https://roxygen2.r-lib.org/reference/roxygenize.html)).
Running a second time will generate cleaner output from the `rssr`
roclets only. The tags included in
[`roxygen2`](https://roxygen2.r-lib.org/) blocks in this skeleton
package may be modified, moved, copied, and changed in any way you like
to help you understand how the roclets work. Simply play around with the
[`roxygen2`](https://roxygen2.r-lib.org/) lines and run
[`roxygensise()`](https://roxygen2.r-lib.org/reference/roxygenize.html))
each time to see the effect.

Individual standards may be moved to, and addressed in, any location
including the directories `R/`, `src/`, or `tests/`. The [`roxygen2`
roclet](https://roxygen2.r-lib.org) associated with this package is able
to parse the various `@rssr` tags in all of these locations.
