<!-- badges: start -->

[![R build
status](https://github.com/ropensci-review-tools/srr/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci-review-tools/srr/actions)
[![codecov](https://codecov.io/gh/ropensci-review-tools/srr/branch/main/graph/badge.svg)](https://codecov.io/gh/ropensci-review-tools/srr)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# srr

“srr” stands for **S**oftware **R**eview **R**oclets, and is
[rOpenSci](https://ropensci.org)’s package for extending documentation
to include additional components specific to the software review
process. The package currently facilitates documenting how statistical
software complies with our collections of [Statistical Software
Standards](https://stats-devguide.ropensci.org/standards.html). Before
proceeding, the answer to an important question: **[What is a
“roclet”](https://github.com/r-lib/roxygen2/issues/1086)?**

-   A roclet is an object used by the
    [`roxygen2`](https://roxygen2.r-lib.org) package to convert
    [`roxygen2`](https://roxygen2.r-lib.org)-style documentation lines
    into some desired form of output.

## Why then?

This package currently serves to aid developers and reviewers of
statistical software in aligning their software against our extensive
[lists of
standards](https://stats-devguide.ropensci.org/standards.html). In
acknowledgement of [Colin Gillespie](https://github.com/csgillespie)’s
sentiments expressed in his keynote speech at the [European R Users
Meeting 2020](https://2020.erum.io/program/keynotes-invited-speakers/):

> Standards are good<br> Standards should be strict<br> No-one reads
> standards

the `srr` package aims to integrate the task of aligning software with
standards within the practice of coding itself, and to make standards
adherence as painless as possible.

## How?

The [`roxygen2`](https://roxygen2.r-lib.org) package parses all
documentation lines from all files in the `R/` directory of a package
which begin with `#'`. Special tags beginning with `@`, such as `@param`
or `@export`, may follow these symbols, and roclets define what is done
with different kinds of tags. The
[`roxygen2`](https://roxygen2.r-lib.org) package includes roclets to
process a number of tags; the `srr` package implements custom roclets to
process several additional tags for use with
[rOpenSci](https://ropensci.org)’s software review systems.

At present, the package only contains roclets and associated functions
to help those developing and reviewing packages submitted to rOpenSci’s
system for [Statistical Software
Review](https://stats-devguide.ropensci.org/). The functions are mostly
intended to ease alignment and assessment of software against the
standards detailed in the [main project
book](https://stats-devguide.ropensci.org/standards.html) (from here on
referred to as the “SSR Book”).

## Installation

The easiest way to install this package is via the associated
[`r-universe`](https://ropensci-review-tools.r-universe.dev/ui#builds).
As shown there, simply enable the universe with

``` r
options(repos = c(
    ropenscireviewtools = "https://ropensci-review-tools.r-universe.dev",
    CRAN = "https://cloud.r-project.org"))
```

And then install the usual way with,

``` r
install.packages("srr")
```

Alternatively, the package can be installed by running one of the
following lines:

``` r
remotes::install_github ("ropensci-review-tools/srr")
pak::pkg_install ("ropensci-review-tools/srr")
```

and loaded for use with,

``` r
library (srr)
```

## Overview

Both this `README`, and the main package vignette, describe the
functionality of the package in the specific context of the statistical
software review project. Both the roclet and all functions intended for
use in this context are prefixed with `srr_stats_`. The remainder of
this document is in two main sections. If you’re developing a statistics
package for submission to our peer review system, keep straight on
reading. If you’ve been invited to review a package, you may skip the
following section and just read the subsequent section. The general
procedures for both developers and reviewers are described at length in
the [SSR book](https://stats-devguide.ropensci.org/standards.html), with
this `README` intended to provide supporting technical details.

Note that the `srr` package can be applied only within the working
directory of a package. There are no `package` or `path` arguments to
allow functions to be applied to packages anywhere other than in the
current working directory.

## For Package Developers

People intending to develop packages for submission to our system for
peer reviewing statistical software will need to follow the following
general steps. Note that, while the `srr` package has a few functions
which developers may call directly to aid their submission process, most
functionality of this package is implemented via custom [`roxygen2`
“roclets”](https://roxygen2.r-lib.org). The third of the following steps
describes how to link your package with `srr` in order to use these
roclets.

1.  Ensure that your package successfully passes all
    [`autotest`](https://github.com/ropensci-review-tools/autotest)
    tests, potentially including setting `test = FALSE` flags to switch
    off any tests you consider not to be applicable to your package. For
    details, please see the [package documentation for
    `autotest`](https://docs.ropensci.org/autotest/).

2.  Decide which of our in-scope categories of statistical software best
    describe your package. The function
    [`srr_stats_categories()`](https://docs.ropensci.org/srr/reference/srr_stats_categories.html)
    provides a list of currently developed categories for which
    standards have been developed, along with links to the online
    standards for each category:

    ``` r
    srr_stats_categories ()$title
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
    book](https://stats-devguide.ropensci.org/standards.html). Any
    software within one or more of these categories may be considered
    for review.

3.  Enable your package to use the `srr_stats` roclets by modifying the
    package’s `DESCRIPTION` file so that the `Roxygen` line looks like
    this:

    ``` r
    Roxygen: list(markdown = TRUE, roclets = c ("namespace", "rd", "srr::srr_stats_roclet"))
    ```

    That will load the [“roclet”](https://roxygen2.r-lib.org) used by
    this package to process the documentation of statistical standards
    within your actual code. Note that you do not need to add, import,
    or depend upon the `srr` package anywhere else within the
    `DESCRIPTION` file or your actual package.

4.  Load the `srr` package and generate lists of standards within your
    package’s `/R` folder by running,
    [`srr_stats_roxygen(category = c("<my-category-1>", "<my-category-2>"))`](https://docs.ropensci.org/srr/reference/srr_stats_roxygen.html).
    This will by default create a new file called by default
    `R/srr_stats_standards.R`, the first few lines of which will look
    like this:

        ## [1] "#' srr_stats"                                                                 
        ## [2] "#'"                                                                           
        ## [3] "#' All of the following standards initially have `@srrstatsTODO` tags."       
        ## [4] "#' These may be moved at any time to any other locations in your code."       
        ## [5] "#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,"
        ## [6] "#' or `@srrstatsNA`, ensuring that references to every one of the following"

    The file will contain a list of all standards from your nominated
    categories. This file may be renamed, and the individual items moved
    to other locations in other files, but all nominated standards
    should remain in [`roxygen2`](https://roxygen2.r-lib.org) blocks
    somewhere in your source code.

    The `@srrstatsVerbose` line defines a variable which may be used to
    suppress output from the `srrstats` roclet when updating package
    documentation (by setting to `FALSE`). After that comes the list of
    standards, each of which is prefixed by a
    [`roxygen2`](https://roxygen2.r-lib.org) tag, `@srrstatsTODO`. A
    package can only be submitted once all of these `TODO` items have
    been addressed via one of the options described in the following two
    items.

5.  A standard may be addressed by moving the item in the
    `srr-stats-standards.R` file (or wherever you’ve chosen to list
    these within your own package) to one or more places in your code
    where these standards have been addressed. In doing so, the
    [`roxygen2`](https://roxygen2.r-lib.org) tag should be changed from
    `@srrstatsTODO` to `@srrstats`, and the text which initially lists
    the actual standard should be changed to provide a brief description
    of how that standard has been met. Tags for one particular standard
    may be repeated in multiple places within your code, and we
    encourage locating multiple `@srrstats` tags which refer to a
    particular standard at all locations which directly address that
    standard.

6.  Alternatively, any standards which you consider not applicable to
    your software may remain listed in the templated section of the main
    `srr-stats-standards.R` document (or any alternative location), with
    their tag changed from `@srrstatsTODO` to `@srrstatsNA`, and the
    description of the standard removed and replaced by an explanation
    of why you consider that standard not to be applicable to your
    software. These `@srrstatsNA` tags should be collected together
    within a single `roxygen2` block with a title of `NA_standards`, as
    provided in the initial template generated by the
    [`srr_stats_roxygen()`](https://docs.ropensci.org/srr/reference/srr_stats_roxygen.html)
    function. Any non-applicable standards can then just be moved into
    this block, with their `@srrstatsTODO` tags changed to `@srrstatsNA`

7.  Each time you run
    [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
    or the equivalent
    [`roxygen2::roxygenise()`](https://roxygen2.r-lib.org/reference/roxygenize.html),
    the roclet will scan your package’s documentation for the state of
    standards, and will generate a summary of the result on your screen.

To help developers understand how to use these roclets, this package
includes a function,
[`srr_stats_pkg_skeleton()`](https://docs.ropensci.org/srr/reference/srr_stats_pkg_skeleton.html),
which will generate a skeleton of a package with several
[`roxygen2`](https://roxygen2.r-lib.org) tags inserted throughout the
code. This function returns the directory where the skeleton package has
been created, so running the following two lines will illustrate the
roclets in action:

``` r
d <- srr_stats_pkg_skeleton ()
roxygen2::roxygenise (d)
```

Note that the skeleton package also includes C++ code in a `src/`
directory, so will be compiled the first time your run
[`roxygensise()`](https://roxygen2.r-lib.org/reference/roxygenize.html).
Running a second time will generate cleaner output from the `srr_stats`
roclets only. The tags included in
[`roxygen2`](https://roxygen2.r-lib.org/) blocks in this skeleton
package may be modified, moved, copied, and changed in any way you like
to help you understand how the roclets work. Simply play around with the
[`roxygen2`](https://roxygen2.r-lib.org/) lines and run
[`roxygensise()`](https://roxygen2.r-lib.org/reference/roxygenize.html)
each time to see the effect. Individual standards may be moved to, and
addressed in, any location including the directories `R/`, `src/`, or
`tests/`, and well as in `.Rmd` documentation files such as `README.Rmd`
or package vignettes. The `srr_stats` roclet associated with this
package is able to parse the various `@srrstats` tags in all of these
locations.

### Places where standards can NOT be inserted

While the `srr` package enables standards compliance to be documented
through inserting `@srrstats` tags in as many locations as possible, in
order to ensure compliance is documented as close as possible to the
point within the code where each standard is addressed, it is not
possible to insert `roxygen2` tags in every type of file. In general,
standards may be inserted in any `.R` or `.Rmd` file, and most types of
files in `src` or `inst/include` directories, as long as they are used
with a package able to convert documentation to a corresponding R file
(such as [`Rcpp`](http://www.rcpp.org/)’s generation of `RcppExports.R`
files which include the C++ documentation).

Tags may generally not be placed in any other kinds of files, including
`.md` files such as `CONTRIBUTING.md`, or other files without extensions
such as `DESCRIPTION`, `NAMESPACE`, or `NEWS`. Standards which are best
addressed in such files must be placed in some other generic location
(such as `R/srr-standards.R`), with a cross-reference to the file in
which they are actually addressed.

## Code of Conduct

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.

## Contributors


<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->

All contributions to this project are gratefully acknowledged using the [`allcontributors` package](https://github.com/ropenscilabs/allcontributors) following the [all-contributors](https://allcontributors.org) specification. Contributions of any kind are welcome!

### Code

<table>

<tr>
<td align="center">
<a href="https://github.com/mpadge">
<img src="https://avatars.githubusercontent.com/u/6697851?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/srr/commits?author=mpadge">mpadge</a>
</td>
<td align="center">
<a href="https://github.com/christophsax">
<img src="https://avatars.githubusercontent.com/u/1390827?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/srr/commits?author=christophsax">christophsax</a>
</td>
<td align="center">
<a href="https://github.com/maelle">
<img src="https://avatars.githubusercontent.com/u/8360597?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/srr/commits?author=maelle">maelle</a>
</td>
</tr>

</table>


### Issue Authors

<table>

<tr>
<td align="center">
<a href="https://github.com/santikka">
<img src="https://avatars.githubusercontent.com/u/8639149?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/srr/issues?q=is%3Aissue+author%3Asantikka">santikka</a>
</td>
<td align="center">
<a href="https://github.com/schneiderpy">
<img src="https://avatars.githubusercontent.com/u/77991319?u=4242d4c5942fced6368dd5c68221e6618092cbf8&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/srr/issues?q=is%3Aissue+author%3Aschneiderpy">schneiderpy</a>
</td>
</tr>

</table>


### Issue Contributors

<table>

<tr>
<td align="center">
<a href="https://github.com/jeroen">
<img src="https://avatars.githubusercontent.com/u/216319?u=bfe086cade8ccec09d43f3f2e1bb3b6304dc7ec6&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/srr/issues?q=is%3Aissue+commenter%3Ajeroen">jeroen</a>
</td>
</tr>

</table>

<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->
<!-- ALL-CONTRIBUTORS-LIST:END -->
