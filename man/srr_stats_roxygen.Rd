% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dl-standards.R
\name{srr_stats_roxygen}
\alias{srr_stats_roxygen}
\title{Insert standards into code in \pkg{roxygen2} format}
\usage{
srr_stats_roxygen(
  category = NULL,
  filename = "srr-stats-standards.R",
  overwrite = FALSE
)
}
\arguments{
\item{category}{One of the names of files given in the directory contents of
\url{https://github.com/ropensci/statistical-software-review-book/tree/main/standards},
each of which is ultimately formatted into a sub-section of the standards.}

\item{filename}{Name of 'R' source file in which to write
\pkg{roxygen2}-formatted lists of standards.}

\item{overwrite}{If \code{FALSE} (default) and \code{filename} already exists, a dialog
will ask whether file should be overwritten.}
}
\value{
Nothing
}
\description{
Obtain rOpenSci standards for statistical software, along with one or more
category-specific standards, as a checklist, convert to project-specific
\pkg{roxygen2} format, and save in nominated file.
}
\examples{
\dontrun{
path <- srr_stats_pkg_skeleton ()
# contains a few standards; insert all with:
f <- file.path (path, "R", "srr-stats-standards.R")
file.exists (f)
length (readLines (f)) # only 14 lines
srr_stats_roxygen (
    category = "regression",
    file = f,
    overwrite = TRUE
)
length (readLines (f)) # now much longer
}
}
\seealso{
Other roxygen: 
\code{\link{srr_stats_roclet}()}
}
\concept{roxygen}
