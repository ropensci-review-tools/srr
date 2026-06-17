# CRAN notes for srr_1.0.0 submission

This is an initial submission, updated in response to CRAN feedback in the following ways:

- All functions now document return examples.
- All examples except one now use '\donttest{}' instead of '\dontrun{}'. The one remaining '\dontrun{}' is needed because that example otherwise triggers a NOTE on CRAN win-builder machines that it takes > 5 seconds.
- Use of 'installed.packages()' now removed, and replaced with 'find.package()'.

## Test environments

The package has been checked on all environments listed below, and generates only one note identifying the package as a new submission.

GitHub actions:
* Linux: R-release, R-devel
* OSX: R-release
* Windows: R-release

CRAN win-builder:
* R-oldrelease, R-release, R-devel
