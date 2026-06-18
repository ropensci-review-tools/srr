# CRAN notes for srr_1.0.0 submission

This is an initial submission, updated in response to CRAN feedback in the following ways:

- All functions now document return examples.
- All examples except two now use '\donttest{}' instead of '\dontrun{}', except:
    - One '\dontrun{}' to avoid a NOTE on CRAN win-builder machines that it takes > 5 seconds.
    - One '\dontrun{}' on a very short function which only writes to clipboard. The `clipr` package has a flag to switch off writing in non-interactive environments, but this fails when running examples on win-builder machines. The problem therefore lies in `clipr`, and not here, and that makes '\dontrun{}' necessary here.
    - Same reasons necessitate one further '\dontrun{}' on a function which calls that clipboard function.
- Use of 'installed.packages()' now removed, and replaced with 'find.package()'.

## Test environments

The package has been checked on all environments listed below, and generates only one note identifying the package as a new submission.

GitHub actions:
* Linux: R-release, R-devel
* OSX: R-release
* Windows: R-release

CRAN win-builder:
* R-oldrelease, R-release, R-devel
