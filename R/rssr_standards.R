#' rssr_standards
#'
#' @rssrVerboseDoc TRUE
#'
#' @rssrTODO G1.0 Statistical Software should list at least one primary reference from published academic literature. 
#' @rssrTODO G1.1 All statistical terminology should be clarified and unambiguously defined. 
#' @rssrTODO G1.2 Software should use [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.
#' @rssrTODO G1.2a All internal (non-exported) functions should also be documented in standard [`roxygen2`](https://roxygen2.r-lib.org/) format, along with a final `@noRd` tag to suppress automatic generation of `.Rd` files. 
#' @rssrTODO G1.3 Software should include all code necessary to reproduce results which form the basis of performance claims made in associated publications. 
#' @rssrTODO G1.4 Software should include code necessary to compare performance claims with alternative implementations in other R packages. 
#' @rssrTODO G2.0 Implement assertions on lengths of inputs, particularly through asserting that inputs expected to be single- or multi-valued are indeed so.
#' @rssrTODO G2.0a Provide explicit secondary documentation of any expectations on lengths of inputs
#' @rssrTODO G2.1 Implement assertions on types of inputs (see the initial point on nomenclature above).
#' @rssrTODO G2.1a Provide explicit secondary documentation of expectations on data types of all vector inputs.
#' @rssrTODO G2.2 Appropriately prohibit or restrict submission of multivariate input to parameters expected to be univariate.
#' @rssrTODO G2.3 For univariate character input:
#' @rssrTODO G2.3a Use `match.arg()` or equivalent where applicable to only permit expected values.
#' @rssrTODO G2.3b Either: use `tolower()` or equivalent to ensure input of character parameters is not case dependent; or explicitly document that parameters are strictly case-sensitive.
#' @rssrTODO G2.4 Provide appropriate mechanisms to convert between different data types, potentially including:
#' @rssrTODO G2.4a explicit conversion to `integer` via `as.integer()`
#' @rssrTODO G2.4b explicit conversion to continuous via `as.numeric()`
#' @rssrTODO G2.4c explicit conversion to character via `as.character()` (and not `paste` or `paste0`)
#' @rssrTODO G2.4d explicit conversion to factor via `as.factor()`
#' @rssrTODO G2.4e explicit conversion from factor via `as...()` functions
#' @rssrTODO G2.5 Where inputs are expected to be of `factor` type, secondary documentation should explicitly state whether these should be `ordered` or not, and those inputs should provide appropriate error or other routines to ensure inputs follow these expectations. 
#' @rssrTODO G2.6 Software should accept as input as many of the above standard tabular forms as possible, including extension to domain-specific forms.
#' @rssrTODO G2.7 Software should provide appropriate conversion or dispatch routines as part of initial pre-processing to ensure that all other sub-functions of a package receive inputs of a single defined class or type.
#' @rssrTODO G2.8 Software should issue diagnostic messages for type conversion in which information is lost (such as conversion of variables from factor to character; standardisation of variable names; or removal of meta-data such as those associated with [`sf`-format](https://r-spatial.github.io/sf/) data) or added (such as insertion of variable or column names where none were provided). 
#' @rssrTODO G2.9 Software should ensure that extraction or filtering of single columns from tabular inputs should not presume any particular default behaviour, and should ensure all column-extraction operations behave consistently regardless of the class of tabular data used as input. 
#' @rssrTODO G2.10 Statistical Software should implement appropriate checks for missing data as part of initial pre-processing prior to passing data to analytic algorithms.
#' @rssrTODO G2.11 Where possible, all functions should provide options for users to specify how to handle missing (`NA`) data, with options minimally including:
#' @rssrTODO G2.11a error on missing data
#' @rssrTODO G2.11b ignore missing data with default warnings or messages issued
#' @rssrTODO G2.11c replace missing data with appropriately imputed values
#' @rssrTODO G2.12 Functions should never assume non-missingness, and should never pass data with potential missing values to any base routines with default `na.rm = FALSE`-type parameters (such as [`mean()`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/mean.html), [`sd()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/sd.html) or [`cor()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.html)).
#' @rssrTODO G2.13 All functions should also provide options to handle undefined values (e.g., `NaN`, `Inf` and `-Inf`), including potentially ignoring or removing such values. 
#' @rssrTODO G3.0 Statistical software should never compare floating point numbers for equality. All numeric equality comparisons should either ensure that they are made between integers, or use appropriate tolerances for approximate equality. 
#' @rssrTODO G4.0 Statistical Software which enables outputs to be written to local files should parse parameters specifying file names to ensure appropriate file suffices are automatically generated where not provided. 
#' @rssrTODO G5.0 Where applicable or practicable, tests should use standard data sets with known properties (for example, the [NIST Standard Reference Datasets](https://www.itl.nist.gov/div898/strd/), or data sets provided by other widely-used R packages).
#' @rssrTODO G5.1 Data sets created within, and used to test, a package should be exported (or otherwise made generally available) so that users can confirm tests and run examples. 
#' @rssrTODO G5.2 Appropriate error and warning behaviour of all functions should be explicitly demonstrated through tests. In particular,
#' @rssrTODO G5.2a Every message produced within R code by `stop()`, `warning()`, `message()`, or equivalent should be unique
#' @rssrTODO G5.2b Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values.
#' @rssrTODO G5.3 For functions which are expected to return objects containing no missing (`NA`) or undefined (`NaN`, `Inf`) values, the absence of any such values in return objects should be explicitly tested. 
#' @rssrTODO G5.4 Correctness tests to test that statistical algorithms produce expected results to some fixed test data sets (potentially through comparisons using binding frameworks such as [RStata](https://github.com/lbraglia/RStata)).
#' @rssrTODO G5.4a For new methods, it can be difficult to separate out correctness of the method from the correctness of the implementation, as there may not be reference for comparison. In this case, testing may be implemented against simple, trivial cases or against multiple implementations such as an initial R implementation compared with results from a C/C++ implementation.
#' @rssrTODO G5.4b For new implementations of existing methods, correctness tests should include tests against previous implementations. Such testing may explicitly call those implementations in testing, preferably from fixed-versions of other software, or use stored outputs from those where that is not possible.
#' @rssrTODO G5.4c Where applicable, stored values may be drawn from published paper outputs when applicable and where code from original implementations is not available
#' @rssrTODO G5.5 Correctness tests should be run with a fixed random seed
#' @rssrTODO G5.6 Parameter recovery tests to test that the implementation produce expected results given data with known properties. For instance, a linear regression algorithm should return expected coefficient values for a simulated data set generated from a linear model.
#' @rssrTODO G5.6a Parameter recovery tests should generally be expected to succeed within a defined tolerance rather than recovering exact values.
#' @rssrTODO G5.6b Parameter recovery tests should be run with multiple random seeds when either data simulation or the algorithm contains a random component. (When long-running, such tests may be part of an extended, rather than regular, test suite; see G4.10-4.12, below).
#' @rssrTODO G5.7 Algorithm performance tests to test that implementation performs as expected as properties of data change. For instance, a test may show that parameters approach correct estimates within tolerance as data size increases, or that convergence times decrease for higher convergence thresholds.
#' @rssrTODO G5.8 Edge condition tests to test that these conditions produce expected behaviour such as clear warnings or errors when confronted with data with extreme properties including but not limited to:
#' @rssrTODO G5.8a Zero-length data
#' @rssrTODO G5.8b Data of unsupported types (e.g., character or complex numbers in for functions designed only for numeric data)
#' @rssrTODO G5.8c Data with all-`NA` fields or columns or all identical fields or columns
#' @rssrTODO G5.8d Data outside the scope of the algorithm (for example, data with more fields (columns) than observations (rows) for some regression algorithms)
#' @rssrTODO G5.9 Noise susceptibility tests Packages should test for expected stochastic behaviour, such as through the following conditions:
#' @rssrTODO G5.9a Adding trivial noise (for example, at the scale of `.Machine$double.eps`) to data does not meaningfully change results
#' @rssrTODO G5.9b Running under different random seeds or initial conditions does not meaningfully change results 
#' @rssrTODO G5.10 Extended tests should included and run under a common framework with other tests but be switched on by flags such as as a `<MYPKG>_EXTENDED_TESTS=1` environment variable.
#' @rssrTODO G5.11 Where extended tests require large data sets or other assets, these should be provided for downloading and fetched as part of the testing workflow.
#' @rssrTODO G5.11a When any downloads of additional data necessary for extended tests fail, the tests themselves should not fail, rather be skipped and implicitly succeed with an appropriate diagnostic message.
#' @rssrTODO G5.12 Any conditions necessary to run extended tests such as platform requirements, memory, expected runtime, and artefacts produced that may need manual inspection, should be described in developer documentation such as a `CONTRIBUTING.md` or `tests/README.md` file.
#' @rssrTODO SP1.0 Spatial software should explicitly indicate its domain of applicability, and in particular distinguish whether the software may be applied in rectilinear/geometric domains, curvilinear/geographic domains, or both. 
#' @rssrTODO SP1.1 Spatial software should explicitly indicate its dimensional domain of applicability, in particular through identifying whether it is applicable to two or three dimensions only, or whether there are any other restrictions on dimensionality. 
#' @rssrTODO SP2.0 Spatial software should adhere to one or the other of the following standards, dependent on whether input data may be generic (non-class-based), or of one or more specific classes.
#' @rssrTODO SP2.0a Spatial Software intended for use in geographical (or more general curvilinear domains) should only accept input data of one or more classes explicitly developed to represent such data.
#' @rssrTODO SP2.0b Spatial Software which is intended for use in rectilinear domains and which accepts generic (non-class-based) inputs should assert that input data are not obviously curvilinear or spherical, commonly through asserting that coordinate columns are not labelled any variant of "longitude" or "latitude", and should implement an appropriate response when such assertions fail. 
#' @rssrTODO SP2.1 Spatial Software which uses either the [`sf`](https://cran.r-project.org/package=sf) of [`sp`](https://cran.r-project.org/package=sp) class systems for representing geographical data should either
#' @rssrTODO SP2.1a Use [`sf`](https://cran.r-project.org/package=sf) rather than [`sp`](https://cran.r-project.org/package=sp), or
#' @rssrTODO SP2.1b Explicitly justify why [`sp`](https://cran.r-project.org/package=sp) is used. 
#' @rssrTODO SP2.2 Geographical Spatial Software should ensure maximal compatibility with established packages and workflows, minimally through:
#' @rssrTODO SP2.2a Clear and extensive documentation demonstrating how routines from that software may be embedded within, or otherwise adapted to, workflows which rely on these established packages; and
#' @rssrTODO SP2.2b Tests which clearly demonstrate that routines from that software may be successfully translated into forms and workflows which rely on these established packages. 
#' @rssrTODO SP2.3 Geographical Spatial Software should be compliant with version 6 (and, ideally 7) of [`proj`](https://proj.org/), and with `wkt2` representations. The primary implications, described in detail in the articles linked to above, are that:
#' @rssrTODO SP2.3a Software should not accept so-called "PROJ4-strings" previously used to specify coordinate reference systems.
#' @rssrTODO SP2.3b Documentation should explicitly clarify whether, and under which conditions, geographical coordinates are expected to be longitude-latitude or latitude-longitude. 
#' @rssrTODO SP2.4 Class systems for input data must contain meta data on associated coordinate reference systems.
#' @rssrTODO SP2.4a Software should provide an ability to convert objects in any new class systems into representations of pre-existing classes such as those listed above.
#' @rssrTODO SP2.5 Spatial Software should explicitly document the types and classes of input data able to be passed to each function.
#' @rssrTODO SP2.6 Spatial Software should accept input data in as many specific spatial classes as possible.
#' @rssrTODO SP2.7 Spatial Software should implement validation routines to confirm that inputs are of acceptable classes (or represented in otherwise appropriate ways for software which does not use class systems).
#' @rssrTODO SP2.8 Spatial Software should implement a single pre-processing routine to validate input data, and to appropriately transform it to a single uniform type to be passed to all subsequent data-processing functions.
#' @rssrTODO SP2.9 The pre-processing function described above should maintain all metadata attributes of input data. 
#' @rssrTODO SP3.0 Spatial software which considers spatial neighbours should enable user control over neighbourhood forms and sizes. In particular:
#' @rssrTODO SP3.0a Neighbours (able to be expressed) on regular grids should be able to be considered in both rectangular only, or rectangular and diagonal (respectively "rook" and "queen" by analogy to chess.
#' @rssrTODO SP3.0b Neighbourhoods in irregular spaces should be minimally able to be controlled via an integer number of neighbours, an area (or equivalent distance defining an area) in which to include neighbours, or otherwise equivalent user-controlled value.
#' @rssrTODO SP3.1 Spatial software which considers spatial neighbours should enable neighbour contributions to be weighted by distance (or other weighting variable), and not rely on a uniform-weight rectangular cut-off.
#' @rssrTODO SP3.2 Spatial software which relies on sampling from input data (even if only of spatial coordinates) should enable sampling procedures to be based on local spatial densities of those input data. 
#' @rssrTODO SP3.3 Spatial regression software should explicitly quantify and distinguish autocovariant or autoregressive processes from those covariant or regressive processes not directly related to spatial structure alone. 
#' @rssrTODO SP3.4 Spatial clustering should not use standard non-spatial clustering algorithms in which spatial proximity is merely represented by an additional weighting factor. Rather, clustering schemes should be derived from explicitly spatial algorithms. 
#' @rssrTODO SP3.5 Spatial machine learning software should ensure that broadcasting procedures for reconciling inputs of different dimensions are not applied.
#' @rssrTODO SP3.6 Spatial machine learning software should ensure that test and training data are spatially distinct, and not simply sampled uniformly from a common region. 
#' @rssrTODO SP4.0 Return values should either:
#' @rssrTODO SP4.0a Be in same class as input data, or
#' @rssrTODO SP4.0b Be in a unique, preferably class-defined, format.
#' @rssrTODO SP4.1 Any units included as attributes of input data should also be included within return values.
#' @rssrTODO SP4.2 The type and class of all return values should be explicitly documented. 
#' @rssrTODO SP4.3 Return values should explicitly include all appropriate units 
#' @rssrTODO SP5.0 Implement default `plot` methods for any implemented class system.
#' @rssrTODO SP5.1 Default to placing the "longitude" or "x" (or equivalent) variable on the horizontal axis.
#' @rssrTODO SP5.2 Ensure that any spatial units associated with input coordinates, and maintained in the return object according to SP4.1, are printed by default on the axes. 
#' @rssrTODO SP5.3 Offer an ability to generate interactive (generally `html`-based) visualisations of results. 
#' @rssrTODO SP6.0 Software which implements routines for transforming coordinates of input data should include tests which demonstrate ability to recover the original coordinates.
#' @rssrTODO SP6.1 All functions which can be applied to both rectilinear and curvilinear data should be tested through application to both.
#' @rssrTODO SP6.1a Functions which may yield inaccurate results when applied to data in one or the other forms (such as the preceding examples of centroids and buffers from ellipsoidal data) should test that results from inappropriate application of those functions are indeed less accurate.
#' @rssrTODO SP6.1b Functions which yield accurate results regardless of whether input data are rectilinear or curvilinear should demonstrate equivalent accuracy in both cases, and should also demonstrate how equivalent results may be obtained through first explicitly transforming input data. 
#' @rssrTODO SP6.2 Geographical Software should include tests with extreme geographical coordinates, minimally including extension to polar extremes of +/-90 degrees. 
#' @rssrTODO SP6.3 Spatial Software which considers spatial neighbours should explicitly test all possible ways of defining them, and should explicitly compare quantitative effects of different ways of defining neighbours.
#' @rssrTODO SP6.4 Spatial Software which considers spatial neighbours should explicitly test effects of different schemes to weight neighbours by spatial proximity. 
#' @rssrTODO SP6.5 Spatial Unsupervised Learning Software which uses clustering algorithms should implement tests which explicitly compare results with equivalent results obtained with a non-spatial clustering algorithm. 
#' @rssrTODO SP6.6 Spatial Machine Learning Software should implement tests which explicitly demonstrate the detrimental consequences of sampling test and training data from the same spatial region, rather than from spatially distinct regions.
#' @noRd
NULL
