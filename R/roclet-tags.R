
#' Parse srrstats tags
#'
#' @param x Input
#' @return Parsed tags
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @noRd
#' @export
roxy_tag_parse.roxy_tag_srrstats <- function(x) { # nolint
    roxygen2::tag_markdown (x)
}

#' @importFrom roxygen2 roxy_tag_rd
#' @noRd
#' @export
roxy_tag_rd.roxy_tag_srrstats <- function(x, base_path, env) { # nolint
  NULL
}

#' Parse srrstatsNA tags
#'
#' @param x Input
#' @return Parsed tags
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @noRd
#' @export
roxy_tag_parse.roxy_tag_srrstatsNA <- function(x) { # nolint
    roxygen2::tag_markdown (x)
}


#' @importFrom roxygen2 roxy_tag_rd
#' @noRd
#' @export
roxy_tag_rd.roxy_tag_srrstatsNA <- function(x, base_path, env) { # nolint
  NULL
}

#' Parse srrstatsTODO tags
#'
#' @param x Input
#' @return Parsed tags
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @noRd
#' @export
roxy_tag_parse.roxy_tag_srrstatsTODO <- function(x) { # nolint
    roxygen2::tag_markdown (x)
}

#' @importFrom roxygen2 roxy_tag_rd
#' @noRd
#' @export
roxy_tag_rd.roxy_tag_srrstatsTODO <- function(x, base_path, env) { # nolint
  NULL
}

#' Parse srrstatsVerbose tags
#'
#' @param x Input
#' @return Parsed tags
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @noRd
#' @export
roxy_tag_parse.roxy_tag_srrstatsVerbose <- function(x) { # nolint
    roxygen2::tag_markdown (x)
}

#' @importFrom roxygen2 roxy_tag_rd
#' @noRd
#' @export
roxy_tag_rd.roxy_tag_srrstatsVerbose <- function(x, base_path, env) { # nolint
  NULL
}
