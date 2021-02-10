
#' Parse rrrstats tags
#'
#' @param x Input
#' @return Parsed tags
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @noRd
#' @export
roxy_tag_parse.roxy_tag_rrrstats <- function(x) { # nolint
    roxygen2::tag_markdown (x)
}

#' @importFrom roxygen2 roxy_tag_rd
#' @noRd
#' @export
roxy_tag_rd.roxy_tag_rrrstats <- function(x, base_path, env) { # nolint
  NULL
}

#' Parse rrrstatsNA tags
#'
#' @param x Input
#' @return Parsed tags
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @noRd
#' @export
roxy_tag_parse.roxy_tag_rrrstatsNA <- function(x) { # nolint
    roxygen2::tag_markdown (x)
}


#' @importFrom roxygen2 roxy_tag_rd
#' @noRd
#' @export
roxy_tag_rd.roxy_tag_rrrstatsNA <- function(x, base_path, env) { # nolint
  NULL
}

#' Parse rrrstatsTODO tags
#'
#' @param x Input
#' @return Parsed tags
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @noRd
#' @export
roxy_tag_parse.roxy_tag_rrrstatsTODO <- function(x) { # nolint
    roxygen2::tag_markdown (x)
}

#' @importFrom roxygen2 roxy_tag_rd
#' @noRd
#' @export
roxy_tag_rd.roxy_tag_rrrstatsTODO <- function(x, base_path, env) { # nolint
  NULL
}

#' Parse rrrstatsVerbose tags
#'
#' @param x Input
#' @return Parsed tags
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @noRd
#' @export
roxy_tag_parse.roxy_tag_rrrstatsVerbose <- function(x) { # nolint
    roxygen2::tag_markdown (x)
}

#' @importFrom roxygen2 roxy_tag_rd
#' @noRd
#' @export
roxy_tag_rd.roxy_tag_rrrstatsVerbose <- function(x, base_path, env) { # nolint
  NULL
}
