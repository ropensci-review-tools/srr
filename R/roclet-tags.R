
#' Parse rssr tags
#'
#' @param x Input
#' @return Parsed tags
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @noRd
#' @export
roxy_tag_parse.roxy_tag_rssr <- function(x) { # nolint
    roxygen2::tag_markdown (x)
}

#' @importFrom roxygen2 roxy_tag_rd
#' @noRd
#' @export
roxy_tag_rd.roxy_tag_rssr <- function(x, base_path, env) { # nolint
  NULL
}

#' Parse rssrNA tags
#'
#' @param x Input
#' @return Parsed tags
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @noRd
#' @export
roxy_tag_parse.roxy_tag_rssrNA <- function(x) { # nolint
    roxygen2::tag_markdown (x)
}


#' @importFrom roxygen2 roxy_tag_rd
#' @noRd
#' @export
roxy_tag_rd.roxy_tag_rssrNA <- function(x, base_path, env) { # nolint
  NULL
}

#' Parse rssrTODO tags
#'
#' @param x Input
#' @return Parsed tags
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @noRd
#' @export
roxy_tag_parse.roxy_tag_rssrTODO <- function(x) { # nolint
    roxygen2::tag_markdown (x)
}

#' @importFrom roxygen2 roxy_tag_rd
#' @noRd
#' @export
roxy_tag_rd.roxy_tag_rssrTODO <- function(x, base_path, env) { # nolint
  NULL
}

#' Parse rssrVerbose tags
#'
#' @param x Input
#' @return Parsed tags
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @noRd
#' @export
roxy_tag_parse.roxy_tag_rssrVerbose <- function(x) { # nolint
    roxygen2::tag_markdown (x)
}

#' @importFrom roxygen2 roxy_tag_rd
#' @noRd
#' @export
roxy_tag_rd.roxy_tag_rssrVerbose <- function(x, base_path, env) { # nolint
  NULL
}
