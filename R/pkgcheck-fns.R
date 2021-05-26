# These fns are copied straight from pkgcheck, but not exported here

default_branch_qry <- function (gh_cli, org, repo) {

    q <- paste0 ("{
            repository(owner:\"", org, "\", name:\"", repo, "\") {
                       defaultBranchRef {
                           name
                       }
                    }
            }")

    qry <- ghql::Query$new()
    qry$query ("default_branch", q)

    return (qry)
}

#' Get GitHub token
#'
#' @param token_name Optional name of token to use
#' @return The value of the GitHub access token extracted from environment
#' variables.
#' @noRd
get_gh_token <- function (token_name = "") {

    e <- Sys.getenv ()
    if (token_name != "") {

        toks <- unique (e [grep (token_name, names (e))])

    } else {

        toks <- e [grep ("GITHUB", names (e))]
        if (length (unique (toks)) > 1) {
            toks <- toks [grep ("TOKEN|PAT", names (toks))]
        }
        # GitHub runners have "GITHUB_PATH" and "GITHUB_EVENT_PATH"
        if (length (unique (toks)) > 1) {
            toks <- toks [grep ("TOKEN$|PAT$", names (toks))]
        }
    }

    if (length (unique (toks)) > 1) {

        stop ("There are ",
              length (unique (toks)),
              " possible tokens named [",
              paste0 (names (toks), collapse = ", "),
              "]; please ensure one distinct ",
              "token named 'GITHIB_TOKEN' or similar.")
    }

    return (unique (toks))
}

#' get_default_branch
#'
#' @note This function is not intended to be called directly, and is only
#' exported to enable it to be used within the \pkg{plumber} API.
#'
#' @param org Github organization
#' @param repo Github repository
#' @return Name of default branch on GitHub
#' @noRd
get_default_branch <- function (org, repo) {

    token <- get_gh_token ()

    gh_cli <- ghql::GraphqlClient$new (
        url = "https://api.github.com/graphql",
        headers = list (Authorization = paste0 ("Bearer ", token))
    )

    qry <- default_branch_qry (gh_cli, org = org, repo = repo)
    x <- gh_cli$exec(qry$queries$default_branch)
    x <- jsonlite::fromJSON (x)
    branch <- x$data$repository$defaultBranchRef$name

    return (branch)
}
