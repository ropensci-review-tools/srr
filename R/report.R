
#' Generate report from `ssr` tags.
#'
#' @param path Path to package for which report is to be generated
#' @param view If `TRUE` (default), a html-formatted version of the report is
#' opened in default system browser. If `FALSE`, the return object includes the
#' name of a `html`-rendered version of the report in an attribute named 'file'.
#' @param branch By default a report will be generated from the default branch
#' as set on the GitHub repository; this parameter can be used to specify any
#' alternative branch.
#' @return (invisibly) Markdown-formatted lines used to generate the final html
#' document.
#' @family report
#' @export
#' @examples
#' \dontrun{
#' path <- srr_stats_pkg_skeleton ()
#' srr_report (path)
#' }
srr_report <- function (path = ".", branch = "", view = TRUE) {

    requireNamespace ("rmarkdown")

    if (path == ".")
        path <- here::here ()

    remote <- get_git_remote (path)

    if (!is.null (remote) & branch == "")
        branch <- get_default_branch (remote)

    m <- get_all_msgs (path)

    tags <- c ("srrstats", "srrstatsNA", "srrstatsTODO")
    md_lines <- lapply (tags, function (tag) {
                            res <- one_tag_to_markdown (m, remote, tag, branch)
                            if (length (res) > 0)
                                res <- c (paste0 ("## ", tag),
                                          "",
                                          res,
                                          "")
                            return (res)
                        })

    md_lines <- do.call (c, md_lines)

    f <- tempfile (fileext = ".Rmd")
    writeLines (md_lines, con = f)
    out <- paste0 (tools::file_path_sans_ext (f), ".html")
    rmarkdown::render (input = f, output_file = out)

    u <- paste0 ("file://", out)
    if (view)
        utils::browseURL (u)
    else
        attr (md_lines, "file") <- out

    invisible (md_lines)
}

# use dependency-free system command rather than gert to get remote,
# returning NULL is not obviously in git repo, and leaving to error on any other
# issues.
get_git_remote <- function (path = ".") {

    if (!any (grepl ("^\\.git$", list.files (path, all.files = TRUE)))) {

        desc <- file.path (path, "DESCRIPTION")
        if (!file.exists (desc))
            return (NULL)

        d <- data.frame (read.dcf (desc))
        if (!"URL" %in% names (d))
            return (NULL)

        r <- strsplit (d$URL, "\\s+") [[1]]
        r <- grep ("^https", r, value = TRUE)
        if (length (r) > 1)
            r <- grep ("git", r, value = TRUE)
        if (length (r) > 1)
            r <- r [which (!grepl ("\\.io", r))]
    } else {

        wd <- setwd (path)
        r <- system2 ("git", args = "remote -v", stdout = TRUE)
        setwd (wd)

        r <- grep ("fetch\\)$", r, value = TRUE)
        if (length (r) != 1) {
            warning ("There appears to be more than one git remote; ",
                     "the first will be chosen")
            r <- r [1]
        }

        r <- gsub (".*\\t|\\s.*$", "", r)
    }

    return (r)
}

get_all_msgs <- function (path = ".") {

    flist <- list.files (file.path (path, "R"), full.names = TRUE)
    blocks <- lapply (flist, function (i) roxygen2::parse_file (i))
    names (blocks) <- flist
    blocks <- do.call (c, blocks)

    blocks <- collect_blocks (blocks, path)

    msgs <- collect_one_tag (path, blocks, tag = "srrstats")
    msgs_na <- collect_one_tag (path, blocks, tag = "srrstatsNA")
    msgs_todo <- collect_one_tag (path, blocks, tag = "srrstatsTODO")

    list (msgs = msgs,
          msgs_na = msgs_na,
          msgs_todo = msgs_todo)
}

#' one_tag_to_markdown
#'
#' Convert all messages for one defined tag into multiple markdown-formatted
#' lines
#' @param m List of all messages, divided into the 3 categories of tags
#' @noRd
one_tag_to_markdown <- function (m, remote, tag, branch) {

    i <- match (tag, c ("srrstats", "srrstatsNA", "srrstatsTODO"))
    tag <- c ("msgs", "msgs_na", "msgs_todo") [i]
    m <- m [[tag]]

    m <- vapply (m, function (i)
                 one_msg_to_markdown (i, remote, branch),
                 character (1))

    return (m)
}

#' one_msg_to_markdown
#'
#' Convert single-entry character vector of one message into one
#' markdown-formatted line
#' @param m One message
#' @noRd
one_msg_to_markdown <- function (m, remote, branch) {

    g <- gregexpr ("[A-Z]+[0-9]+(\\.[0-9]+)?", m)
    stds <- regmatches (m, g) [[1]]
    stds_g <- sort (stds [grep ("^G", stds)])
    stds_other <- sort (stds [!stds %in% stds_g])
    stds <- c (stds_g, stds_other)

    g <- gregexpr ("\\sline#[0-9]+", m)
    line_num <- NA_integer_
    if (any (g [[1]] > 0))
        line_num <- gsub ("\\sline#", "", regmatches (m, g) [[1]])

    fn <- NA_character_
    if (grepl ("\\sfunction\\s", m)) {
        g <- gregexpr ("\\sfunction\\s+\\'.*\\'", m)
        fn <- gsub ("^\\sfunction\\s+\\'|\\'$", "", regmatches (m, g) [[1]])
    }

    g <- gregexpr ("file\\s+\\[.*\\]$", m)
    file_name <- gsub ("file\\s+\\[|\\]$", "", regmatches (m, g) [[1]])

    if (!is.null (remote)) {

        remote_file <- paste0 (remote, "/blob/", branch, "/", file_name)
        if (!is.na (line_num))
            remote_file <- paste0 (remote_file, "#L", line_num)
    }

    stds <- paste0 (paste0 ("**", stds, "**"), collapse = ", ")

    br_open <- br_close <- ""
    if (!is.null (remote)) {
        br_open <- "["
        br_close <- "]"
    }

    msg <- paste0 ("- ", stds, " in ", br_open)
    if (!is.na (fn)) {
        msg <- paste0 (msg, fn, " of file ")
    }
    msg <- paste0 (msg, file_name, br_close)
    if (!is.null (remote))
        msg <- paste0 (msg, "(", remote_file, ")")

    return (msg)
}


# ---- fns to get default gh branch

get_gh_token <- function (token = "") {

    e <- Sys.getenv ()
    if (token != "") {

        toks <- e [grep (token, names (e))]

    } else {

        toks <- e [grep ("GITHUB", names (e))]
        if (length (toks) > 1)
            toks <- toks [grep ("QL", names (toks))]
    }

    if (length (unique (toks)) > 1)
        stop (paste0 ("No unambiguous token found; please use ",
                      "Sys.setenv() to set a github graphQL tokan ",
                      "named 'GITHUB', 'GITHUBQL', or similar"))
    return (unique (toks))
}

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

get_default_branch <- function (remote) {

    or <- strsplit (remote, "/") [[1]]
    org <- utils::tail (or, 2) [1]
    repo <- utils::tail (or, 1)

    token <- get_gh_token ()

    gh_cli <- ghql::GraphqlClient$new (
        url = "https://api.github.com/graphql",
        headers = list (Authorization = paste0 ("Bearer ", token))
    )

    qry <- default_branch_qry (gh_cli, org = org, repo = repo)
    res <- gh_cli$exec(qry$queries$default_branch)
    x <- jsonlite::fromJSON (res)
    branch <- x$data$repository$defaultBranchRef$name

    return (branch)
}
