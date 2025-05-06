repo_is_git <- function (path) {

    g <- tryCatch (
        gert::git_find (path),
        error = function (e) e
    )

    return (!methods::is (g, "libgit2_error"))
}

get_git_remote <- function (path = ".") {

    r <- NULL

    if (fs::dir_exists (fs::path (path, ".git"))) {

        desc <- fs::path (path, "DESCRIPTION")
        if (!fs::file_exists (desc)) {
            return (NULL)
        }

        d <- data.frame (read.dcf (desc))
        if (!"URL" %in% names (d)) {
            return (NULL)
        }

        r <- strsplit (d$URL, ",?\\s+?") [[1]]
        r <- grep ("^https", r, value = TRUE)
        if (length (r) > 1) {
            r <- grep ("git", r, value = TRUE)
        }
        if (length (r) > 1) {
            r <- r [which (!grepl ("\\.io", r))]
        }
        if (grepl ("\\.io", r) && "BugReports" %in% names (d)) {
            r <- strsplit (d$BugReports, ",?\\s+?") [[1]]
            r <- grep ("github\\.com", r, value = TRUE)
            r <- gsub ("\\/issues$", "", r)
        }

    } else if (repo_is_git (path)) {

        r <- gert::git_remote_list (path)$url
        if (length (r) != 1) {
            warning (
                "There appears to be more than one git remote; ",
                "the first will be chosen"
            )
            r <- r [1]
        }

        r <- gsub (".*\\t|\\s.*$", "", r)
    }

    return (r)
}

get_git_branch <- function (path, branch = "") {

    if (!repo_is_git (path)) {
        return (NULL)
    }
    if (is.null (branch)) {
        branch <- ""
    }

    g <- gert::git_info (path)
    if (branch == "") {
        branch <- g$shorthand
    } else if (branch != g$shorthand) {
        gert::git_branch_checkout (branch = branch, repo = path)
    }

    return (branch)
}
