add_extra_skeleton_code <- function (path) {
    add_extra_skeleton_code_rust (path)
}

add_extra_skeleton_code_rust <- function (path) {

    d <- fs::dir_create (fs::path (path, "inst", "extdata"), recurse = TRUE)
    f <- fs::path (d, "file.rs")

    code <- c (
        "/// # rust file",
        "///",
        "/// with some text",
        "/// ",
        "/// srr stats",
        "/// ",
        "/// @srrstats {G5.1} A general comment",
        "/// @srrstats {G5.2} Another general comment",
        "/// that continues on a second line.",
        "/// ",
        "/// ## A separate section",
        "/// ",
        "/// here.",
        "fn main() {",
        "}"
    )
    writeLines (code, f)

    return (f)
}
