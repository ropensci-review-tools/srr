# functions to check that no standards are listed with multiple tags

msgs_to_standards <- function(msgs) {
  res <- lapply(msgs, function(i) {
    re <- regexpr("(?=\\[).*?(?<=\\])", i, perl = T)
    res <- gsub("^\\[|\\]$", "", regmatches(i, re))
    strsplit(res, ",\\s")[[1]]
  })
  return(unique(unlist(res)))
}

mixed_tag_error <- function(stds1, stds2, tag1, tag2) {
  index <- which(stds1 %in% stds2)
  if (length(index) > 0) {
    stop(
      "Standards [", paste0(stds1[index], collapse = ", "),
      "] are listed with both ", tag1, " and ", tag2, " tags.\n",
      "Please rectify to ensure these standards are only ",
      "associated with one tag.",
      call. = FALSE
    )
  }

  index <- which(stds2 %in% stds1)
  if (length(index) > 0) {
    stop(
      "Standards [", paste0(stds2[index], collapse = ", "),
      "] are listed with both ", tag1, " and ", tag2, " tags.\n",
      "Please rectify to ensure these standards are only ",
      "associated with one tag.",
      call. = FALSE
    )
  }

  return(TRUE)
}

check_no_mixed_tags <- function(msgs, msgs_na, msgs_todo) {
  stds <- msgs_to_standards(msgs)
  stds_na <- msgs_to_standards(msgs_na)
  stds_todo <- msgs_to_standards(msgs_todo)

  chk <- mixed_tag_error(stds, stds_na, "@srrstats", "@srrstatsNA")
  chk <- mixed_tag_error(stds, stds_todo, "@srrstats", "@srrstatsTODO")
  chk <- mixed_tag_error(stds_na, stds_todo, "@srrstatsNA", "@srrstatsTODO")

  invisible(chk)
}
