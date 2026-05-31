# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' extract qmd excerpts
#'
#' @return A tibble, model object, list, or file path depending on context.
extract_qmd_excerpts <- function(source, excerpt_ids) {
  text <- readLines(source, warn = FALSE)
  # TODO: parse fenced Div markers of class sample-excerpt and matching ids.
  text
}

#' extract marked divs
#'
#' @return A tibble, model object, list, or file path depending on context.
extract_marked_divs <- function(text, ids) {
  text
}

#' assemble writing sample qmd
#'
#' @return A tibble, model object, list, or file path depending on context.
assemble_writing_sample_qmd <- function(cover_note, excerpts, output_qmd) {
  writeLines(c(readLines(cover_note, warn = FALSE), excerpts), output_qmd); output_qmd
}

#' validate excerpt ids
#'
#' @return A tibble, model object, list, or file path depending on context.
validate_excerpt_ids <- function(source, excerpt_ids) {
  invisible(TRUE)
}

