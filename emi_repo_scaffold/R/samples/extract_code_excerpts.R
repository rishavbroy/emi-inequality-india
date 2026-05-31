# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' extract code excerpts
#'
#' @return A tibble, model object, list, or file path depending on context.
extract_code_excerpts <- function(spec) {
  purrr::map_chr(spec$excerpts, ~ extract_between_sample_markers(.x$file, .x$id))
}

#' extract between sample markers
#'
#' @return A tibble, model object, list, or file path depending on context.
extract_between_sample_markers <- function(file, id) {
  x <- readLines(file, warn = FALSE)
  start <- grep(paste0("# sample-start: ", id, "$"), x, fixed = FALSE)
  end <- grep(paste0("# sample-end: ", id, "$"), x, fixed = FALSE)
  if (!length(start) || !length(end)) stop("Missing sample markers for ", id, " in ", file)
  paste(x[(start[1] + 1):(end[1] - 1)], collapse = "\n")
}

#' assemble coding sample qmd
#'
#' @return A tibble, model object, list, or file path depending on context.
assemble_coding_sample_qmd <- function(cover_note, code_blocks, output_qmd) {
  writeLines(c(readLines(cover_note, warn = FALSE), code_blocks), output_qmd); output_qmd
}

#' validate code excerpt markers
#'
#' @return A tibble, model object, list, or file path depending on context.
validate_code_excerpt_markers <- function(spec) {
  invisible(TRUE)
}

