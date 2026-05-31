# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' render writing samples
#'
#' @return A tibble, model object, list, or file path depending on context.
render_writing_samples <- function(spec_dir = "application-samples/specs") {
  specs <- list.files(spec_dir, pattern = "^writing-.*\.yml$", full.names = TRUE)
  purrr::map_chr(specs, render_one_writing_sample)
}

#' render one writing sample
#'
#' @return A tibble, model object, list, or file path depending on context.
render_one_writing_sample <- function(spec_path) {
  spec <- yaml::read_yaml(spec_path)
  # TODO: extract marked report excerpts, prepend cover note, render to spec$output.
  spec$output
}

#' prepend cover note qmd
#'
#' @return A tibble, model object, list, or file path depending on context.
prepend_cover_note_qmd <- function(cover_note, body) {
  c(readLines(cover_note, warn = FALSE), body)
}

#' write sample output path
#'
#' @return A tibble, model object, list, or file path depending on context.
write_sample_output_path <- function(spec) {
  spec$output
}

