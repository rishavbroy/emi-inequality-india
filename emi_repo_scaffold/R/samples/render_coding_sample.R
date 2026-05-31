# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' render coding samples
#'
#' @return A tibble, model object, list, or file path depending on context.
render_coding_samples <- function(spec_dir = "application-samples/specs") {
  specs <- list.files(spec_dir, pattern = "^coding-.*\.yml$", full.names = TRUE)
  purrr::map_chr(specs, render_one_coding_sample)
}

#' render one coding sample
#'
#' @return A tibble, model object, list, or file path depending on context.
render_one_coding_sample <- function(spec_path) {
  spec <- yaml::read_yaml(spec_path)
  # TODO: extract marked code excerpts, prepend cover note, render to spec$output.
  spec$output
}

#' format code excerpt
#'
#' @return A tibble, model object, list, or file path depending on context.
format_code_excerpt <- function(code, title = NULL) {
  c(if (!is.null(title)) paste0("## ", title), "```r", code, "```")
}

