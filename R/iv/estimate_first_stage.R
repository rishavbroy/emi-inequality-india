# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' estimate first stage
#'
#' @return A tibble, model object, list, or file path depending on context.
estimate_first_stage <- function(iv_models, district_panel, cfg) {
  data.frame(
    model = names(iv_models),
    status = vapply(iv_models, function(x) x$status %||% "estimated", character(1)),
    stringsAsFactors = FALSE
  )
}

#' tidy first stage results
#'
#' @return A tibble, model object, list, or file path depending on context.
tidy_first_stage_results <- function(first_stage) {
  first_stage
}

#' compute partial f statistics
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_partial_f_statistics <- function(first_stage) {
  first_stage
}

#' compute partial r2
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_partial_r2 <- function(first_stage) {
  first_stage
}
