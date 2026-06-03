# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' estimate spatial iv experimental
#'
#' @return A tibble, model object, list, or file path depending on context.
estimate_spatial_iv_experimental <- function(district_panel, spatial_weights, cfg) {
  list(status = "out_of_active_pipeline", reason = "Experimental spatial IV is documented but not active.")
}

#' add spatial lags
#'
#' @return A tibble, model object, list, or file path depending on context.
add_spatial_lags <- function(district_panel, spatial_weights, vars) {
  district_panel
}

#' fit spatial lag iv attempts
#'
#' @return A tibble, model object, list, or file path depending on context.
fit_spatial_lag_iv_attempts <- function(...) {
  list(status = "out_of_active_pipeline", reason = "Experimental spatial IV is documented but not active.")
}

#' summarize spatial iv failures
#'
#' @return A tibble, model object, list, or file path depending on context.
summarize_spatial_iv_failures <- function(...) {
  tibble::tibble()
}
