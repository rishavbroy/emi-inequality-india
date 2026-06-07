# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' estimate spatial iv experimental
#'
#' @return Internal pipeline output used by the targets graph.
estimate_spatial_iv_experimental <- function(district_panel, spatial_weights, cfg) {
  list(status = "out_of_active_pipeline", reason = "Experimental spatial IV is documented but not active.")
}

#' add spatial lags
#'
#' @return Internal pipeline output used by the targets graph.
add_spatial_lags <- function(district_panel, spatial_weights, vars) {
  district_panel
}

#' fit spatial lag iv attempts
#'
#' @return Internal pipeline output used by the targets graph.
fit_spatial_lag_iv_attempts <- function(...) {
  list(status = "out_of_active_pipeline", reason = "Experimental spatial IV is documented but not active.")
}

#' summarize spatial iv failures
#'
#' @return Internal pipeline output used by the targets graph.
summarize_spatial_iv_failures <- function(...) {
  tibble::tibble()
}
