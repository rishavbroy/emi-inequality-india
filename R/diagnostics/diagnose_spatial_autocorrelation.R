# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' diagnose spatial autocorrelation
#'
#' @return A tibble, model object, list, or file path depending on context.
diagnose_spatial_autocorrelation <- function(district_panel, iv_models, spatial_weights, cfg) {
  data.frame(test = "moran", status = "not_run_in_smoke_mode")
}

#' compute moran tests
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_moran_tests <- function(...) {
  tibble::tibble()
}

#' compute monte carlo moran tests
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_monte_carlo_moran_tests <- function(...) {
  tibble::tibble()
}

#' save spatial autocorrelation diagnostics
#'
#' @return A tibble, model object, list, or file path depending on context.
save_spatial_autocorrelation_diagnostics <- function(diagnostics) {
  diagnostics
}
