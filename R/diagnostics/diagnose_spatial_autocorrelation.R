# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' diagnose spatial autocorrelation
#'
#' @return A data frame with Moran diagnostics or an explicit inactive status.
diagnose_spatial_autocorrelation <- function(district_panel, iv_models, spatial_weights, cfg) {
  data.frame(
    test = "moran",
    status = "out_of_active_pipeline",
    reason = "Requires a validated district-panel geometry join before Moran diagnostics are reported.",
    stringsAsFactors = FALSE
  )
}

#' compute moran tests
#'
#' @return A tibble placeholder for future Moran diagnostics.
compute_moran_tests <- function(...) {
  tibble::tibble()
}

#' compute monte carlo moran tests
#'
#' @return A tibble placeholder for future Moran diagnostics.
compute_monte_carlo_moran_tests <- function(...) {
  tibble::tibble()
}

#' save spatial autocorrelation diagnostics
#'
#' @return A tibble placeholder for future Moran diagnostics.
save_spatial_autocorrelation_diagnostics <- function(diagnostics) {
  diagnostics
}
