# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-multicollinearity-spatial-autocorrelation

#' diagnose multicollinearity
#'
#' @return A tibble, model object, list, or file path depending on context.
diagnose_multicollinearity <- function(district_panel, iv_models, cfg) {
  data.frame(test = "kappa", status = "not_run_in_smoke_mode")
}

#' compute design matrix rank
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_design_matrix_rank <- function(formula, data) {
  X <- stats::model.matrix(formula, data = data); tibble::tibble(rank = qr(X)$rank, ncol = ncol(X))
}

#' compute kappa
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_kappa <- function(formula, data) {
  X <- stats::model.matrix(formula, data = data); kappa(X, exact = TRUE)
}

#' compute vif if applicable
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_vif_if_applicable <- function(model) {
  if (requireNamespace("car", quietly = TRUE)) car::vif(model) else NULL
}

#' save multicollinearity diagnostics
#'
#' @return A tibble, model object, list, or file path depending on context.
save_multicollinearity_diagnostics <- function(diagnostics) {
  diagnostics
}

# sample-end: code-multicollinearity-spatial-autocorrelation
