# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-spatial-weights

#' build spatial weights
#'
#' @return A tibble, model object, list, or file path depending on context.
build_spatial_weights <- function(district_panel, cfg) {
  if (inherits(district_panel, "sf")) { nb <- spdep::poly2nb(district_panel, queen = FALSE); spdep::nb2listw(nb, style = "W", zero.policy = TRUE) } else NULL
}

#' diagnose spatial weights
#'
#' @return A tibble, model object, list, or file path depending on context.
diagnose_spatial_weights <- function(district_panel, spatial_weights, cfg) {
  list(weights = spatial_weights)
}

#' compare rook queen contiguity
#'
#' @return A tibble, model object, list, or file path depending on context.
compare_rook_queen_contiguity <- function(district_panel) {
  tibble::tibble()
}

#' summarize islands
#'
#' @return A tibble, model object, list, or file path depending on context.
summarize_islands <- function(spatial_weights) {
  tibble::tibble()
}

#' summarize neighbor counts
#'
#' @return A tibble, model object, list, or file path depending on context.
summarize_neighbor_counts <- function(spatial_weights) {
  tibble::tibble()
}

#' save spatial weight diagnostics
#'
#' @return A tibble, model object, list, or file path depending on context.
save_spatial_weight_diagnostics <- function(diagnostics) {
  diagnostics
}

# sample-end: code-spatial-weights
