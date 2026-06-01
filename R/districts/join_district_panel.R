# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' join district panel
#'
#' @return A tibble, model object, list, or file path depending on context.
join_district_panel <- function(...) {
  stop("TODO")
}

#' join panel to geometry
#'
#' @return A tibble, model object, list, or file path depending on context.
join_panel_to_geometry <- function(panel, geometry) {
  dplyr::left_join(panel, geometry)
}

#' collapse or expand split districts
#'
#' @return A tibble, model object, list, or file path depending on context.
collapse_or_expand_split_districts <- function(panel) {
  panel
}

#' assert unique panel rows
#'
#' @return A tibble, model object, list, or file path depending on context.
assert_unique_panel_rows <- function(panel) {
  stopifnot(!anyDuplicated(panel$district_panel_id)); invisible(panel)
}

#' attach spatial ids
#'
#' @return A tibble, model object, list, or file path depending on context.
attach_spatial_ids <- function(panel) {
  panel
}

