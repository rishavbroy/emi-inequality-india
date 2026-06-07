# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' join district panel
#'
#' @return Internal pipeline output used by the targets graph.
join_district_panel <- function(district_tracker, district_join_map, measures_2007, measures_2017, linguistic_distance_iv, boundaries_2020, cfg = list()) {
  build_district_panel(district_tracker, district_join_map, measures_2007, measures_2017, linguistic_distance_iv, boundaries_2020, cfg)
}

#' join panel to geometry
#'
#' @return Internal pipeline output used by the targets graph.
join_panel_to_geometry <- function(panel, geometry) {
  dplyr::left_join(panel, geometry)
}

#' collapse or expand split districts
#'
#' @return Internal pipeline output used by the targets graph.
collapse_or_expand_split_districts <- function(panel) {
  panel
}

#' assert unique panel rows
#'
#' @return Internal pipeline output used by the targets graph.
assert_unique_panel_rows <- function(panel) {
  stopifnot(!anyDuplicated(panel$district_panel_id)); invisible(panel)
}

#' attach spatial ids
#'
#' @return Internal pipeline output used by the targets graph.
attach_spatial_ids <- function(panel) {
  panel
}
