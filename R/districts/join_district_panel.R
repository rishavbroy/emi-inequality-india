# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' join district panel
#'
join_district_panel <- function(district_tracker, district_join_map, measures_2007, measures_2017, linguistic_distance_iv, boundaries_2020, cfg = list()) {
  build_district_panel(district_tracker, district_join_map, measures_2007, measures_2017, linguistic_distance_iv, boundaries_2020, cfg)
}

#' join panel to geometry
#'
join_panel_to_geometry <- function(panel, geometry) {
  dplyr::left_join(panel, geometry)
}

#' collapse or expand split districts
#'
collapse_or_expand_split_districts <- function(panel) {
  panel
}

#' assert unique panel rows
#'
assert_unique_panel_rows <- function(panel) {
  stopifnot(!anyDuplicated(panel$district_panel_id)); invisible(panel)
}

#' attach spatial ids
#'
attach_spatial_ids <- function(panel) {
  panel
}
