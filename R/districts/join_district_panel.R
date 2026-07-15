# Compatibility alias for the active district-panel builder.
# Geometry attachment and uniqueness validation live in dedicated deep modules:
# R/districts/geometry_attachment.R and R/districts/validate_district_panel.R.

#' Join district sources into the analysis panel
#'
#' Backward-compatible wrapper around `build_district_panel()`. New target code
#' should call `build_district_panel()` directly.
join_district_panel <- function(district_tracker, district_join_map, measures_2007, measures_2017, linguistic_distance_iv, boundaries_2020, cfg = list(), legacy_district_tracker = NULL) {
  build_district_panel(district_tracker, district_join_map, measures_2007, measures_2017, linguistic_distance_iv, boundaries_2020, cfg, legacy_district_tracker = legacy_district_tracker)
}
