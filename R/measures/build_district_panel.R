# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' build district panel
#'
#' @return A tibble, model object, list, or file path depending on context.
build_district_panel <- function(district_tracker, district_join_map, measures_2007, measures_2017, linguistic_distance_iv, boundaries_2020, cfg) {
  list(tracker = district_tracker, join_map = district_join_map, measures_2007 = measures_2007, measures_2017 = measures_2017, iv = linguistic_distance_iv, geometry = boundaries_2020)
}

#' compute consumption growth pct
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_consumption_growth_pct <- function(df) {
  df
}

#' compute log consumption difference
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_log_consumption_difference <- function(df) {
  df
}

#' compute gini change
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_gini_change <- function(df) {
  df
}

#' attach baseline controls
#'
#' @return A tibble, model object, list, or file path depending on context.
attach_baseline_controls <- function(df) {
  df
}

#' attach iv measures
#'
#' @return A tibble, model object, list, or file path depending on context.
attach_iv_measures <- function(df) {
  df
}

#' save processed district panel
#'
#' @return A tibble, model object, list, or file path depending on context.
save_processed_district_panel <- function(district_panel, path = "data/processed/district_panel_emi_consumption_2001_2007_2017_2020.csv") {
  readr::write_csv(tibble::as_tibble(district_panel), path)
  path
}

#' save processed district tracker
#'
#' @return A tibble, model object, list, or file path depending on context.
save_processed_district_tracker <- function(district_tracker, path = "data/processed/district_tracker_2001_2007_2017_2020.csv") {
  readr::write_csv(tibble::as_tibble(district_tracker), path)
  path
}

