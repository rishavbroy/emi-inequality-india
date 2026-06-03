# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' build district panel
#'
#' @return A tibble, model object, list, or file path depending on context.
build_district_panel <- function(district_tracker, district_join_map, measures_2007, measures_2017, linguistic_distance_iv, boundaries_2020, cfg) {
  out <- safe_df(measures_2007)
  if (!nrow(out)) return(empty_panel())
  if (all(c("state_std", "district_std") %in% names(measures_2017))) {
    out <- merge(out, measures_2017, by = c("state_std", "district_std"), all.x = TRUE, suffixes = c("_2007", "_2017"))
  }
  if (all(c("state_std", "district_std") %in% names(linguistic_distance_iv))) {
    out <- merge(out, linguistic_distance_iv, by = c("state_std", "district_std"), all.x = TRUE)
  }
  if (!"district_panel_id" %in% names(out)) {
    out$district_panel_id <- make_district_key(out$state_std, out$district_std, 2007L)
  }
  out
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
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(as.data.frame(district_panel), path, row.names = FALSE)
  path
}

#' save processed district tracker
#'
#' @return A tibble, model object, list, or file path depending on context.
save_processed_district_tracker <- function(district_tracker, path = "data/processed/district_tracker_2001_2007_2017_2020.csv") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(as.data.frame(district_tracker), path, row.names = FALSE)
  path
}
