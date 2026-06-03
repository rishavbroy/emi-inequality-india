# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' clean district boundaries
#'
#' @return A tibble, model object, list, or file path depending on context.
clean_district_boundaries <- function(raw_sf) {
  df <- safe_df(raw_sf)
  if ("dtname" %in% names(df)) df$district_20 <- df$dtname
  if ("stname" %in% names(df)) df$state_20 <- df$stname
  if ("DT_NM" %in% names(df)) df$district_20 <- df$DT_NM
  if ("ST_NM" %in% names(df)) df$state_20 <- df$ST_NM
  std(df, 2020L)
}

#' standardize boundary state names
#'
#' @return A tibble, model object, list, or file path depending on context.
standardize_boundary_state_names <- function(df) {
  std(df, 2020L)
}

#' standardize boundary district names
#'
#' @return A tibble, model object, list, or file path depending on context.
standardize_boundary_district_names <- function(df) {
  std(df, 2020L)
}

#' repair invalid geometries
#'
#' @return A tibble, model object, list, or file path depending on context.
repair_invalid_geometries <- function(sf_df) {
  sf::st_make_valid(sf_df)
}

#' add boundary join ids
#'
#' @return A tibble, model object, list, or file path depending on context.
add_boundary_join_ids <- function(df) {
  df
}
