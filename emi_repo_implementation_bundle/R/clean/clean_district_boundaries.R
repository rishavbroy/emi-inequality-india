# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' clean district boundaries
#'
#' @return A tibble, model object, list, or file path depending on context.
clean_district_boundaries <- function(raw_sf) {
  raw_sf |> dplyr::rename(district_20 = dtname, state_20 = stname) |> dplyr::mutate(state_20 = stringr::str_to_title(state_20))
}

#' standardize boundary state names
#'
#' @return A tibble, model object, list, or file path depending on context.
standardize_boundary_state_names <- function(df) {
  df
}

#' standardize boundary district names
#'
#' @return A tibble, model object, list, or file path depending on context.
standardize_boundary_district_names <- function(df) {
  df
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

