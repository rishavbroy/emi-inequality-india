# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' clean district boundaries
#'
#' @return An sf/data frame with canonical state and district join keys.
clean_district_boundaries <- function(raw_sf) {
  df <- raw_sf
  if (!inherits(df, "sf")) df <- safe_df(df)
  if ("dtname" %in% names(df)) df$district_20 <- df$dtname
  if ("stname" %in% names(df)) df$state_20 <- df$stname
  if ("DT_NM" %in% names(df)) df$district_20 <- df$DT_NM
  if ("ST_NM" %in% names(df)) df$state_20 <- df$ST_NM

  state_code_col <- first_col(df, c("stcode11", "STCODE11", "state_code", "StateCode"))
  district_code_col <- first_col(df, c("dtcode11", "DTCODE11", "district_code", "DistrictCode"))
  state_col <- first_col(df, c("state_20", "stname", "ST_NM", "state", "State"))
  district_col <- first_col(df, c("district_20", "dtname", "DT_NM", "district", "District"))
  if (!is.null(state_code_col)) {
    df$state_std <- normalize_numeric_join_code(df[[state_code_col]])
  } else if (!is.null(state_col)) {
    df$state_std <- canonicalize_state_name(df[[state_col]])
  }
  if (!is.null(district_code_col)) {
    df$district_std <- normalize_numeric_join_code(df[[district_code_col]])
  } else if (!is.null(district_col)) {
    df$district_std <- canonicalize_district_name(df[[district_col]])
  }
  df$source_year <- rep(2020L, nrow(df))

  if (inherits(df, "sf")) df <- repair_invalid_geometries(df)
  df
}

normalize_numeric_join_code <- function(x) {
  x <- trimws(as.character(x))
  numeric <- grepl("^[0-9]+$", x)
  x[numeric] <- as.character(as.integer(x[numeric]))
  x
}

#' standardize boundary state names
#'
#' @return Function-specific return value.
standardize_boundary_state_names <- function(df) {
  std(df, 2020L)
}

#' standardize boundary district names
#'
#' @return Function-specific return value.
standardize_boundary_district_names <- function(df) {
  std(df, 2020L)
}

#' repair invalid geometries
#'
#' @return Function-specific return value.
repair_invalid_geometries <- function(sf_df) {
  if (!inherits(sf_df, "sf")) return(sf_df)
  sf::st_make_valid(sf_df)
}

#' add boundary join ids
#'
#' @return Function-specific return value.
add_boundary_join_ids <- function(df) {
  df
}
