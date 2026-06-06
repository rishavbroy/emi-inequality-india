# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' clean nss 2017 education
#'
#' @return Function-specific return value.
clean_nss_2017_education <- function(raw) {
  out <- lapply(raw, std, year = 2017L)
  class(out) <- c("nss_2017_education_clean", class(out))
  out
}

#' standardize edu1718 hhid
#'
#' @return Function-specific return value.
standardize_edu1718_hhid <- function(df) {
  df
}

#' standardize edu1718 district codes
#'
#' @return Function-specific return value.
standardize_edu1718_district_codes <- function(df) {
  std(df, 2017L)
}

#' standardize edu1718 weights
#'
#' @return Function-specific return value.
standardize_edu1718_weights <- function(df) {
  df
}

#' join 2017 state district labels
#'
#' @return Function-specific return value.
join_2017_state_district_labels <- function(df, districts, state_codes) {
  df
}
