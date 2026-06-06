# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' clean nss 2007 consumption
#'
#' @return Function-specific return value.
clean_nss_2007_consumption <- function(raw) {
  out <- lapply(raw, std, year = 2007L)
  class(out) <- c("nss_2007_consumption_clean", class(out))
  out
}

#' standardize cons0708 hhid
#'
#' @return Function-specific return value.
standardize_cons0708_hhid <- function(df) {
  df
}

#' standardize cons0708 district codes
#'
#' @return Function-specific return value.
standardize_cons0708_district_codes <- function(df) {
  std(df, 2007L)
}

#' standardize cons0708 weights
#'
#' @return Function-specific return value.
standardize_cons0708_weights <- function(df) {
  df
}
