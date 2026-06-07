# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' clean nss 2007 consumption
#'
#' @return Internal pipeline output used by the targets graph.
clean_nss_2007_consumption <- function(raw) {
  out <- lapply(raw, std, year = 2007L)
  class(out) <- c("nss_2007_consumption_clean", class(out))
  out
}

#' standardize cons0708 hhid
#'
#' @return Internal pipeline output used by the targets graph.
standardize_cons0708_hhid <- function(df) {
  df
}

#' standardize cons0708 district codes
#'
#' @return Internal pipeline output used by the targets graph.
standardize_cons0708_district_codes <- function(df) {
  std(df, 2007L)
}

#' standardize cons0708 weights
#'
#' @return Internal pipeline output used by the targets graph.
standardize_cons0708_weights <- function(df) {
  df
}
