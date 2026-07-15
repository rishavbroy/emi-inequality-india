# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' clean nss 2007 consumption
#'
clean_nss_2007_consumption <- function(raw) {
  out <- lapply(raw, std, year = 2007L)
  class(out) <- c("nss_2007_consumption_clean", class(out))
  out
}

#' standardize cons0708 district codes
#'
standardize_cons0708_district_codes <- function(df) {
  std(df, 2007L)
}
