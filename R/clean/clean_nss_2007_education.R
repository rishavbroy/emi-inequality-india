# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' clean nss 2007 education
#'
clean_nss_2007_education <- function(raw) {
  out <- lapply(raw, std, year = 2007L)
  class(out) <- c("nss_2007_education_clean", class(out))
  out
}

#' standardize edu0708 district codes
#'
standardize_edu0708_district_codes <- function(df) {
  std(df, 2007L)
}
