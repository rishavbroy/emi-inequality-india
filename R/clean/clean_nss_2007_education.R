# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' clean nss 2007 education
#'
clean_nss_2007_education <- function(raw) {
  out <- lapply(raw, std, year = 2007L)
  class(out) <- c("nss_2007_education_clean", class(out))
  out
}

#' clean edu0708 households
#'
clean_edu0708_households <- function(block3) {
  block3
}

#' clean edu0708 members
#'
clean_edu0708_members <- function(block4) {
  block4
}

#' clean edu0708 schooling
#'
clean_edu0708_schooling <- function(block5) {
  block5
}

#' clean edu0708 private expenditure
#'
clean_edu0708_private_expenditure <- function(block6) {
  block6
}

#' standardize edu0708 weights
#'
standardize_edu0708_weights <- function(df) {
  df
}

#' standardize edu0708 district codes
#'
standardize_edu0708_district_codes <- function(df) {
  std(df, 2007L)
}
