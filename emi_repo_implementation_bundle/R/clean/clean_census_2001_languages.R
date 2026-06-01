# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' clean census 2001 languages
#'
#' @return A tibble, model object, list, or file path depending on context.
clean_census_2001_languages <- function(raw) {
  purrr::imap_dfr(raw, ~{ temp <- .x; colnames(temp) <- c("table", "state_code", "district_code", "tehsil_code", "area_name", "mother_tongue_code", "mother_tongue", "spkr_tot", "m_spkr_tot", "f_spkr_tot", "spkr_urban", "m_spkr_urban", "f_spkr_urban", "spkr_rural", "m_spkr_rural", "f_spkr_rural"); temp })
}

#' standardize census state names
#'
#' @return A tibble, model object, list, or file path depending on context.
standardize_census_state_names <- function(df) {
  df
}

#' standardize census district names
#'
#' @return A tibble, model object, list, or file path depending on context.
standardize_census_district_names <- function(df) {
  df
}

#' clean mother tongue names
#'
#' @return A tibble, model object, list, or file path depending on context.
clean_mother_tongue_names <- function(df) {
  df
}

#' compute mother tongue population shares
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_mother_tongue_population_shares <- function(df) {
  df
}

