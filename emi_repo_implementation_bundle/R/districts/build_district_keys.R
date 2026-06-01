# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' canonicalize district name
#'
#' @return A tibble, model object, list, or file path depending on context.
canonicalize_district_name <- function(x) {
  x |> stringr::str_to_lower() |> stringr::str_replace_all("[^a-z0-9]+", " ") |> stringr::str_squish()
}

#' canonicalize state name
#'
#' @return A tibble, model object, list, or file path depending on context.
canonicalize_state_name <- function(x) {
  canonicalize_district_name(x)
}

#' make district key
#'
#' @return A tibble, model object, list, or file path depending on context.
make_district_key <- function(state, district, year) {
  paste(year, canonicalize_state_name(state), canonicalize_district_name(district), sep = "__")
}

#' build district keys 2001
#'
#' @return A tibble, model object, list, or file path depending on context.
build_district_keys_2001 <- function(census_2001_languages) {
  census_2001_languages
}

#' build district keys 2007
#'
#' @return A tibble, model object, list, or file path depending on context.
build_district_keys_2007 <- function(nss_2007_education, nss_2007_consumption) {
  list(education = nss_2007_education, consumption = nss_2007_consumption)
}

#' build district keys 2017
#'
#' @return A tibble, model object, list, or file path depending on context.
build_district_keys_2017 <- function(nss_2017_education) {
  nss_2017_education
}

#' build district keys 2020
#'
#' @return A tibble, model object, list, or file path depending on context.
build_district_keys_2020 <- function(boundaries_2020) {
  boundaries_2020
}

