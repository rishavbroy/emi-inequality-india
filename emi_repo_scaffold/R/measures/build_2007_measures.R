# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' build 2007 measures
#'
#' @return A tibble, model object, list, or file path depending on context.
build_2007_measures <- function(nss_2007_education, nss_2007_consumption, selection_data, ame_results, cfg) {
  list(education = nss_2007_education, consumption = nss_2007_consumption, selection = selection_data, ames = ame_results)
}

#' compute emie 2007
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_emie_2007 <- function(df) {
  df
}

#' compute enrollment share 2007
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_enrollment_share_2007 <- function(df) {
  df
}

#' compute consumption 2007
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_consumption_2007 <- function(df) {
  df
}

#' compute gini consumption 2007
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_gini_consumption_2007 <- function(df) {
  df
}

#' compute baseline controls 2007
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_baseline_controls_2007 <- function(df) {
  df
}

#' compute education freebies ivs 2007
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_education_freebies_ivs_2007 <- function(df) {
  df
}

