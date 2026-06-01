# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-output-generation

#' make figures
#'
#' @return A tibble, model object, list, or file path depending on context.
make_figures <- function(district_panel, raw_ilo_figures, cfg) {
  list(ilo = make_ilo_trends_figure(raw_ilo_figures))
}

#' make ilo trends figure
#'
#' @return A tibble, model object, list, or file path depending on context.
make_ilo_trends_figure <- function(raw_ilo_figures) {
  raw_ilo_figures
}

#' make emi map
#'
#' @return A tibble, model object, list, or file path depending on context.
make_emi_map <- function(district_panel) {
  district_panel
}

#' make consumption growth map
#'
#' @return A tibble, model object, list, or file path depending on context.
make_consumption_growth_map <- function(district_panel) {
  district_panel
}

#' make pucca map
#'
#' @return A tibble, model object, list, or file path depending on context.
make_pucca_map <- function(district_panel) {
  district_panel
}

#' make education map
#'
#' @return A tibble, model object, list, or file path depending on context.
make_education_map <- function(district_panel) {
  district_panel
}

#' make region map
#'
#' @return A tibble, model object, list, or file path depending on context.
make_region_map <- function(district_panel) {
  district_panel
}

#' make linguistic distance map
#'
#' @return A tibble, model object, list, or file path depending on context.
make_linguistic_distance_map <- function(district_panel) {
  district_panel
}

#' make map collages
#'
#' @return A tibble, model object, list, or file path depending on context.
make_map_collages <- function(figures) {
  figures
}
# sample-end: code-output-generation
