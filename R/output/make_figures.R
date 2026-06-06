# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-output-generation

figure_spec <- function(name, file, title, subtitle = NULL, kind = "status", variable = NULL, sources = NULL, inputs = NULL) {
  out <- list(
    name = name,
    file = file,
    title = title,
    subtitle = subtitle,
    kind = kind,
    variable = variable,
    sources = sources,
    inputs = inputs
  )
}

has_sf_geometry <- function(x) {
  inherits(x, "sf") && !is.null(attr(x, "sf_column")) && attr(x, "sf_column") %in% names(x)
}

require_final_figure_inputs <- function(district_panel, cfg, required_variables) {
  if (!identical(cfg$mode, "final")) return(invisible(TRUE))
  missing_vars <- setdiff(required_variables, names(as.data.frame(district_panel)))
  if (length(missing_vars)) {
    stop(
      "Final figure generation requires mapped variables. Missing variables: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' make figures
#'
#' @return A named list of figure specifications consumed by save_figures().
make_figures <- function(district_panel, raw_ilo_figures, cfg) {
  required_variables <- c(
    "emie_2007",
    "consumption_growth_pct",
    "pucca_share_2007",
    "head_secondary_plus_2007",
    "region",
    "wavg_ling_degrees"
  )
  require_final_figure_inputs(district_panel, cfg, required_variables)

  out <- list(
    fig_ilo_trends = figure_spec(
      "fig_ilo_trends",
      "fig_ilo_trends.png",
      "ILO labor market indicators",
      "Composed from the archived ILO figure assets.",
      kind = "ilo_collage",
      sources = raw_ilo_figures
    ),
    map_emi_exposure = figure_spec("map_emi_exposure", "map_emi_exposure.png", "EMI exposure by district", variable = "emie_2007"),
    map_consumption_growth = figure_spec("map_consumption_growth", "map_consumption_growth.png", "Consumption growth by district", variable = "consumption_growth_pct"),
    map_pucca = figure_spec("map_pucca", "map_pucca.png", "Pucca housing share by district", variable = "pucca_share_2007"),
    map_education = figure_spec("map_education", "map_education.png", "Household head secondary education by district", variable = "head_secondary_plus_2007"),
    map_region = figure_spec("map_region", "map_region.png", "Region assignment by district", variable = "region"),
    map_linguistic_distance = figure_spec("map_linguistic_distance", "map_linguistic_distance.png", "Linguistic distance from Hindi by district", variable = "wavg_ling_degrees"),
    collage_main_maps = figure_spec(
      "collage_main_maps",
      "collage_main_maps.png",
      "Main district-level map inputs",
      kind = "collage",
      inputs = c("map_emi_exposure", "map_consumption_growth", "map_pucca", "map_education")
    ),
    collage_iv_region_maps = figure_spec(
      "collage_iv_region_maps",
      "collage_iv_region_maps.png",
      "Instrument and region map inputs",
      kind = "collage",
      inputs = c("map_linguistic_distance", "map_region")
    ),
    district_carveouts_shifts = figure_spec(
      "district_carveouts_shifts",
      "district_carveouts_shifts.png",
      "District carve-outs, shifts, and non-partitions",
      "Source coverage in the active district-tracker inputs.",
      kind = "district_tracker_summary"
    )
  )
  attr(out, "district_panel") <- district_panel
  out
}

#' make ilo trends figure
#'
#' @return A vector of source image paths.
make_ilo_trends_figure <- function(raw_ilo_figures) {
  raw_ilo_figures
}

#' make emi map
#'
#' @return The district panel used for map generation.
make_emi_map <- function(district_panel) {
  district_panel
}

#' make consumption growth map
#'
#' @return The district panel used for map generation.
make_consumption_growth_map <- function(district_panel) {
  district_panel
}

#' make pucca map
#'
#' @return The district panel used for map generation.
make_pucca_map <- function(district_panel) {
  district_panel
}

#' make education map
#'
#' @return The district panel used for map generation.
make_education_map <- function(district_panel) {
  district_panel
}

#' make region map
#'
#' @return The district panel used for map generation.
make_region_map <- function(district_panel) {
  district_panel
}

#' make linguistic distance map
#'
#' @return The district panel used for map generation.
make_linguistic_distance_map <- function(district_panel) {
  district_panel
}

#' make map collages
#'
#' @return A list of figure specifications.
make_map_collages <- function(figures) {
  figures
}
# sample-end: code-output-generation
