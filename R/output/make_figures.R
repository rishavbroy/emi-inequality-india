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

sf_geometry_coverage <- function(x) {
  if (!has_sf_geometry(x) || !nrow(x)) return(0)
  mean(!sf::st_is_empty(sf::st_geometry(x)))
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
  if (!has_sf_geometry(district_panel)) {
    stop("Final map generation requires an sf district_panel with validated geometry.", call. = FALSE)
  }
  coverage <- sf_geometry_coverage(district_panel)
  if (!is.finite(coverage) || coverage < 0.75) {
    stop(
      "Final map generation requires a validated geometry join covering at least 75% of district-panel rows; current coverage is ",
      round(100 * coverage, 1),
      "%.",
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

  out <- list(
    fig_ilo_trends = figure_spec(
      "fig_ilo_trends",
      "fig_ilo_trends.png",
      "ILO labor market indicators",
      "Composed from the archived ILO figure assets.",
      kind = "ilo_collage",
      sources = raw_ilo_figures
    ),
    district_carveouts_shifts = figure_spec(
      "district_carveouts_shifts",
      "district_carveouts_shifts.png",
      "District carve-outs and shifts",
      kind = "district_carveouts_shifts"
    )
  )

  missing_vars <- setdiff(required_variables, names(as.data.frame(district_panel)))
  geometry_ok <- has_sf_geometry(district_panel) && is.finite(sf_geometry_coverage(district_panel)) && sf_geometry_coverage(district_panel) >= 0.75
  maps_available <- !length(missing_vars) && geometry_ok

  map_specs <- list(
    map_emi_exposure = figure_spec("map_emi_exposure", "map_emi_exposure.png", "EMI Exposure", kind = if (maps_available) "map" else "status", variable = "emie_2007"),
    map_consumption_growth = figure_spec("map_consumption_growth", "map_consumption_growth.png", "% Change in Consumption", kind = if (maps_available) "map" else "status", variable = "consumption_growth_pct"),
    map_pucca = figure_spec("map_pucca", "map_pucca.png", "% Pucca Homes", kind = if (maps_available) "map" else "status", variable = "pucca_share_2007"),
    map_education = figure_spec("map_education", "map_education.png", "% HH Head w/ Sec.+", kind = if (maps_available) "map" else "status", variable = "head_secondary_plus_2007"),
    map_region = figure_spec("map_region", "map_region.png", "Region", kind = if (maps_available) "map" else "status", variable = "region"),
    map_linguistic_distance = figure_spec("map_linguistic_distance", "map_linguistic_distance.png", "Linguistic Distance", kind = if (maps_available) "map" else "status", variable = "wavg_ling_degrees"),
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
    )
  )

  if (!maps_available && !identical(cfg$mode, "final")) {
    # Draft-mode diagnostics live outside outputs/figures/main and are explicitly
    # labeled as diagnostics by figure_output_dir().
    map_specs <- map_specs[c("map_emi_exposure", "map_consumption_growth")]
  }

  map_input_failures <- character()
  if (!maps_available && identical(cfg$mode, "final")) {
    map_input_failures <- c(
      if (length(missing_vars)) paste0("Missing map variables: ", paste(missing_vars, collapse = ", ")),
      paste0("Geometry coverage: ", round(100 * sf_geometry_coverage(district_panel), 1), "%")
    )
  }

  out <- c(out, map_specs)

  if (length(map_input_failures)) {
    attr(out, "legacy_map_input_failures") <- map_input_failures
  }
  attr(out, "district_panel") <- district_panel
  out
}

#' make ilo trends figure
#'
#' @return A vector of source image paths.
make_ilo_trends_figure <- function(raw_ilo_figures) {
  raw_ilo_figures
}

# sample-end: code-output-generation
