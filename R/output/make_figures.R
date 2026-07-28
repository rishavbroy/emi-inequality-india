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

poster_fixed_effect_term <- function(fixed_effect) {
  switch(
    fixed_effect,
    none = character(),
    region = "factor(region)",
    state = "factor(state_code_2001)",
    stop("Unknown poster fixed-effect specification: ", fixed_effect, call. = FALSE)
  )
}

poster_residual_terms <- function(fixed_effect = "region", controls = census_2001_absorption_controls()) {
  c(poster_fixed_effect_term(fixed_effect), controls)
}

poster_estimable_residual_terms <- function(data, terms) {
  keep <- vapply(terms, function(term) {
    variable <- sub("^factor\\((.*)\\)$", "\\1", term)
    if (!variable %in% names(data)) return(FALSE)
    values <- data[[variable]]
    if (grepl("^factor\\(", term)) {
      return(length(unique(values[!is.na(values)])) >= 2L)
    }
    TRUE
  }, logical(1))
  terms[keep]
}

poster_residualize <- function(data, variable, terms) {
  estimable_terms <- poster_estimable_residual_terms(data, terms)
  fit <- stats::lm(stats::reformulate(estimable_terms, response = variable), data = data)
  stats::residuals(fit)
}

poster_residual_pair <- function(
  panel,
  variables = c("emi_exposure_all_children_0708", "ling_distance_nonzero_mean"),
  fixed_effect = "region",
  controls = census_2001_absorption_controls()
) {
  df <- as.data.frame(panel)
  terms <- poster_residual_terms(fixed_effect, controls)
  required <- unique(c(variables, gsub("^factor\\((.*)\\)$", "\\1", terms)))
  out <- matrix(NA_real_, nrow = nrow(df), ncol = length(variables), dimnames = list(NULL, variables))
  if (length(setdiff(required, names(df)))) return(out)

  keep <- stats::complete.cases(df[, required, drop = FALSE])
  if (!any(keep)) return(out)
  dat <- df[keep, , drop = FALSE]
  for (variable in variables) {
    out[keep, variable] <- poster_residualize(dat, variable, terms)
  }
  out
}

add_poster_residual_variables <- function(district_panel) {
  if (!nrow(as.data.frame(district_panel))) return(district_panel)
  out <- district_panel
  residuals <- poster_residual_pair(out, fixed_effect = "region")
  out$resid_emi_exposure_region_expanded <- residuals[, "emi_exposure_all_children_0708"]
  out$resid_ling_distance_region_expanded <- residuals[, "ling_distance_nonzero_mean"]
  out
}


#' make figures
#'
#' @return A named list of figure specifications consumed by save_figures().
make_figures <- function(district_panel, raw_ilo_figures, cfg, iv_models = NULL, map_geometry = NULL) {
  district_panel <- add_poster_residual_variables(district_panel)

  required_variables <- c(
    "EMIE",
    "emi_exposure_all_children_0708",
    "ling_distance_nonzero_mean",
    "real_log_consumption_change",
    "pct_pucca",
    "pct_head_secondary_plus",
    "region",
    "wavg_ling_degrees",
    "resid_emi_exposure_region_expanded",
    "resid_ling_distance_region_expanded"
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
    ),
    poster_emie_expected_values = figure_spec(
      "poster_emie_expected_values",
      "poster_emie_expected_values.png",
      "Adjusted real consumption growth across EMI exposure",
      "Average counterfactual predictions at observed EMIE percentiles.",
      kind = "emie_expected_values"
    ),
    poster_first_stage_specs = figure_spec(
      "poster_first_stage_specs",
      "poster_first_stage_specs.png",
      "First-stage relationship across specifications",
      "Residualized EMI exposure on residualized linguistic distance.",
      kind = "poster_first_stage_specs"
    ),
    poster_second_stage_specs = figure_spec(
      "poster_second_stage_specs",
      "poster_second_stage_specs.png",
      "Consumption response across IV specifications",
      "Preferred EMI exposure instrumented with preferred linguistic distance.",
      kind = "poster_second_stage_specs"
    )
  )

  missing_vars <- setdiff(required_variables, names(as.data.frame(district_panel)))
  geometry_ok <- has_sf_geometry(district_panel) && is.finite(sf_geometry_coverage(district_panel)) && sf_geometry_coverage(district_panel) >= 0.75
  maps_available <- !length(missing_vars) && geometry_ok

  map_specs <- list(
    map_emi_exposure = figure_spec("map_emi_exposure", "map_emi_exposure.png", "EMI Exposure", kind = if (maps_available) "map" else "status", variable = "EMIE"),
    map_consumption_growth = figure_spec("map_consumption_growth", "map_consumption_growth.png", "Real Log Consumption Change", kind = if (maps_available) "map" else "status", variable = "real_log_consumption_change"),
    map_pucca = figure_spec("map_pucca", "map_pucca.png", "% Pucca Homes", kind = if (maps_available) "map" else "status", variable = "pct_pucca"),
    map_education = figure_spec("map_education", "map_education.png", "% HH Head w/ Sec.+", kind = if (maps_available) "map" else "status", variable = "pct_head_secondary_plus"),
    map_region = figure_spec("map_region", "map_region.png", "Region", kind = if (maps_available) "map" else "status", variable = "region"),
    map_linguistic_distance = figure_spec("map_linguistic_distance", "map_linguistic_distance.png", "Linguistic Distance", kind = if (maps_available) "map" else "status", variable = "wavg_ling_degrees"),
    map_preferred_linguistic_distance = figure_spec(
      "map_preferred_linguistic_distance",
      "map_preferred_linguistic_distance.png",
      "Preferred Linguistic Distance",
      kind = if (maps_available) "map" else "status",
      variable = "ling_distance_nonzero_mean"
    ),
    map_residual_emi_exposure = figure_spec(
      "map_residual_emi_exposure",
      "map_residual_emi_exposure.png",
      "Residual EMI Exposure",
      kind = if (maps_available) "map" else "status",
      variable = "resid_emi_exposure_region_expanded"
    ),
    map_residual_linguistic_distance = figure_spec(
      "map_residual_linguistic_distance",
      "map_residual_linguistic_distance.png",
      "Residual Linguistic Distance",
      kind = if (maps_available) "map" else "status",
      variable = "resid_ling_distance_region_expanded"
    ),
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
      inputs = c("map_region", "map_linguistic_distance", "map_residual_linguistic_distance", "map_residual_emi_exposure")
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
    attr(out, "map_input_failures") <- map_input_failures
  }
  attr(out, "district_panel") <- district_panel
  attr(out, "map_geometry") <- map_geometry
  attr(out, "iv_models") <- iv_models
  out
}


# sample-end: code-output-generation
