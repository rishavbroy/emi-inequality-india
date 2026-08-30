# Cross-vintage geography policy and diagnostics.
#
# Evidence acquisition stays in source-specific lineage modules. This file owns
# only two source-agnostic contracts needed before fractional interpolation:
# (1) what geographic operations different quantity types permit, and
# (2) how canonical transition graphs from multiple vintages are connected.

geography_allocation_semantics_registry <- function() {
  data.frame(
    semantic_id = c(
      "extensive_human",
      "ratio_human",
      "survey_microdata",
      "land_area",
      "point_location",
      "spatial_surface"
    ),
    aggregation_operation = c(
      "sum_sufficient_statistics",
      "aggregate_numerator_denominator",
      "reweight_records",
      "sum_or_area_allocate",
      "direct_location",
      "overlay_or_area_allocate"
    ),
    preferred_fractional_basis = c(
      "population",
      "population",
      "reviewed_record_allocation",
      "area",
      "direct_location",
      "area"
    ),
    population_fractional_allowed = c(
      TRUE, TRUE, TRUE, FALSE, FALSE, FALSE
    ),
    area_fractional_allowed = c(
      FALSE, FALSE, FALSE, TRUE, FALSE, TRUE
    ),
    requires_sufficient_statistics = c(
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
    ),
    examples = c(
      "population;workers;literates;enrollment",
      "shares;rates;EMIE;linguistic distance",
      "consumption;household or person surveys",
      "land area;forest;cropland",
      "schools;hospitals;banks with coordinates",
      "rainfall;gridded environmental surfaces"
    ),
    stringsAsFactors = FALSE
  )
}

validate_geography_allocation_semantics <- function(
    registry = geography_allocation_semantics_registry()) {
  x <- safe_df(registry)
  required <- c(
    "semantic_id", "aggregation_operation",
    "preferred_fractional_basis",
    "population_fractional_allowed", "area_fractional_allowed",
    "requires_sufficient_statistics", "examples"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Geography allocation semantics lack: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyNA(x$semantic_id) || any(!nzchar(plain_chr(x$semantic_id))) ||
      anyDuplicated(x$semantic_id)) {
    stop(
      "Geography allocation semantic IDs must be unique and non-empty.",
      call. = FALSE
    )
  }
  allowed_basis <- c(
    "population", "reviewed_record_allocation",
    "area", "direct_location"
  )
  invalid_basis <- setdiff(
    unique(plain_chr(x$preferred_fractional_basis)),
    allowed_basis
  )
  if (length(invalid_basis)) {
    stop(
      "Unsupported geography fractional basis: ",
      paste(invalid_basis, collapse = ", "),
      call. = FALSE
    )
  }
  if (any(
      x$population_fractional_allowed %in% TRUE &
        x$area_fractional_allowed %in% TRUE
    )) {
    stop(
      "Allocation semantics cannot silently treat population and area weights as interchangeable.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

geography_measure_family_registry <- function() {
  data.frame(
    measure_family = c(
      "census_extensive_counts",
      "census_rates_and_shares",
      "eventual_emie",
      "linguistic_distance",
      "consumption_welfare",
      "survey_person_outcomes",
      "land_quantities",
      "point_facilities",
      "environmental_surfaces"
    ),
    semantic_id = c(
      "extensive_human",
      "ratio_human",
      "ratio_human",
      "ratio_human",
      "survey_microdata",
      "survey_microdata",
      "land_area",
      "point_location",
      "spatial_surface"
    ),
    sufficient_statistics = c(
      "count",
      "numerator+denominator",
      "enrolled_weight+eligible_weight",
      "speaker_count+distance_components",
      "record_weight+household_size+lineage_weight",
      "record_weight+lineage_weight",
      "quantity+area",
      "coordinates_or_locality_id",
      "raster_or_polygon_support"
    ),
    stringsAsFactors = FALSE
  )
}

validate_geography_measure_families <- function(
    registry = geography_measure_family_registry(),
    semantics = geography_allocation_semantics_registry()) {
  x <- safe_df(registry)
  required <- c(
    "measure_family", "semantic_id", "sufficient_statistics"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Geography measure-family registry lacks: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyNA(x$measure_family) ||
      any(!nzchar(plain_chr(x$measure_family))) ||
      anyDuplicated(x$measure_family)) {
    stop(
      "Geography measure-family IDs must be unique and non-empty.",
      call. = FALSE
    )
  }
  validate_geography_allocation_semantics(semantics)
  unknown <- setdiff(
    unique(plain_chr(x$semantic_id)),
    plain_chr(semantics$semantic_id)
  )
  if (length(unknown)) {
    stop(
      "Geography measure families reference unknown semantics: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

geography_specification_registry <- function() {
  data.frame(
    geography_spec_id = c(
      "G0_exact_only",
      "G1_deterministic_amalgamation",
      "G2_population_interpolated",
      "G3_area_interpolated",
      "G4_reviewed_fractional"
    ),
    allows_fractional_allocation = c(
      FALSE, FALSE, TRUE, TRUE, TRUE
    ),
    fractional_basis = c(
      NA_character_,
      NA_character_,
      "population",
      "area",
      "reviewed"
    ),
    description = c(
      "Exact one-to-one or directly identified geography only.",
      "Exact closed constant-boundary components; no fractional allocation.",
      "G1 plus population-weighted unresolved changes for compatible human quantities.",
      "G1 plus area-weighted unresolved changes for area-compatible quantities.",
      "G1 plus externally reviewed source-specific fractional allocation."
    ),
    stringsAsFactors = FALSE
  )
}

validate_geography_specifications <- function(
    registry = geography_specification_registry()) {
  x <- safe_df(registry)
  required <- c(
    "geography_spec_id", "allows_fractional_allocation",
    "fractional_basis", "description"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Geography specification registry lacks: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyNA(x$geography_spec_id) ||
      any(!nzchar(plain_chr(x$geography_spec_id))) ||
      anyDuplicated(x$geography_spec_id)) {
    stop(
      "Geography specification IDs must be unique and non-empty.",
      call. = FALSE
    )
  }
  exact <- !x$allows_fractional_allocation
  if (any(!is.na(x$fractional_basis[exact]))) {
    stop(
      "Exact geography specifications cannot declare a fractional basis.",
      call. = FALSE
    )
  }
  fractional <- x$allows_fractional_allocation
  allowed <- c("population", "area", "reviewed")
  invalid <- setdiff(
    unique(plain_chr(x$fractional_basis[fractional])),
    allowed
  )
  if (length(invalid) || anyNA(x$fractional_basis[fractional])) {
    stop(
      "Fractional geography specifications require a supported allocation basis.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

combine_canonical_geography_transitions <- function(transitions) {
  if (!is.list(transitions) || !length(transitions) ||
      is.null(names(transitions)) || any(!nzchar(names(transitions)))) {
    stop(
      "Multi-vintage geography requires a non-empty named transition list.",
      call. = FALSE
    )
  }
  rows <- lapply(names(transitions), function(transition_id) {
    x <- safe_df(transitions[[transition_id]])
    validate_geography_transition(x)
    out <- x[geography_transition_columns()]
    out$transition_id <- transition_id
    out
  })
  out <- safe_bind_rows(rows)
  if (anyDuplicated(out[c(
      "transition_id", "source_unit_id", "target_unit_id"
  )])) {
    stop(
      "Multi-vintage geography contains duplicate edges within a transition.",
      call. = FALSE
    )
  }
  out
}

collapse_multivintage_component_membership <- function(components) {
  x <- safe_df(components)
  required <- c(
    "harmonized_component_id", "vintage",
    "state_code", "district_code", "unit_id", "side"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Multi-vintage component membership lacks: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!nrow(x)) return(x)

  safe_bind_rows(lapply(
    split(
      seq_len(nrow(x)),
      paste(x$harmonized_component_id, x$vintage, x$unit_id, sep = "__")
    ),
    function(i) {
      part <- x[i, , drop = FALSE]
      states <- unique(plain_chr(part$state_code))
      districts <- unique(plain_chr(part$district_code))
      if (length(states) != 1L || length(districts) != 1L) {
        stop(
          "One canonical geography unit has inconsistent administrative codes.",
          call. = FALSE
        )
      }
      sides <- sort(unique(plain_chr(part$side)))
      data.frame(
        harmonized_component_id =
          part$harmonized_component_id[[1L]],
        vintage = as.integer(part$vintage[[1L]]),
        state_code = states[[1L]],
        district_code = districts[[1L]],
        unit_id = part$unit_id[[1L]],
        side = if (length(sides) == 1L) {
          sides[[1L]]
        } else {
          "bridge"
        },
        stringsAsFactors = FALSE
      )
    }
  ))
}

build_multivintage_geography_inventory <- function(
    transitions, required_vintages = NULL) {
  combined <- combine_canonical_geography_transitions(transitions)
  components <- build_geography_components(combined)
  components <- collapse_multivintage_component_membership(components)

  observed_vintages <- sort(unique(c(
    as.integer(combined$source_vintage),
    as.integer(combined$target_vintage)
  )))
  if (is.null(required_vintages)) {
    required_vintages <- observed_vintages
  }
  required_vintages <- sort(unique(as.integer(required_vintages)))
  if (anyNA(required_vintages)) {
    stop(
      "Required multi-vintage geography vintages must be finite integers.",
      call. = FALSE
    )
  }

  component_summary <- safe_bind_rows(lapply(
    split(components, components$harmonized_component_id),
    function(part) {
      vintages <- sort(unique(as.integer(part$vintage)))
      counts <- table(factor(
        part$vintage,
        levels = required_vintages
      ))
      data.frame(
        harmonized_component_id =
          part$harmonized_component_id[[1L]],
        vintages_present = paste(vintages, collapse = "|"),
        n_vintages_present = length(vintages),
        spans_all_required_vintages =
          all(required_vintages %in% vintages),
        n_units = nrow(part),
        stringsAsFactors = FALSE
      ) |>
        cbind(
          setNames(
            as.data.frame.list(as.integer(counts)),
            paste0("n_units_", required_vintages)
          )
        )
    }
  ))

  transition_summary <- safe_bind_rows(lapply(
    split(combined, combined$transition_id),
    function(part) {
      data.frame(
        transition_id = part$transition_id[[1L]],
        source_vintage = unique(as.integer(part$source_vintage))[[1L]],
        target_vintage = unique(as.integer(part$target_vintage))[[1L]],
        n_edges = nrow(part),
        n_source_units = length(unique(part$source_unit_id)),
        n_target_units = length(unique(part$target_unit_id)),
        n_complete_source_edges = sum(
          is.finite(num(part$source_coverage)) &
            num(part$source_coverage) >= 1 - 1e-12
        ),
        n_complete_target_edges = sum(
          is.finite(num(part$target_coverage)) &
            num(part$target_coverage) >= 1 - 1e-12
        ),
        stringsAsFactors = FALSE
      )
    }
  ))

  list(
    transitions = combined,
    components = components,
    component_summary = component_summary,
    transition_summary = transition_summary,
    required_vintages = data.frame(
      vintage = required_vintages,
      stringsAsFactors = FALSE
    )
  )
}

save_geography_harmonization_foundation <- function(
    multivintage,
    semantics = geography_allocation_semantics_registry(),
    measure_families = geography_measure_family_registry(),
    specifications = geography_specification_registry(),
    directory = "outputs/diagnostics/extended/geography") {
  validate_geography_allocation_semantics(semantics)
  validate_geography_measure_families(measure_families, semantics)
  validate_geography_specifications(specifications)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)

  paths <- c(
    allocation_semantics = file.path(
      directory, "allocation_semantics.csv"
    ),
    measure_families = file.path(
      directory, "measure_allocation_families.csv"
    ),
    geography_specifications = file.path(
      directory, "geography_specifications.csv"
    ),
    multivintage_transitions = file.path(
      directory, "multivintage_transitions.csv"
    ),
    multivintage_components = file.path(
      directory, "multivintage_components.csv"
    ),
    multivintage_component_summary = file.path(
      directory, "multivintage_component_summary.csv"
    ),
    multivintage_transition_summary = file.path(
      directory, "multivintage_transition_summary.csv"
    )
  )
  write_diagnostic_csv(semantics, paths[["allocation_semantics"]])
  write_diagnostic_csv(
    measure_families, paths[["measure_families"]]
  )
  write_diagnostic_csv(
    specifications, paths[["geography_specifications"]]
  )
  write_diagnostic_csv(
    multivintage$transitions, paths[["multivintage_transitions"]]
  )
  write_diagnostic_csv(
    multivintage$components, paths[["multivintage_components"]]
  )
  write_diagnostic_csv(
    multivintage$component_summary,
    paths[["multivintage_component_summary"]]
  )
  write_diagnostic_csv(
    multivintage$transition_summary,
    paths[["multivintage_transition_summary"]]
  )
  unname(paths)
}
