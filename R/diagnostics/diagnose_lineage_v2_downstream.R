# This file is part of the EMI inequality research pipeline.
# It builds a diagnostic-only panel from the reviewed lineage-v2 crosswalk.
# Public targets continue to use the inherited production panel until migration
# is explicitly reviewed and accepted.

lineage_v2_source_code <- function(x) {
  raw <- gsub("[^0-9]", "", plain_chr(x))
  raw[nchar(raw) == 0L] <- NA_character_
  suppressWarnings(as.character(as.integer(raw)))
}

lineage_v2_target_codes <- function(unit_id) {
  parts <- strsplit(plain_chr(unit_id), "__", fixed = TRUE)
  data.frame(
    state_code_2001 = vapply(
      parts, function(x) if (length(x) >= 3L) x[[2L]] else NA_character_,
      character(1)
    ),
    district_code_2001 = vapply(
      parts, function(x) if (length(x) >= 3L) x[[3L]] else NA_character_,
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}

lineage_v2_wave_measure_spec <- function(wave) {
  if (identical(wave, "nss_2007_08")) {
    return(list(
      code_col = "district_code_0708",
      count_cols = c("npeople_0708", "nhouses_0708", "n"),
      weight_col = "nhouses_0708"
    ))
  }
  if (identical(wave, "nss_2017_18")) {
    return(list(
      code_col = "district_code_1718",
      count_cols = c("npeople_1718", "nhouses_1718", "n"),
      weight_col = "nhouses_1718"
    ))
  }
  stop("Unsupported lineage-v2 wave: ", wave, call. = FALSE)
}

first_nonmissing_v2 <- function(x) {
  keep <- !is.na(x)
  if (is.character(x)) keep <- keep & nzchar(x)
  hit <- which(keep)
  if (length(hit)) x[[hit[[1L]]]] else x[[1L]][NA_integer_]
}

weighted_mean_v2 <- function(x, w) {
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) return(NA_real_)
  sum(x[keep] * w[keep]) / sum(w[keep])
}

collapse_lineage_v2_measure_rows <- function(mapped, spec) {
  mapped <- safe_df(mapped)
  if (!nrow(mapped)) return(data.frame())
  if (!"weight" %in% names(mapped)) mapped$weight <- 1
  if (!"panel_variant" %in% names(mapped)) {
    mapped$panel_variant <- "deterministic"
  }

  raw_households <- if (spec$weight_col %in% names(mapped)) {
    suppressWarnings(as.numeric(mapped[[spec$weight_col]]))
  } else {
    rep(1, nrow(mapped))
  }
  mapped$.aggregation_mass <- raw_households * mapped$weight
  for (nm in intersect(spec$count_cols, names(mapped))) {
    if (is.numeric(mapped[[nm]])) {
      mapped[[nm]] <- mapped[[nm]] * mapped$weight
    }
  }

  groups <- split(seq_len(nrow(mapped)), mapped$target_unit_2001)
  safe_bind_rows(lapply(groups, function(i) {
    rows <- mapped[i, , drop = FALSE]
    out <- rows[1L, c("target_unit_2001"), drop = FALSE]
    source_ids <- if ("source_row_id" %in% names(rows)) {
      unique(rows$source_row_id[
        !is.na(rows$source_row_id) & nzchar(rows$source_row_id)
      ])
    } else {
      character()
    }
    out$lineage_source_count <- if (length(source_ids)) {
      length(source_ids)
    } else {
      nrow(rows)
    }
    allocated <- any(
      rows$panel_variant %in% "population_allocation" |
        abs(rows$weight - 1) > 1e-8
    )
    out$lineage_aggregation_status <- if (
      nrow(rows) == 1L && !allocated
    ) {
      "one_to_one"
    } else if (allocated && out$lineage_source_count == 1L) {
      "source_split_population_allocated"
    } else if (allocated) {
      "district_aggregate_population_weighted"
    } else {
      "district_aggregate_weighted"
    }

    value_cols <- setdiff(
      names(rows),
      c(
        "target_unit_2001", "source_code", "source_row_id", "wave",
        "mapping_class", "basis", "source_id", "panel_variant",
        "weight", ".aggregation_mass", "lineage_source_count",
        "lineage_aggregation_status"
      )
    )
    for (nm in value_cols) {
      x <- rows[[nm]]
      if (nm %in% spec$count_cols && is.numeric(x)) {
        out[[nm]] <- sum(x, na.rm = TRUE)
      } else if (is.numeric(x)) {
        out[[nm]] <- weighted_mean_v2(x, rows$.aggregation_mass)
      } else {
        out[[nm]] <- first_nonmissing_v2(x)
      }
    }
    out
  }))
}

map_lineage_v2_measures <- function(measures, crosswalk, wave) {
  measures <- safe_df(measures)
  crosswalk <- safe_df(crosswalk)
  spec <- lineage_v2_wave_measure_spec(wave)
  if (!nrow(measures) || !nrow(crosswalk) || !spec$code_col %in% names(measures)) {
    return(data.frame())
  }

  map <- crosswalk[
    crosswalk$wave %in% wave,
    intersect(
      c(
        "source_row_id", "wave", "source_code", "target_unit_2001",
        "mapping_class", "weight", "basis", "source_id", "panel_variant"
      ),
      names(crosswalk)
    ),
    drop = FALSE
  ]
  if (!nrow(map)) return(data.frame())
  if (!"weight" %in% names(map)) map$weight <- 1
  if (!"panel_variant" %in% names(map)) {
    map$panel_variant <- "deterministic"
  }
  map$source_code_key <- lineage_v2_source_code(map$source_code)
  measures$source_code_key <- lineage_v2_source_code(measures[[spec$code_col]])

  source_weight <- aggregate(
    map$weight,
    list(source_row_id = map$source_row_id),
    sum
  )
  if (any(abs(source_weight$x - 1) > 1e-8)) {
    stop(
      "Lineage-v2 crosswalk weights must sum to one within source row.",
      call. = FALSE
    )
  }
  source_code_rows <- unique(map[c("source_row_id", "source_code_key")])
  if (anyDuplicated(
    source_code_rows$source_code_key[
      !is.na(source_code_rows$source_code_key)
    ]
  )) {
    stop(
      "Each wave-specific source code must identify one NSS source row.",
      call. = FALSE
    )
  }

  mapped <- merge(
    map,
    measures,
    by = "source_code_key",
    all.x = TRUE,
    sort = FALSE
  )
  mapped$source_code_key <- NULL
  collapse_lineage_v2_measure_rows(mapped, spec)
}

attach_lineage_v2_instrument <- function(panel, linguistic_distance_iv) {
  panel <- safe_df(panel)
  iv <- safe_df(linguistic_distance_iv)
  if (!nrow(panel) || !nrow(iv)) return(panel)

  codes <- lineage_v2_target_codes(panel$target_unit_2001)
  panel$state_code_2001 <- codes$state_code_2001
  panel$district_code_2001 <- codes$district_code_2001

  state_col <- first_col(iv, c("state_code", "state"))
  district_col <- first_col(iv, c("district_code", "district"))
  if (is.null(state_col) || is.null(district_col)) return(panel)

  iv$state_code_2001 <- sprintf("%02d", as.integer(num(iv[[state_col]])))
  iv$district_code_2001 <- sprintf("%02d", as.integer(num(iv[[district_col]])))
  duplicate <- intersect(
    setdiff(names(iv), c("state_code_2001", "district_code_2001")),
    names(panel)
  )
  iv <- iv[setdiff(names(iv), duplicate)]
  merge(
    panel,
    iv,
    by = c("state_code_2001", "district_code_2001"),
    all.x = TRUE,
    sort = FALSE
  )
}

attach_lineage_v2_geometry <- function(panel, geometry_2001) {
  panel <- safe_df(panel)
  if (!inherits(geometry_2001, "sf") || !nrow(geometry_2001) ||
      !"unit_id" %in% names(geometry_2001)) {
    return(panel)
  }
  geometry <- geometry_2001[
    !duplicated(plain_chr(geometry_2001$unit_id)),
    c("unit_id", attr(geometry_2001, "sf_column")),
    drop = FALSE
  ]
  names(geometry)[names(geometry) == "unit_id"] <- "target_unit_2001"
  merge(geometry, panel, by = "target_unit_2001", all.y = TRUE, sort = FALSE)
}

build_lineage_v2_district_panel <- function(
  primary_crosswalk, measures_2007, measures_2017,
  linguistic_distance_iv, geometry_2001 = data.frame(), cfg = list()
) {
  m07 <- map_lineage_v2_measures(
    measures_2007, primary_crosswalk, "nss_2007_08"
  )
  m17 <- map_lineage_v2_measures(
    measures_2017, primary_crosswalk, "nss_2017_18"
  )
  if (!nrow(m07) || !nrow(m17)) return(empty_panel())

  duplicate <- intersect(
    setdiff(names(m17), c("target_unit_2001")),
    names(m07)
  )
  m17 <- m17[setdiff(names(m17), duplicate)]
  panel <- merge(m07, m17, by = "target_unit_2001", all = FALSE, sort = FALSE)
  panel <- attach_lineage_v2_instrument(panel, linguistic_distance_iv)
  panel$district_panel_id <- sub(
    "^pc2001__", "2001__", panel$target_unit_2001
  )
  panel <- add_panel_standardized_names(panel)
  panel <- add_panel_regions(panel)
  panel <- compute_consumption_pct_change(panel)
  panel <- compute_log_consumption_difference(panel)
  panel <- compute_gini_change(panel)
  panel <- panel[panel_has_analysis_core(panel), , drop = FALSE]
  rownames(panel) <- NULL
  validate_analysis_district_panel(
    attach_lineage_v2_geometry(panel, geometry_2001),
    cfg,
    strict = FALSE
  )
}

lineage_panel_unit_id <- function(panel) {
  panel <- safe_df(panel)
  if ("target_unit_2001" %in% names(panel)) {
    return(plain_chr(panel$target_unit_2001))
  }
  if ("district_panel_id" %in% names(panel)) {
    return(sub("^2001__", "pc2001__", plain_chr(panel$district_panel_id)))
  }
  rep(NA_character_, nrow(panel))
}

compare_lineage_v2_panels <- function(production_panel, v2_panel) {
  production <- safe_df(production_panel)
  candidate <- safe_df(v2_panel)
  production$target_unit_2001 <- lineage_panel_unit_id(production)
  candidate$target_unit_2001 <- lineage_panel_unit_id(candidate)

  production_units <- unique(stats::na.omit(production$target_unit_2001))
  candidate_units <- unique(stats::na.omit(candidate$target_unit_2001))
  units <- sort(unique(c(production_units, candidate_units)))
  data.frame(
    target_unit_2001 = units,
    in_production = units %in% production_units,
    in_v2 = units %in% candidate_units,
    comparison_status = ifelse(
      units %in% production_units & units %in% candidate_units,
      "shared",
      ifelse(units %in% candidate_units, "v2_only", "production_only")
    ),
    stringsAsFactors = FALSE
  )
}


lineage_v2_panel_duplicates <- function(panel, variant) {
  panel <- safe_df(panel)
  unit_id <- lineage_panel_unit_id(panel)
  duplicate <- !is.na(unit_id) & (duplicated(unit_id) | duplicated(unit_id, fromLast = TRUE))
  if (!any(duplicate)) {
    return(data.frame(
      panel_variant = character(),
      target_unit_2001 = character(),
      row_count = integer(),
      stringsAsFactors = FALSE
    ))
  }
  counts <- table(unit_id[duplicate])
  data.frame(
    panel_variant = variant,
    target_unit_2001 = names(counts),
    row_count = as.integer(counts),
    stringsAsFactors = FALSE
  )
}

summarize_lineage_v2_downstream_coverage <- function(
  primary_crosswalk, primary_eligibility, production_panel, v2_panel
) {
  crosswalk <- safe_df(primary_crosswalk)
  eligibility <- safe_df(primary_eligibility)

  waves <- sort(unique(c(
    plain_chr(crosswalk$wave),
    plain_chr(eligibility$wave)
  )))
  wave_rows <- safe_bind_rows(lapply(waves, function(wave) {
    eligible <- eligibility[eligibility$wave %in% wave, , drop = FALSE]
    preferred <- crosswalk[crosswalk$wave %in% wave, , drop = FALSE]
    data.frame(
      scope = "wave",
      wave = wave,
      accepted_identities = nrow(eligible),
      preferred_mappings = nrow(preferred),
      preferred_targets = length(unique(stats::na.omit(
        preferred$target_unit_2001
      ))),
      excluded_identities = sum(
        eligible$eligible_primary %in% FALSE,
        na.rm = TRUE
      ),
      preferred_identity_share = if (nrow(eligible)) {
        nrow(preferred) / nrow(eligible)
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
  }))

  production_units <- unique(stats::na.omit(
    lineage_panel_unit_id(production_panel)
  ))
  v2_units <- unique(stats::na.omit(lineage_panel_unit_id(v2_panel)))
  overlap <- if (all(c("nss_2007_08", "nss_2017_18") %in% waves)) {
    map_07 <- crosswalk$target_unit_2001[
      crosswalk$wave %in% "nss_2007_08"
    ]
    map_17 <- crosswalk$target_unit_2001[
      crosswalk$wave %in% "nss_2017_18"
    ]
    length(intersect(unique(map_07), unique(map_17)))
  } else {
    0L
  }

  overall <- data.frame(
    scope = "panel",
    wave = "two_wave_overlap",
    accepted_identities = NA_integer_,
    preferred_mappings = NA_integer_,
    preferred_targets = overlap,
    excluded_identities = NA_integer_,
    preferred_identity_share = if (length(production_units)) {
      length(v2_units) / length(production_units)
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )
  safe_bind_rows(list(wave_rows, overall))
}

lineage_v2_downstream_review_gates <- function(
  coverage, production_panel, v2_panel
) {
  coverage <- safe_df(coverage)
  production_duplicates <- lineage_v2_panel_duplicates(
    production_panel, "production"
  )
  v2_duplicates <- lineage_v2_panel_duplicates(v2_panel, "lineage_v2")
  panel_row <- coverage[
    coverage$scope %in% "panel" &
      coverage$wave %in% "two_wave_overlap",
    ,
    drop = FALSE
  ]
  coverage_equal <- nrow(panel_row) &&
    isTRUE(all.equal(panel_row$preferred_identity_share[[1L]], 1))

  data.frame(
    gate = c(
      "production_panel_unique_by_2001_unit",
      "lineage_v2_panel_unique_by_2001_unit",
      "lineage_v2_panel_matches_production_coverage",
      "downstream_model_comparison_interpretable",
      "production_migration_reviewable"
    ),
    passed = c(
      nrow(production_duplicates) == 0L,
      nrow(v2_duplicates) == 0L,
      coverage_equal,
      coverage_equal &&
        nrow(production_duplicates) == 0L &&
        nrow(v2_duplicates) == 0L,
      coverage_equal &&
        nrow(production_duplicates) == 0L &&
        nrow(v2_duplicates) == 0L
    ),
    next_action = c(
      "Resolve duplicated Census-2001 units in the inherited production panel.",
      "Resolve duplicated Census-2001 units in the lineage-v2 panel.",
      paste0(
        "Complete reviewed 2017-18-to-2001 transitions before comparing ",
        "full-panel estimates."
      ),
      paste0(
        "Do not interpret coefficient changes until both panels represent ",
        "the same unique district support."
      ),
      paste0(
        "Keep downstream_results as needs_review until coverage and duplicate ",
        "gates pass."
      )
    ),
    stringsAsFactors = FALSE
  )
}

lineage_v2_model_summary <- function(iv_models, first_stage_tests, panel, variant) {
  coefficients <- tidy_iv_models(iv_models)
  if (nrow(coefficients)) coefficients$panel_variant <- variant

  first_stage <- safe_df(first_stage_tests)
  if (nrow(first_stage)) first_stage$panel_variant <- variant

  panel_df <- safe_df(panel)
  panel_summary <- data.frame(
    panel_variant = variant,
    panel_rows = nrow(panel_df),
    unique_districts = length(unique(stats::na.omit(
      lineage_panel_unit_id(panel_df)
    ))),
    complete_iv_rows = if (nrow(panel_df)) {
      sum(panel_has_analysis_core(panel_df), na.rm = TRUE)
    } else {
      0L
    },
    multi_source_rows = if (
      "lineage_source_count" %in% names(panel_df)
    ) {
      sum(num(panel_df$lineage_source_count) > 1, na.rm = TRUE)
    } else {
      0L
    },
    stringsAsFactors = FALSE
  )
  list(
    coefficients = coefficients,
    first_stage = first_stage,
    panel_summary = panel_summary
  )
}

compare_lineage_v2_model_summaries <- function(production, candidate) {
  prod_coef <- safe_df(production$coefficients)
  cand_coef <- safe_df(candidate$coefficients)
  keys <- intersect(c("model", "term"), intersect(names(prod_coef), names(cand_coef)))
  coefficient_comparison <- if (length(keys) == 2L) {
    merge(
      prod_coef[c(keys, "estimate", "std.error", "p.value")],
      cand_coef[c(keys, "estimate", "std.error", "p.value")],
      by = keys,
      all = TRUE,
      suffixes = c("_production", "_v2"),
      sort = FALSE
    )
  } else {
    data.frame()
  }
  if (nrow(coefficient_comparison)) {
    coefficient_comparison$estimate_change <-
      coefficient_comparison$estimate_v2 -
      coefficient_comparison$estimate_production
    coefficient_comparison$std_error_change <-
      coefficient_comparison$std.error_v2 -
      coefficient_comparison$std.error_production
    coefficient_comparison$comparison_scope <-
      "different_panel_composition"
    coefficient_comparison$comparable <- FALSE
  }

  prod_fs <- safe_df(production$first_stage)
  cand_fs <- safe_df(candidate$first_stage)
  fs_keys <- intersect(c("model", "term"), intersect(names(prod_fs), names(cand_fs)))
  first_stage_comparison <- if (length(fs_keys) == 2L) {
    merge(
      prod_fs[c(
        fs_keys, "estimate", "std.error", "partial_f", "partial_p", "nobs"
      )],
      cand_fs[c(
        fs_keys, "estimate", "std.error", "partial_f", "partial_p", "nobs"
      )],
      by = fs_keys,
      all = TRUE,
      suffixes = c("_production", "_v2"),
      sort = FALSE
    )
  } else {
    data.frame()
  }

  if (nrow(first_stage_comparison)) {
    first_stage_comparison$comparison_scope <-
      "different_panel_composition"
    first_stage_comparison$comparable <- FALSE
  }

  list(
    panel_summary = safe_bind_rows(list(
      production$panel_summary,
      candidate$panel_summary
    )),
    coefficient_comparison = coefficient_comparison,
    first_stage_comparison = first_stage_comparison
  )
}

build_lineage_v2_downstream_review <- function(
  production_panel, v2_panel, production_models, v2_models,
  production_first_stage, v2_first_stage,
  primary_crosswalk = data.frame(),
  primary_eligibility = data.frame()
) {
  production <- lineage_v2_model_summary(
    production_models, production_first_stage, production_panel, "production"
  )
  candidate <- lineage_v2_model_summary(
    v2_models, v2_first_stage, v2_panel, "lineage_v2"
  )
  comparison <- compare_lineage_v2_model_summaries(production, candidate)
  comparison$panel_membership <- compare_lineage_v2_panels(
    production_panel, v2_panel
  )
  comparison$crosswalk_coverage <-
    summarize_lineage_v2_downstream_coverage(
      primary_crosswalk,
      primary_eligibility,
      production_panel,
      v2_panel
    )
  comparison$panel_duplicates <- safe_bind_rows(list(
    lineage_v2_panel_duplicates(production_panel, "production"),
    lineage_v2_panel_duplicates(v2_panel, "lineage_v2")
  ))
  comparison$review_gates <- lineage_v2_downstream_review_gates(
    comparison$crosswalk_coverage,
    production_panel,
    v2_panel
  )
  comparison
}

save_lineage_v2_downstream_review <- function(
  review, dir = "outputs/diagnostics/extended/district_lineage_v2"
) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  output_manifest(c(
    downstream_panel_summary = write_diagnostic_csv(
      review$panel_summary %||% data.frame(),
      file.path(dir, "downstream_panel_summary.csv")
    ),
    downstream_panel_membership = write_diagnostic_csv(
      review$panel_membership %||% data.frame(),
      file.path(dir, "downstream_panel_membership.csv")
    ),
    downstream_coefficient_comparison = write_diagnostic_csv(
      review$coefficient_comparison %||% data.frame(),
      file.path(dir, "downstream_coefficient_comparison.csv")
    ),
    downstream_first_stage_comparison = write_diagnostic_csv(
      review$first_stage_comparison %||% data.frame(),
      file.path(dir, "downstream_first_stage_comparison.csv")
    ),
    downstream_crosswalk_coverage = write_diagnostic_csv(
      review$crosswalk_coverage %||% data.frame(),
      file.path(dir, "downstream_crosswalk_coverage.csv")
    ),
    downstream_panel_duplicates = write_diagnostic_csv(
      review$panel_duplicates %||% data.frame(),
      file.path(dir, "downstream_panel_duplicates.csv")
    ),
    downstream_review_gates = write_diagnostic_csv(
      review$review_gates %||% data.frame(),
      file.path(dir, "downstream_review_gates.csv")
    )
  ))
}
