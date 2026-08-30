# Extended diagnostics for Census migration source measures and IV validity.

census_migration_balance_variables <- function() {
  c(
    "migrant_stock_share_population",
    "recent_0_9_migrant_share_population",
    "interstate_migrant_share_population",
    "other_district_same_state_share_among_migrants"
  )
}

census_migration_first_stage_controls <- function() {
  c(
    "migrant_stock_share_population",
    "recent_0_9_migrant_share_population",
    "interstate_migrant_share_population"
  )
}

prepare_census_migration_validity_panel <- function(panel, d02_2001) {
  prepare_census_2001_balance_panel(
    panel,
    d02_2001,
    census_migration_balance_variables(),
    "Census migration validity"
  )
}

census_migration_first_stage_specifications <- function(
    outcome = "real_log_consumption_change",
    treatment = preferred_iv_variables()$treatment) {
  registry <- iv_specification_registry(
    outcome = outcome, treatment = treatment,
    panel_variant = "primary", sample_rule = "alternative_distance_common_support"
  )
  keep <- registry$adjustment_id %in% iv_candidate_design_adjustments() &
    registry$construction_id == "nonzero_mean"
  base <- registry[keep, , drop = FALSE]
  expected <- paste(iv_candidate_design_adjustments(), "nonzero_mean", sep = "__")
  if (!setequal(base$specification_id, expected) || anyDuplicated(base$specification_id)) {
    stop("Census migration first-stage sensitivity could not recover the primary candidate designs.", call. = FALSE)
  }
  base$migration_adjustment <- "baseline"

  augmented <- base
  augmented$specification_id <- paste0(augmented$specification_id, "__plus_migration")
  augmented$controls <- I(lapply(augmented$controls, function(controls) {
    order_iv_controls(unique(c(
      unlist(controls, use.names = FALSE),
      census_migration_first_stage_controls()
    )))
  }))
  augmented$migration_adjustment <- "plus_migration"
  out <- bind_iv_specification_rows(list(base, augmented))
  out$sequence <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}

estimate_census_migration_first_stage_sensitivity <- function(panel) {
  x <- safe_df(panel)
  specifications <- census_migration_first_stage_specifications()
  required <- unique(c(
    "state_code_2001", "region",
    census_migration_first_stage_controls(),
    unlist(specifications$controls, use.names = FALSE),
    unlist(specifications$included_language_controls, use.names = FALSE),
    unlist(specifications$excluded_instruments, use.names = FALSE),
    specifications$treatment
  ))
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Census migration first-stage sensitivity is missing columns: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  common <- x[stats::complete.cases(x[required]), , drop = FALSE]
  if (!nrow(common)) {
    stop("Census migration first-stage sensitivity has no complete common sample.", call. = FALSE)
  }
  rows <- safe_bind_rows(lapply(seq_len(nrow(specifications)), function(i) {
    specification <- specifications[i, , drop = FALSE]
    estimate <- estimate_alternative_distance_spec(
      common, specification, treatment = specification$treatment[[1L]]
    )$summary
    estimate$migration_adjustment <- specification$migration_adjustment[[1L]]
    estimate$n_migration_controls <- if (
      identical(specification$migration_adjustment[[1L]], "plus_migration")
    ) length(census_migration_first_stage_controls()) else 0L
    estimate
  }))

  baseline <- rows[
    rows$migration_adjustment == "baseline",
    c("adjustment_id", "joint_excluded_f", "partial_r_squared"),
    drop = FALSE
  ]
  names(baseline)[2:3] <- c("baseline_joint_excluded_f", "baseline_partial_r_squared")
  out <- merge(rows, baseline, by = "adjustment_id", all.x = TRUE, sort = FALSE)
  out$joint_excluded_f_change <- out$joint_excluded_f - out$baseline_joint_excluded_f
  out$partial_r_squared_change <- out$partial_r_squared - out$baseline_partial_r_squared
  out <- out[match(rows$specification_id, out$specification_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

census_migration_mechanism_registry <- function() {
  data.frame(
    outcome_id = c(
      "interstate_migrant_composition",
      "work_migration_reason",
      "education_migration_reason",
      "skilled_migrant_composition",
      "technical_migrant_composition",
      "outside_state_recent_work_migration",
      "skilled_recent_work_migration",
      "technical_recent_work_migration"
    ),
    source_id = c("d02", "d03", "d03", "d04", "d04", "d07", "d07", "d07"),
    variable = c(
      "interstate_share_among_migrants",
      "work_employment_share_among_migrants",
      "education_share_among_migrants",
      "graduate_or_technical_degree_share_among_migrants",
      "technical_credential_share_among_migrants",
      "outside_state_share_among_recent_work_migrants",
      "graduate_or_technical_degree_share_among_recent_work_migrants",
      "technical_credential_share_among_recent_work_migrants"
    ),
    mechanism_family = c(
      "geographic_sorting", "migration_reason", "migration_reason",
      "migrant_skill", "migrant_skill", "work_migrant_sorting",
      "work_migrant_skill", "work_migrant_skill"
    ),
    tier = c("core", "core", "core", "core", "secondary", "core", "core", "secondary"),
    denominator = c(
      "all_migrants", "all_migrants", "all_migrants", "all_migrants",
      "all_migrants", "recent_work_migrants", "recent_work_migrants",
      "recent_work_migrants"
    ),
    stringsAsFactors = FALSE
  )
}

census_migration_mechanism_specifications <- function(
    outcome = "interstate_share_among_migrants",
    treatment = preferred_iv_variables()$treatment) {
  registry <- iv_specification_registry(
    outcome = outcome,
    treatment = treatment,
    panel_variant = "primary",
    sample_rule = "migration_mechanism_common_support"
  )
  construction_ids <- unname(alternative_distance_design_constructions())
  keep <- registry$adjustment_id %in% iv_candidate_design_adjustments() &
    registry$construction_id %in% construction_ids
  out <- registry[keep, , drop = FALSE]
  expected <- as.vector(outer(
    iv_candidate_design_adjustments(), construction_ids, paste, sep = "__"
  ))
  if (!setequal(out$specification_id, expected) || anyDuplicated(out$specification_id)) {
    stop("Could not recover the candidate scalar-IV designs for Census migration mechanisms.", call. = FALSE)
  }
  out
}

census_migration_mechanism_sources <- function(d02_2011, d03_2011, d04_2011, d07_2011) {
  list(
    d02 = safe_df(d02_2011),
    d03 = safe_df(d03_2011),
    d04 = safe_df(d04_2011),
    d07 = safe_df(d07_2011)
  )
}

prepare_census_migration_mechanism_panel <- function(
    district_panel, d02_2011, d03_2011, d04_2011, d07_2011,
    registry = census_migration_mechanism_registry()) {
  registry <- safe_df(registry)
  required_registry <- c("outcome_id", "source_id", "variable")
  if (length(setdiff(required_registry, names(registry))) ||
      anyDuplicated(registry$outcome_id) || anyDuplicated(registry$variable)) {
    stop("Census migration mechanism registry is malformed.", call. = FALSE)
  }

  sources <- census_migration_mechanism_sources(d02_2011, d03_2011, d04_2011, d07_2011)
  if (!all(registry$source_id %in% names(sources))) {
    stop("Census migration mechanism registry references an unknown source.", call. = FALSE)
  }
  target_sets <- lapply(sources, function(x) {
    if (!"target_unit_2001" %in% names(x) || anyDuplicated(x$target_unit_2001)) {
      stop("Harmonized Census migration mechanism sources must be unique by target_unit_2001.", call. = FALSE)
    }
    sort(unique(plain_chr(x$target_unit_2001)))
  })
  reference_targets <- target_sets[[1L]]
  if (!all(vapply(target_sets, identical, logical(1), reference_targets))) {
    stop("Harmonized Census migration mechanism sources have different district support.", call. = FALSE)
  }

  measures <- data.frame(target_unit_2001 = reference_targets, stringsAsFactors = FALSE)
  for (source_id in unique(registry$source_id)) {
    rows <- registry[registry$source_id == source_id, , drop = FALSE]
    source <- sources[[source_id]]
    missing <- setdiff(rows$variable, names(source))
    if (length(missing)) {
      stop(
        "Census migration mechanism source `", source_id, "` is missing variables: ",
        paste(missing, collapse = ", "), call. = FALSE
      )
    }
    payload <- source[c("target_unit_2001", rows$variable)]
    measures <- merge(measures, payload, by = "target_unit_2001", all = FALSE, sort = FALSE)
  }
  measures <- measures[match(reference_targets, measures$target_unit_2001), , drop = FALSE]
  codes <- lineage_target_codes(measures$target_unit_2001)
  measures$state_code_2001 <- normalize_census_code(codes$state_code_2001, 2L)
  measures$district_code_2001 <- normalize_census_code(codes$district_code_2001, 2L)

  panel <- if (inherits(district_panel, "sf")) sf::st_drop_geometry(district_panel) else safe_df(district_panel)
  keys <- c("state_code_2001", "district_code_2001")
  panel$state_code_2001 <- normalize_census_code(panel$state_code_2001, 2L)
  panel$district_code_2001 <- normalize_census_code(panel$district_code_2001, 2L)
  if (anyDuplicated(panel[keys])) {
    stop("District panel is not unique by Census-2001 district for migration mechanisms.", call. = FALSE)
  }
  panel <- merge(
    panel,
    measures[c(keys, registry$variable)],
    by = keys,
    all = FALSE,
    sort = FALSE
  )
  if (nrow(panel) != nrow(measures)) {
    stop("Census migration mechanism districts are not all present in the IV panel.", call. = FALSE)
  }

  projected <- prepare_alternative_distance_panel(
    panel,
    treatment = preferred_iv_variables()$treatment,
    retain = registry$variable
  )
  complete <- stats::complete.cases(projected[registry$variable])
  out <- projected[complete, , drop = FALSE]
  if (!nrow(out)) {
    stop("Census migration mechanisms have no common complete outcome sample.", call. = FALSE)
  }
  attr(out, "n_harmonized_mechanism_districts") <- nrow(measures)
  rownames(out) <- NULL
  out
}

add_census_migration_holm <- function(results, p_column, output_column) {
  out <- safe_df(results)
  if (!all(c("specification_id", "status", p_column) %in% names(out))) {
    stop("Migration Holm adjustment lacks required result columns.", call. = FALSE)
  }
  out[[output_column]] <- NA_real_
  groups <- split(seq_len(nrow(out)), out$specification_id)
  for (index in groups) {
    usable <- index[out$status[index] == "estimated" & is.finite(num(out[[p_column]][index]))]
    if (length(usable)) {
      out[[output_column]][usable] <- stats::p.adjust(num(out[[p_column]][usable]), method = "holm")
    }
  }
  out
}

estimate_census_migration_mechanism_models <- function(
    mechanism_panel,
    registry = census_migration_mechanism_registry(),
    cfg = list(),
    ar_points = 401L) {
  panel <- safe_df(mechanism_panel)
  registry <- safe_df(registry)
  base_specs <- census_migration_mechanism_specifications()
  sample_n <- nrow(panel)
  harmonized_n <- attr(mechanism_panel, "n_harmonized_mechanism_districts", exact = TRUE)
  if (!is.finite(harmonized_n)) harmonized_n <- sample_n

  first_stage <- safe_bind_rows(lapply(seq_len(nrow(base_specs)), function(i) {
    spec <- base_specs[i, , drop = FALSE]
    estimate_alternative_distance_spec(
      panel, spec, treatment = spec$treatment[[1L]]
    )$summary
  }))
  if (any(num(first_stage$n) != sample_n)) {
    stop("Census migration mechanism first stages did not use the registered common sample.", call. = FALSE)
  }

  reduced_form <- safe_bind_rows(lapply(seq_len(nrow(registry)), function(j) {
    outcome <- registry[j, , drop = FALSE]
    safe_bind_rows(lapply(seq_len(nrow(base_specs)), function(i) {
      spec <- base_specs[i, , drop = FALSE]
      spec$outcome <- outcome$variable[[1L]]
      estimate <- estimate_iv_reduced_form_spec(panel, spec, cfg)
      estimate$outcome_id <- outcome$outcome_id[[1L]]
      estimate$outcome_variable <- outcome$variable[[1L]]
      estimate$mechanism_family <- outcome$mechanism_family[[1L]]
      estimate$tier <- outcome$tier[[1L]]
      estimate$denominator <- outcome$denominator[[1L]]
      estimate$adjustment_id <- spec$adjustment_id[[1L]]
      estimate$construction_id <- spec$construction_id[[1L]]
      estimate$fixed_effect <- spec$fixed_effect[[1L]]
      estimate
    }))
  }))
  if (any(num(reduced_form$n) != sample_n)) {
    stop("Census migration reduced forms did not use one common mechanism sample.", call. = FALSE)
  }
  reduced_form <- add_census_migration_holm(
    reduced_form, "p.value", "p_holm_within_spec"
  )
  reduced_form <- reduced_form[c(
    "outcome_id", "outcome_variable", "mechanism_family", "tier", "denominator",
    "specification_id", "adjustment_id", "construction_id", "fixed_effect",
    "term", "estimate", "std.error", "statistic", "p.value",
    "p_holm_within_spec", "n", "status", "reason"
  )]

  weak_estimates <- list()
  weak_grids <- list()
  k <- 0L
  for (j in seq_len(nrow(registry))) {
    outcome <- registry[j, , drop = FALSE]
    for (i in seq_len(nrow(base_specs))) {
      spec <- base_specs[i, , drop = FALSE]
      spec$outcome <- outcome$variable[[1L]]
      result <- estimate_weak_iv_specification(
        panel, spec, cfg = cfg, ar_points = ar_points
      )
      if (is.null(result)) next
      k <- k + 1L
      summary <- result$summary
      summary$outcome_id <- outcome$outcome_id[[1L]]
      summary$outcome_variable <- outcome$variable[[1L]]
      summary$mechanism_family <- outcome$mechanism_family[[1L]]
      summary$tier <- outcome$tier[[1L]]
      summary$denominator <- outcome$denominator[[1L]]
      summary$fixed_effect <- spec$fixed_effect[[1L]]
      weak_estimates[[k]] <- summary

      grid <- result$grid
      if (nrow(grid)) {
        grid$outcome_id <- outcome$outcome_id[[1L]]
        grid$outcome_variable <- outcome$variable[[1L]]
        grid$adjustment_id <- spec$adjustment_id[[1L]]
        grid$construction_id <- spec$construction_id[[1L]]
        grid$fixed_effect <- spec$fixed_effect[[1L]]
        weak_grids[[k]] <- grid
      }
    }
  }
  weak_iv <- safe_bind_rows(weak_estimates)
  if (nrow(weak_iv) != nrow(registry) * nrow(base_specs) ||
      any(num(weak_iv$n) != sample_n)) {
    stop("Census migration weak-IV models did not use one complete registered model grid.", call. = FALSE)
  }
  weak_iv <- add_census_migration_holm(
    weak_iv, "p_value_clustered", "p_value_clustered_holm_within_spec"
  )
  weak_iv <- add_census_migration_holm(
    weak_iv, "anderson_rubin_p_beta0", "anderson_rubin_p_beta0_holm_within_spec"
  )
  weak_iv <- weak_iv[c(
    "outcome_id", "outcome_variable", "mechanism_family", "tier", "denominator",
    "specification_id", "adjustment_id", "construction_id", "fixed_effect",
    "estimate_2sls", "std_error_clustered", "p_value_clustered",
    "p_value_clustered_holm_within_spec",
    "effective_f", "effective_f_critical_value", "effective_f_p_value", "effective_f_df",
    "reduced_form_joint_f", "reduced_form_joint_p",
    "anderson_rubin_f_beta0", "anderson_rubin_p_beta0",
    "anderson_rubin_p_beta0_holm_within_spec",
    "ar_95_lower", "ar_95_upper", "ar_95_empty", "ar_95_n_components",
    "ar_95_disconnected", "ar_95_contains_zero", "ar_95_grid_accepted_min",
    "ar_95_grid_accepted_max", "ar_95_left_truncated", "ar_95_right_truncated",
    "ar_95_components", "n", "status", "reason"
  )]
  ar_grid <- safe_bind_rows(weak_grids)

  list(
    registry = registry,
    sample_coverage = data.frame(
      n_harmonized_mechanism_districts = as.integer(harmonized_n),
      n_common_analysis_districts = sample_n,
      n_states = length(unique(panel$state_code_2001)),
      n_regions = length(unique(panel$region)),
      stringsAsFactors = FALSE
    ),
    first_stage = first_stage,
    reduced_form = reduced_form,
    weak_iv = weak_iv,
    anderson_rubin_grid = ar_grid
  )
}


build_census_migration_diagnostics <- function(
    d02_2001,
    d02_2011_source,
    d03_2011_source,
    d04_2011_source,
    d07_2011_source,
    d02_2011,
    d03_2011,
    d04_2011,
    d07_2011,
    district_panel,
    cfg = list()) {
  validity_panel <- prepare_census_migration_validity_panel(district_panel, d02_2001)
  validity_specs <- candidate_iv_balance_specifications()
  balance <- add_iv_balance_holm(
    run_iv_balance_diagnostics(
      validity_panel,
      specifications = validity_specs,
      variables = census_migration_balance_variables()
    )
  )
  joint_balance <- run_iv_joint_balance_diagnostics(
    validity_panel,
    specifications = validity_specs,
    variables = census_migration_balance_variables()
  )
  mechanism_registry <- census_migration_mechanism_registry()
  mechanism_panel <- prepare_census_migration_mechanism_panel(
    district_panel, d02_2011, d03_2011, d04_2011, d07_2011,
    registry = mechanism_registry
  )
  mechanism <- estimate_census_migration_mechanism_models(
    mechanism_panel, mechanism_registry, cfg = cfg
  )
  list(
    d02_2001 = safe_df(d02_2001),
    d02_2011_harmonized = safe_df(d02_2011),
    d03_2011_harmonized = safe_df(d03_2011),
    d04_2011_harmonized = safe_df(d04_2011),
    d07_2011_harmonized = safe_df(d07_2011),
    coverage = summarise_census_migration_coverage(list(
      d02_2001 = d02_2001,
      d02_2011_harmonized = d02_2011,
      d03_2011_harmonized = d03_2011,
      d04_2011_harmonized = d04_2011,
      d07_2011_harmonized = d07_2011
    )),
    d02_d03_2011_total_validation = validate_census_2011_migration_totals(
      d02_2011_source, d03_2011_source
    ),
    d02_d04_2011_total_validation = validate_census_2011_d02_d04_totals(
      d02_2011_source, d04_2011_source
    ),
    d03_d07_2011_recent_work_validation = validate_census_2011_d03_d07_recent_work(
      d03_2011_source, d07_2011_source
    ),
    d02_2001_balance = balance,
    d02_2001_joint_balance = joint_balance,
    d02_2001_first_stage_sensitivity =
      estimate_census_migration_first_stage_sensitivity(validity_panel),
    mechanism_registry = mechanism$registry,
    mechanism_sample_coverage = mechanism$sample_coverage,
    mechanism_first_stage = mechanism$first_stage,
    mechanism_reduced_form = mechanism$reduced_form,
    mechanism_weak_iv = mechanism$weak_iv,
    mechanism_anderson_rubin_grid = mechanism$anderson_rubin_grid
  )
}

save_census_migration_diagnostics <- function(
    diagnostics, dir = "outputs/diagnostics/extended/census_migration") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  files <- c(
    d02_2001 = file.path(dir, "d02_2001.csv"),
    d02_2011_harmonized = file.path(dir, "d02_2011_harmonized_2001.csv"),
    d03_2011_harmonized = file.path(dir, "d03_2011_harmonized_2001.csv"),
    d04_2011_harmonized = file.path(dir, "d04_2011_harmonized_2001.csv"),
    d07_2011_harmonized = file.path(dir, "d07_2011_harmonized_2001.csv"),
    coverage = file.path(dir, "coverage.csv"),
    d02_d03_2011_total_validation = file.path(dir, "d02_d03_2011_total_validation.csv"),
    d02_d04_2011_total_validation = file.path(dir, "d02_d04_2011_total_validation.csv"),
    d03_d07_2011_recent_work_validation =
      file.path(dir, "d03_d07_2011_recent_work_validation.csv"),
    d02_2001_balance = file.path(dir, "d02_2001_instrument_balance.csv"),
    d02_2001_joint_balance = file.path(dir, "d02_2001_instrument_balance_joint.csv"),
    d02_2001_first_stage_sensitivity =
      file.path(dir, "d02_2001_first_stage_sensitivity.csv"),
    mechanism_registry = file.path(dir, "mechanism_registry.csv"),
    mechanism_sample_coverage = file.path(dir, "mechanism_sample_coverage.csv"),
    mechanism_first_stage = file.path(dir, "mechanism_first_stage.csv"),
    mechanism_reduced_form = file.path(dir, "mechanism_reduced_form.csv"),
    mechanism_weak_iv = file.path(dir, "mechanism_weak_iv.csv"),
    mechanism_anderson_rubin_grid = file.path(dir, "mechanism_anderson_rubin_grid.csv")
  )
  for (name in names(files)) {
    utils::write.csv(diagnostics[[name]], files[[name]], row.names = FALSE, na = "")
  }
  unname(files)
}
