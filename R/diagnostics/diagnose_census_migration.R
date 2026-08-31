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
  census_mechanism_specifications(
    outcome = outcome,
    treatment = treatment,
    sample_rule = "migration_mechanism_common_support"
  )
}

census_migration_mechanism_sources <- function(d02_2011, d03_2011, d04_2011, d07_2011) {
  list(
    d02 = safe_df(d02_2011),
    d03 = safe_df(d03_2011),
    d04 = safe_df(d04_2011),
    d07 = safe_df(d07_2011)
  )
}

census_migration_mechanism_design_variables <- function(
    specifications = census_migration_mechanism_specifications()) {
  census_mechanism_design_variables(specifications)
}

prepare_census_migration_mechanism_panel <- function(
    district_panel, d02_2011, d03_2011, d04_2011, d07_2011,
    registry = census_migration_mechanism_registry()) {
  specifications <- census_migration_mechanism_specifications()
  prepare_census_mechanism_panel(
    district_panel = district_panel,
    sources = census_migration_mechanism_sources(
      d02_2011, d03_2011, d04_2011, d07_2011
    ),
    registry = registry,
    specifications = specifications,
    label = "Census migration"
  )
}

add_census_migration_holm <- function(results, p_column, output_column) {
  add_census_mechanism_holm(
    results, p_column, output_column, label = "Census migration"
  )
}

estimate_census_migration_mechanism_models <- function(
    mechanism_panel,
    registry = census_migration_mechanism_registry(),
    cfg = list(),
    ar_points = 401L) {
  estimate_census_mechanism_models(
    mechanism_panel = mechanism_panel,
    registry = registry,
    specifications = census_migration_mechanism_specifications(),
    cfg = cfg,
    ar_points = ar_points,
    label = "Census migration"
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
    mechanism_sample_support = mechanism$sample_support,
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
    mechanism_sample_support = file.path(dir, "mechanism_sample_support.csv"),
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
