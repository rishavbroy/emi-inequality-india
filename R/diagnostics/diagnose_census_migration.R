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
    district_panel) {
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
      estimate_census_migration_first_stage_sensitivity(validity_panel)
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
      file.path(dir, "d02_2001_first_stage_sensitivity.csv")
  )
  for (name in names(files)) {
    utils::write.csv(diagnostics[[name]], files[[name]], row.names = FALSE, na = "")
  }
  unname(files)
}
