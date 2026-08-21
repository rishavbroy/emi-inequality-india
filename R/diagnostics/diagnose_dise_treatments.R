# DISE treatment validation and IV-permutation diagnostics.

add_dise_construct_id <- function(data, construct) {
  data <- safe_df(data)
  if (!nrow(data)) return(data)
  data$construct_id <- construct$construct_id[[1]]
  data$treatment <- construct$variable[[1]]
  data$analysis_scope <- construct$analysis_scope[[1]]
  data
}

dise_nss_validation_registry <- function() {
  data.frame(
    dise_variable = c(
      "dise_emi_enrollment_share_total_0708",
      "dise_emi_enrollment_share_total_0708"
    ),
    nss_variable = c(
      "emi_share_enrolled_0708",
      "emi_exposure_all_children_0708"
    ),
    comparison = c(
      "enrolled_total_denominator",
      "all_child_context"
    ),
    stringsAsFactors = FALSE
  )
}

diagnose_dise_nss_validation <- function(panel, registry = dise_nss_validation_registry()) {
  data <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else safe_df(panel)
  rows <- lapply(seq_len(nrow(registry)), function(i) {
    spec <- registry[i, , drop = FALSE]
    dise <- spec$dise_variable[[1]]
    nss <- spec$nss_variable[[1]]
    required <- c(dise, nss)
    if (!all(required %in% names(data))) {
      return(data.frame(
        spec, n = 0L, pearson = NA_real_, spearman = NA_real_,
        mean_dise = NA_real_, mean_nss = NA_real_, mean_difference = NA_real_,
        rmse = NA_real_, state_residual_pearson = NA_real_,
        status = "not_available", stringsAsFactors = FALSE
      ))
    }

    needed <- c(required, intersect("state_code_2001", names(data)))
    x <- data[stats::complete.cases(data[needed]), needed, drop = FALSE]
    if (nrow(x) < 3L) {
      return(data.frame(
        spec, n = nrow(x), pearson = NA_real_, spearman = NA_real_,
        mean_dise = NA_real_, mean_nss = NA_real_, mean_difference = NA_real_,
        rmse = NA_real_, state_residual_pearson = NA_real_,
        status = "insufficient_sample", stringsAsFactors = FALSE
      ))
    }

    x$.dise <- num(x[[dise]])
    x$.nss <- num(x[[nss]])
    state_residual <- NA_real_
    if ("state_code_2001" %in% names(x) && length(unique(x$state_code_2001)) > 1L) {
      dise_resid <- stats::residuals(stats::lm(.dise ~ factor(state_code_2001), data = x))
      nss_resid <- stats::residuals(stats::lm(.nss ~ factor(state_code_2001), data = x))
      state_residual <- suppressWarnings(stats::cor(dise_resid, nss_resid))
    }

    data.frame(
      spec,
      n = nrow(x),
      pearson = suppressWarnings(stats::cor(x$.dise, x$.nss)),
      spearman = suppressWarnings(stats::cor(x$.dise, x$.nss, method = "spearman")),
      mean_dise = mean(x$.dise),
      mean_nss = mean(x$.nss),
      mean_difference = mean(x$.dise - x$.nss),
      rmse = sqrt(mean((x$.dise - x$.nss)^2)),
      state_residual_pearson = state_residual,
      status = "estimated",
      stringsAsFactors = FALSE
    )
  })
  safe_bind_rows(rows)
}

dise_publication_check_values <- function(district_year, checks) {
  x <- safe_df(district_year)
  checks <- safe_df(checks)
  x$state_key <- canonicalize_state_name(x$state_name_dise)
  x$district_key <- canonicalize_district_name(x$district_name_dise)
  checks$state_key <- canonicalize_state_name(checks$state)
  checks$district_key <- canonicalize_district_name(checks$district)

  safe_bind_rows(lapply(seq_len(nrow(checks)), function(i) {
    check <- checks[i, , drop = FALSE]
    row <- x[
      x$academic_year == check$academic_year[[1]] &
        x$state_key == check$state_key[[1]] &
        x$district_key == check$district_key[[1]],
      , drop = FALSE
    ]
    metric <- check$metric[[1]]
    actual <- if (nrow(row) == 1L && metric %in% names(row)) num(row[[metric]])[[1]] else NA_real_
    expected <- num(check$expected_value)[[1]]
    data.frame(
      academic_year = check$academic_year,
      state = check$state,
      district = check$district,
      metric = metric,
      expected_value = expected,
      actual_value = actual,
      difference = actual - expected,
      matches = is.finite(actual) && is.finite(expected) && identical(actual, expected),
      source_pdf = check$source_pdf,
      source_page = check$source_page,
      note = check$note,
      stringsAsFactors = FALSE
    )
  }))
}

diagnose_dise_archive <- function(district_year, treatments, publication_checks = data.frame()) {
  year_summary <- safe_bind_rows(lapply(split(district_year, district_year$academic_year), function(x) {
    data.frame(
      academic_year = x$academic_year[[1]],
      n_districts = nrow(x),
      n_identity_complete = sum(x$dise_medium_identity_complete %||% FALSE, na.rm = TRUE),
      n_english_resolved = sum(x$dise_english_identity_resolved %||% FALSE, na.rm = TRUE),
      n_hindi_resolved = sum(x$dise_hindi_identity_resolved %||% FALSE, na.rm = TRUE),
      median_medium_classification_ratio = stats::median(
        num(x$dise_medium_classification_ratio), na.rm = TRUE
      ),
      n_medium_classification_above_total = sum(
        num(x$dise_medium_classification_ratio) > 100, na.rm = TRUE
      ),
      max_medium_classification_ratio = max(
        num(x$dise_medium_classification_ratio), na.rm = TRUE
      ),
      median_abs_medium_classification_gap = stats::median(
        abs(num(x$dise_medium_classification_ratio) - 100), na.rm = TRUE
      ),
      max_abs_management_enrollment_difference = max(
        abs(num(x$dise_management_enrollment_difference)), na.rm = TRUE
      ),
      stringsAsFactors = FALSE
    )
  }))
  treatment_summary <- safe_bind_rows(lapply(dise_construct_registry()$variable, function(variable) {
    values <- num(treatments[[variable]])
    data.frame(
      variable = variable,
      n_nonmissing = sum(is.finite(values)),
      mean = if (any(is.finite(values))) mean(values, na.rm = TRUE) else NA_real_,
      sd = if (sum(is.finite(values)) > 1L) stats::sd(values, na.rm = TRUE) else NA_real_,
      min = if (any(is.finite(values))) min(values, na.rm = TRUE) else NA_real_,
      max = if (any(is.finite(values))) max(values, na.rm = TRUE) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
  list(
    year_summary = year_summary,
    treatment_summary = treatment_summary,
    publication_checks = if (nrow(publication_checks)) {
      dise_publication_check_values(district_year, publication_checks)
    } else {
      data.frame()
    }
  )
}

estimate_dise_first_stage_suite <- function(panel, construct) {
  treatment <- construct$variable[[1]]
  data <- prepare_alternative_distance_panel(panel, treatment)
  registry <- iv_diagnostic_specification_registry(treatment = treatment)
  estimated <- lapply(seq_len(nrow(registry)), function(i) {
    estimate_alternative_distance_spec(data, registry[i, , drop = FALSE], treatment)
  })
  list(
    summary = add_dise_construct_id(safe_bind_rows(lapply(estimated, `[[`, "summary")), construct),
    coefficients = add_dise_construct_id(safe_bind_rows(lapply(estimated, `[[`, "coefficients")), construct),
    registry = registry
  )
}

diagnose_dise_iv_permutations <- function(
  panel,
  constructs = dise_construct_registry(),
  outcome = "real_log_consumption_change"
) {
  first_stage <- list()
  coefficients <- list()
  weak_summary <- list()
  ar_grid <- list()
  overidentification <- list()
  monotonicity_summary <- list()
  monotonicity_bins <- list()
  monotonicity_state <- list()
  balance <- list()
  joint_balance <- list()

  for (i in seq_len(nrow(constructs))) {
    construct <- constructs[i, , drop = FALSE]
    variable <- construct$variable[[1]]
    if (!variable %in% names(panel)) next
    fs <- estimate_dise_first_stage_suite(panel, construct)
    first_stage[[i]] <- fs$summary
    coefficients[[i]] <- fs$coefficients

    if (!identical(construct$analysis_scope[[1]], "structural_iv")) next
    weak <- estimate_weak_iv_outcomes(panel, outcome = outcome, treatment = variable)
    weak_summary[[i]] <- add_dise_construct_id(weak$summary, construct)
    ar_grid[[i]] <- add_dise_construct_id(weak$ar_grid, construct)
    overidentification[[i]] <- add_dise_construct_id(weak$overidentification, construct)
    mono <- run_iv_monotonicity_diagnostics(panel, specifications = weak$registry)
    monotonicity_summary[[i]] <- add_dise_construct_id(mono$summary, construct)
    monotonicity_bins[[i]] <- add_dise_construct_id(mono$bins, construct)
    monotonicity_state[[i]] <- add_dise_construct_id(mono$state_slopes, construct)
    balance_panel <- panel[is.finite(num(panel[[variable]])), , drop = FALSE]
    balance[[i]] <- add_dise_construct_id(
      run_iv_balance_diagnostics(balance_panel, specifications = weak$registry), construct
    )
    joint_balance[[i]] <- add_dise_construct_id(
      run_iv_joint_balance_diagnostics(balance_panel, specifications = weak$registry), construct
    )
  }

  list(
    construct_registry = constructs,
    nss_validation = diagnose_dise_nss_validation(panel),
    first_stage = safe_bind_rows(first_stage),
    first_stage_coefficients = safe_bind_rows(coefficients),
    weak_iv_outcomes = safe_bind_rows(weak_summary),
    anderson_rubin_grid = safe_bind_rows(ar_grid),
    overidentification = safe_bind_rows(overidentification),
    monotonicity_summary = safe_bind_rows(monotonicity_summary),
    monotonicity_bins = safe_bind_rows(monotonicity_bins),
    monotonicity_state_slopes = safe_bind_rows(monotonicity_state),
    balance = safe_bind_rows(balance),
    joint_balance = safe_bind_rows(joint_balance)
  )
}

save_dise_diagnostics <- function(
  archive_diagnostics,
  permutations,
  district_year,
  treatments,
  dir = "outputs/diagnostics/extended/dise"
) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  outputs <- c(
    district_year = write_diagnostic_csv(district_year, file.path(dir, "dise_district_year_measures.csv")),
    treatments = write_diagnostic_csv(treatments, file.path(dir, "dise_baseline_treatments.csv")),
    archive_summary = write_diagnostic_csv(archive_diagnostics$year_summary, file.path(dir, "dise_archive_summary.csv")),
    treatment_summary = write_diagnostic_csv(archive_diagnostics$treatment_summary, file.path(dir, "dise_treatment_summary.csv")),
    publication_checks = write_diagnostic_csv(archive_diagnostics$publication_checks, file.path(dir, "dise_publication_checks.csv")),
    construct_registry = write_diagnostic_csv(permutations$construct_registry, file.path(dir, "dise_construct_registry.csv")),
    nss_validation = write_diagnostic_csv(permutations$nss_validation, file.path(dir, "dise_nss_validation.csv")),
    first_stage = write_diagnostic_csv(permutations$first_stage, file.path(dir, "dise_first_stage_permutations.csv")),
    first_stage_coefficients = write_diagnostic_csv(permutations$first_stage_coefficients, file.path(dir, "dise_first_stage_coefficients.csv")),
    weak_iv_outcomes = write_diagnostic_csv(permutations$weak_iv_outcomes, file.path(dir, "dise_weak_iv_outcomes.csv")),
    ar_grid = write_diagnostic_csv(permutations$anderson_rubin_grid, file.path(dir, "dise_anderson_rubin_grid.csv")),
    overidentification = write_diagnostic_csv(permutations$overidentification, file.path(dir, "dise_overidentification.csv")),
    monotonicity_summary = write_diagnostic_csv(permutations$monotonicity_summary, file.path(dir, "dise_monotonicity_summary.csv")),
    monotonicity_bins = write_diagnostic_csv(permutations$monotonicity_bins, file.path(dir, "dise_monotonicity_bins.csv")),
    monotonicity_state = write_diagnostic_csv(permutations$monotonicity_state_slopes, file.path(dir, "dise_monotonicity_state_slopes.csv")),
    balance = write_diagnostic_csv(permutations$balance, file.path(dir, "dise_instrument_balance.csv")),
    joint_balance = write_diagnostic_csv(permutations$joint_balance, file.path(dir, "dise_instrument_balance_joint.csv"))
  )
  output_manifest(outputs)
}
