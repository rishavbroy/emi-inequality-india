# DISE treatment validation and IV-permutation diagnostics.

add_dise_construct_id <- function(data, construct) {
  data <- safe_df(data)
  if (!nrow(data)) return(data)
  data$construct_id <- construct$construct_id[[1]]
  data$treatment <- construct$variable[[1]]
  for (field in intersect(
    c("analysis_scope", "domain", "margin", "source_side", "paper_role", "label"),
    names(construct)
  )) {
    data[[field]] <- construct[[field]][[1]]
  }
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
  construct_registry <- dise_construct_registry()
  treatment_summary <- safe_bind_rows(lapply(seq_len(nrow(construct_registry)), function(i) {
    construct <- construct_registry[i, , drop = FALSE]
    variable <- construct$variable[[1]]
    values <- num(treatments[[variable]])
    summary <- data.frame(
      variable = variable,
      n_nonmissing = sum(is.finite(values)),
      mean = if (any(is.finite(values))) mean(values, na.rm = TRUE) else NA_real_,
      sd = if (sum(is.finite(values)) > 1L) stats::sd(values, na.rm = TRUE) else NA_real_,
      min = if (any(is.finite(values))) min(values, na.rm = TRUE) else NA_real_,
      max = if (any(is.finite(values))) max(values, na.rm = TRUE) else NA_real_,
      stringsAsFactors = FALSE
    )
    add_dise_construct_id(summary, construct)
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

prepare_dise_iv_diagnostic_panel <- function(
  panel,
  constructs = dise_construct_registry(),
  outcome = "real_log_consumption_change"
) {
  x <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else safe_df(panel)
  validation <- dise_nss_validation_registry()
  needed <- unique(c(
    outcome,
    constructs$variable,
    validation$dise_variable,
    validation$nss_variable,
    "state_code_2001", "district_code_2001", "region",
    census_2001_diagnostic_controls(),
    alternative_distance_variables()
  ))
  missing <- setdiff(needed, names(x))
  if (length(missing)) {
    stop(
      "DISE IV diagnostic panel is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if ("district_panel_id" %in% names(x)) needed <- c(needed, "district_panel_id")
  out <- x[unique(needed)]
  rownames(out) <- NULL
  out
}

diagnose_dise_iv_construct <- function(
  panel,
  construct,
  outcome = "real_log_consumption_change"
) {
  construct <- as.data.frame(construct, stringsAsFactors = FALSE)
  if (nrow(construct) != 1L) {
    stop("DISE diagnostic branch requires exactly one construct.", call. = FALSE)
  }
  required <- c("construct_id", "variable", "analysis_scope")
  missing <- setdiff(required, names(construct))
  if (length(missing)) {
    stop(
      "DISE diagnostic construct is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  variable <- construct$variable[[1]]
  if (!variable %in% names(panel)) {
    return(list(
      first_stage = data.frame(), first_stage_coefficients = data.frame(),
      weak_iv_outcomes = data.frame(), anderson_rubin_grid = data.frame(),
      overidentification = data.frame(), monotonicity_summary = data.frame(),
      monotonicity_bins = data.frame(), monotonicity_state_slopes = data.frame(),
      balance = data.frame(), joint_balance = data.frame()
    ))
  }
  fs <- estimate_dise_first_stage_suite(panel, construct)
  out <- list(
    first_stage = fs$summary,
    first_stage_coefficients = fs$coefficients,
    weak_iv_outcomes = data.frame(), anderson_rubin_grid = data.frame(),
    overidentification = data.frame(), monotonicity_summary = data.frame(),
    monotonicity_bins = data.frame(), monotonicity_state_slopes = data.frame(),
    balance = data.frame(), joint_balance = data.frame()
  )
  if (!identical(construct$analysis_scope[[1]], "structural_iv")) return(out)
  weak <- estimate_weak_iv_outcomes(panel, outcome = outcome, treatment = variable)
  out$weak_iv_outcomes <- add_dise_construct_id(weak$summary, construct)
  out$anderson_rubin_grid <- add_dise_construct_id(weak$ar_grid, construct)
  out$overidentification <- add_dise_construct_id(weak$overidentification, construct)
  mono <- run_iv_monotonicity_diagnostics(panel, specifications = weak$registry)
  out$monotonicity_summary <- add_dise_construct_id(mono$summary, construct)
  out$monotonicity_bins <- add_dise_construct_id(mono$bins, construct)
  out$monotonicity_state_slopes <- add_dise_construct_id(mono$state_slopes, construct)
  balance_panel <- panel[is.finite(num(panel[[variable]])), , drop = FALSE]
  out$balance <- add_dise_construct_id(
    run_iv_balance_diagnostics(balance_panel, specifications = weak$registry), construct
  )
  out$joint_balance <- add_dise_construct_id(
    run_iv_joint_balance_diagnostics(balance_panel, specifications = weak$registry), construct
  )
  out
}

assemble_dise_iv_permutations <- function(constructs, nss_validation, branches) {
  branches <- unname(branches)
  collect <- function(name) safe_bind_rows(lapply(branches, `[[`, name))
  list(
    construct_registry = constructs,
    nss_validation = nss_validation,
    first_stage = collect("first_stage"),
    first_stage_coefficients = collect("first_stage_coefficients"),
    weak_iv_outcomes = collect("weak_iv_outcomes"),
    anderson_rubin_grid = collect("anderson_rubin_grid"),
    overidentification = collect("overidentification"),
    monotonicity_summary = collect("monotonicity_summary"),
    monotonicity_bins = collect("monotonicity_bins"),
    monotonicity_state_slopes = collect("monotonicity_state_slopes"),
    balance = collect("balance"),
    joint_balance = collect("joint_balance")
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
  data <- prepare_dise_iv_diagnostic_panel(panel, constructs, outcome)
  branches <- lapply(seq_len(nrow(constructs)), function(i) {
    diagnose_dise_iv_construct(
      data, constructs[i, , drop = FALSE], outcome = outcome
    )
  })
  assemble_dise_iv_permutations(
    constructs,
    diagnose_dise_nss_validation(data),
    branches
  )
}

save_dise_diagnostics <- function(
  archive_diagnostics,
  permutations,
  district_year,
  treatments,
  lineage_bridge = data.frame(),
  harmonized_district_year = data.frame(),
  dynamic_panel = data.frame(),
  dynamic_relevance = list(registry = data.frame(), summary = data.frame(), coefficients = data.frame()),
  school_quality = list(
    registry = data.frame(), baseline_association = data.frame(),
    report_panel = data.frame(), summary = data.frame(), coefficients = data.frame()
  ),
  age_exposure = list(
    anchors = data.frame(), population = data.frame(),
    dynamic_relevance = list(summary = data.frame(), coefficients = data.frame())
  ),
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
    joint_balance = write_diagnostic_csv(permutations$joint_balance, file.path(dir, "dise_instrument_balance_joint.csv")),
    lineage_bridge = write_diagnostic_csv(lineage_bridge, file.path(dir, "dise_lineage_bridge.csv")),
    harmonized_district_year = write_diagnostic_csv(
      harmonized_district_year, file.path(dir, "dise_district_year_2001.csv")
    ),
    dynamic_panel = write_diagnostic_csv(
      dynamic_panel, file.path(dir, "dise_dynamic_district_year_2001.csv")
    ),
    dynamic_registry = write_diagnostic_csv(
      dynamic_relevance$registry,
      file.path(dir, "dise_dynamic_specification_registry.csv")
    ),
    dynamic_summary = write_diagnostic_csv(
      dynamic_relevance$summary,
      file.path(dir, "dise_dynamic_first_stage_summary.csv")
    ),
    dynamic_coefficients = write_diagnostic_csv(
      dynamic_relevance$coefficients,
      file.path(dir, "dise_dynamic_first_stage_event_study.csv")
    ),
    school_quality_registry = write_diagnostic_csv(
      school_quality$registry, file.path(dir, "dise_school_quality_registry.csv")
    ),
    school_quality_baseline = write_diagnostic_csv(
      school_quality$baseline_association,
      file.path(dir, "dise_school_quality_baseline_association.csv")
    ),
    school_quality_report_panel = write_diagnostic_csv(
      school_quality$report_panel,
      file.path(dir, "dise_school_quality_report_2001.csv")
    ),
    school_quality_summary = write_diagnostic_csv(
      school_quality$summary, file.path(dir, "dise_school_quality_dynamic_summary.csv")
    ),
    school_quality_coefficients = write_diagnostic_csv(
      school_quality$coefficients,
      file.path(dir, "dise_school_quality_dynamic_event_study.csv")
    ),
    age_6_13_anchors = write_diagnostic_csv(
      age_exposure$anchors,
      file.path(dir, "census_age_6_13_anchors_2001_2011.csv")
    ),
    age_6_13_population = write_diagnostic_csv(
      age_exposure$population,
      file.path(dir, "census_age_6_13_population_by_academic_year.csv")
    ),
    age_exposure_dynamic_summary = write_diagnostic_csv(
      age_exposure$dynamic_relevance$summary,
      file.path(dir, "dise_elementary_age_exposure_dynamic_summary.csv")
    ),
    age_exposure_dynamic_coefficients = write_diagnostic_csv(
      age_exposure$dynamic_relevance$coefficients,
      file.path(dir, "dise_elementary_age_exposure_dynamic_event_study.csv")
    )
  )
  output_manifest(outputs)
}

dise_dynamic_instrument_registry <- function() {
  constructions <- alternative_distance_constructions()
  rows <- lapply(names(constructions), function(id) {
    x <- constructions[[id]]
    excluded <- unlist(x$excluded, use.names = FALSE)
    if (length(excluded) != 1L) return(NULL)
    data.frame(
      construction_id = id,
      construction = x$label,
      excluded_instrument = excluded[[1]],
      stringsAsFactors = FALSE
    )
  })
  out <- safe_bind_rows(rows)
  groups <- split(seq_len(nrow(out)), out$excluded_instrument)
  dedup <- safe_bind_rows(lapply(groups, function(i) {
    part <- out[i, , drop = FALSE]
    data.frame(
      construction_id = part$construction_id[[1]],
      construction = part$construction[[1]],
      excluded_instrument = part$excluded_instrument[[1]],
      equivalent_construction_ids = paste(sort(part$construction_id), collapse = ";"),
      stringsAsFactors = FALSE
    )
  }))
  rownames(dedup) <- NULL
  dedup
}

dise_dynamic_fe_registry <- function() {
  data.frame(
    dynamic_fe = c("district_year", "district_state_year"),
    label = c(
      "District FE + academic-year FE",
      "District FE + state-by-academic-year FE"
    ),
    stringsAsFactors = FALSE
  )
}

dise_year_interaction_terms <- function(years, instrument, reference_year = "2007-08") {
  years <- setdiff(sort(unique(plain_chr(years))), reference_year)
  safe <- gsub("[^0-9A-Za-z]+", "_", years)
  data.frame(
    academic_year = years,
    term = paste0("dise_distance_year_", safe),
    instrument = instrument,
    stringsAsFactors = FALSE
  )
}

estimate_dise_dynamic_spec <- function(
  data,
  instrument,
  dynamic_fe,
  reference_year = "2007-08",
  outcome = "dise_emi_enrollment_share_total"
) {
  x <- safe_df(data)
  required <- c(
    outcome, "target_unit_2001",
    "state_code_2001", "academic_year", instrument
  )
  x <- x[stats::complete.cases(x[required]), , drop = FALSE]
  rownames(x) <- NULL
  if (!nrow(x)) return(list(summary = data.frame(), coefficients = data.frame()))
  terms <- dise_year_interaction_terms(x$academic_year, instrument, reference_year)
  for (i in seq_len(nrow(terms))) {
    x[[terms$term[[i]]]] <- ifelse(
      x$academic_year == terms$academic_year[[i]],
      num(x[[instrument]]),
      0
    )
  }
  fixed <- if (identical(dynamic_fe, "district_state_year")) {
    c("factor(target_unit_2001)", "interaction(state_code_2001, academic_year, drop = TRUE)")
  } else {
    c("factor(target_unit_2001)", "factor(academic_year)")
  }
  fit <- stats::lm(
    stats::reformulate(c(fixed, terms$term), response = outcome),
    data = x
  )
  cluster <- x$target_unit_2001
  if (length(cluster) != stats::nobs(fit)) {
    stop("Dynamic DISE cluster vector is not aligned to fitted observations.", call. = FALSE)
  }
  inf <- iv_clustered_inference(fit, cluster)
  vc <- inf$vcov
  coef_rows <- safe_bind_rows(lapply(seq_len(nrow(terms)), function(i) {
    term <- terms$term[[i]]
    estimate <- unname(stats::coef(fit)[term])
    se <- if (!is.null(vc) && term %in% rownames(vc)) sqrt(vc[term, term]) else NA_real_
    statistic <- estimate / se
    data.frame(
      academic_year = terms$academic_year[[i]],
      reference_year = reference_year,
      estimate = estimate,
      std.error = se,
      statistic = statistic,
      p.value = if (is.finite(statistic)) 2 * stats::pnorm(abs(statistic), lower.tail = FALSE) else NA_real_,
      outcome = outcome,
      stringsAsFactors = FALSE
    )
  }))
  joint <- clustered_joint_wald_test(fit, terms$term, cluster)
  pre_terms <- terms$term[terms$academic_year < reference_year]
  post_terms <- terms$term[terms$academic_year > reference_year]
  pre_joint <- if (length(pre_terms)) {
    clustered_joint_wald_test(fit, pre_terms, cluster)
  } else {
    c(statistic = NA_real_, p.value = NA_real_)
  }
  post_joint <- if (length(post_terms)) {
    clustered_joint_wald_test(fit, post_terms, cluster)
  } else {
    c(statistic = NA_real_, p.value = NA_real_)
  }
  summary <- data.frame(
    instrument = instrument,
    dynamic_fe = dynamic_fe,
    reference_year = reference_year,
    n = stats::nobs(fit),
    n_districts = length(unique(x$target_unit_2001)),
    n_years = length(unique(x$academic_year)),
    joint_distance_year_f = unname(joint[["statistic"]]),
    joint_distance_year_p = unname(joint[["p.value"]]),
    pre_distance_year_f = unname(pre_joint[["statistic"]]),
    pre_distance_year_p = unname(pre_joint[["p.value"]]),
    post_distance_year_f = unname(post_joint[["statistic"]]),
    post_distance_year_p = unname(post_joint[["p.value"]]),
    cluster_status = inf$status,
    outcome = outcome,
    stringsAsFactors = FALSE
  )
  list(summary = summary, coefficients = coef_rows)
}

dise_school_quality_registry <- function() {
  data.frame(
    outcome = c(
      "dise_pupils_per_teacher",
      "dise_single_teacher_school_share",
      "dise_girls_toilet_school_share"
    ),
    baseline_outcome = c(
      "dise_pupils_per_teacher_0708",
      "dise_single_teacher_school_share_0708",
      "dise_girls_toilet_school_share_0708"
    ),
    dynamic_outcome = c(
      "dise_report_pupils_per_teacher",
      "dise_report_single_teacher_school_share",
      "dise_report_girls_toilet_school_share"
    ),
    label = c(
      "Pupils per teacher",
      "Single-teacher schools (%)",
      "Schools with girls' toilet (%)"
    ),
    direction = c("lower_is_better", "lower_is_better", "higher_is_better"),
    domain = "quality",
    margin = c("teacher_resources", "school_stock_quality", "school_amenity"),
    source_side = "administrative_supply",
    paper_role = "complementarity",
    dynamic_start_year = c("2011-12", "2011-12", "2012-13"),
    dynamic_reference_year = c("2011-12", "2011-12", "2012-13"),
    dynamic_status = "estimated_report_cards",
    definition_note = c(
      "Published all-school PTR.",
      "Published all-school single-teacher-school percentage.",
      paste(
        "Published all-school eligible-school percentage; 2011-12 is excluded",
        "because the report-card denominator changes from all schools to",
        "girls'/coeducational schools in 2012-13."
      )
    ),
    stringsAsFactors = FALSE
  )
}

estimate_dise_school_quality_baseline <- function(
  data,
  outcome,
  instrument = "ling_distance_nonzero_mean",
  academic_year = "2007-08"
) {
  x <- safe_df(data)
  required <- c(outcome, instrument, "state_code_2001", "target_unit_2001", "academic_year")
  x <- x[x$academic_year == academic_year & stats::complete.cases(x[required]), , drop = FALSE]
  rownames(x) <- NULL
  if (!nrow(x)) return(data.frame())

  fit <- stats::lm(
    stats::reformulate(c(instrument, "factor(state_code_2001)"), response = outcome),
    data = x
  )
  inf <- iv_clustered_inference(fit, x$state_code_2001)
  estimate <- unname(stats::coef(fit)[instrument])
  se <- if (!is.null(inf$vcov) && instrument %in% rownames(inf$vcov)) {
    sqrt(inf$vcov[instrument, instrument])
  } else {
    NA_real_
  }
  statistic <- estimate / se
  data.frame(
    academic_year = academic_year,
    outcome = outcome,
    instrument = instrument,
    estimate = estimate,
    std.error = se,
    statistic = statistic,
    p.value = if (is.finite(statistic)) {
      2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
    } else {
      NA_real_
    },
    n = stats::nobs(fit),
    cluster_variable = "state_code_2001",
    cluster_status = inf$status,
    stringsAsFactors = FALSE
  )
}

dise_school_quality_dynamic_rows <- function(
  data,
  registry_row,
  instrument,
  fes
) {
  start_year <- registry_row$dynamic_start_year[[1]]
  reference_year <- registry_row$dynamic_reference_year[[1]]
  x <- safe_df(data)
  years <- sort(unique(plain_chr(x$academic_year)))
  keep_years <- years[years >= start_year]
  x <- x[x$academic_year %in% keep_years, , drop = FALSE]
  results <- lapply(seq_len(nrow(fes)), function(j) {
    result <- estimate_dise_dynamic_spec(
      x,
      instrument,
      fes$dynamic_fe[[j]],
      reference_year = reference_year,
      outcome = registry_row$dynamic_outcome[[1]]
    )
    if (nrow(result$summary)) {
      result$summary$label <- registry_row$label[[1]]
      result$summary$dynamic_status <- registry_row$dynamic_status[[1]]
      result$summary$definition_note <- registry_row$definition_note[[1]]
    }
    if (nrow(result$coefficients)) {
      result$coefficients$label <- registry_row$label[[1]]
      result$coefficients$dynamic_status <- registry_row$dynamic_status[[1]]
      result$coefficients$definition_note <- registry_row$definition_note[[1]]
      result$coefficients$dynamic_fe <- fes$dynamic_fe[[j]]
      result$coefficients$instrument <- instrument
    }
    result
  })
  list(
    summary = safe_bind_rows(lapply(results, `[[`, "summary")),
    coefficients = safe_bind_rows(lapply(results, `[[`, "coefficients"))
  )
}

diagnose_dise_school_quality_mechanisms <- function(
  data,
  report_quality = data.frame(),
  baseline_years = c("2005-06", "2006-07"),
  instrument = "ling_distance_nonzero_mean"
) {
  registry <- dise_school_quality_registry()
  fes <- dise_dynamic_fe_registry()
  baseline <- safe_bind_rows(lapply(baseline_years, function(academic_year) {
    safe_bind_rows(lapply(registry$outcome, function(outcome) {
      estimate_dise_school_quality_baseline(data, outcome, instrument, academic_year)
    }))
  }))

  report <- safe_df(report_quality)
  dynamic_data <- merge(
    safe_df(data),
    report,
    by = c("target_unit_2001", "academic_year"),
    all.x = TRUE,
    sort = FALSE
  )
  dynamics <- lapply(seq_len(nrow(registry)), function(i) {
    dise_school_quality_dynamic_rows(
      dynamic_data,
      registry[i, , drop = FALSE],
      instrument,
      fes
    )
  })
  list(
    registry = registry,
    baseline_association = baseline,
    report_panel = report,
    summary = safe_bind_rows(lapply(dynamics, `[[`, "summary")),
    coefficients = safe_bind_rows(lapply(dynamics, `[[`, "coefficients"))
  )
}

diagnose_dise_dynamic_relevance <- function(
  data,
  reference_year = "2007-08",
  outcome = "dise_emi_enrollment_share_total"
) {
  instruments <- dise_dynamic_instrument_registry()
  fes <- dise_dynamic_fe_registry()
  results <- list()
  k <- 1L
  for (i in seq_len(nrow(instruments))) {
    for (j in seq_len(nrow(fes))) {
      result <- estimate_dise_dynamic_spec(
        data,
        instruments$excluded_instrument[[i]],
        fes$dynamic_fe[[j]],
        reference_year,
        outcome = outcome
      )
      if (nrow(result$summary)) {
        result$summary$construction_id <- instruments$construction_id[[i]]
        result$summary$equivalent_construction_ids <- instruments$equivalent_construction_ids[[i]]
      }
      if (nrow(result$coefficients)) {
        result$coefficients$construction_id <- instruments$construction_id[[i]]
        result$coefficients$instrument <- instruments$excluded_instrument[[i]]
        result$coefficients$dynamic_fe <- fes$dynamic_fe[[j]]
      }
      results[[k]] <- result
      k <- k + 1L
    }
  }
  list(
    registry = merge(instruments, fes, by = NULL),
    summary = safe_bind_rows(lapply(results, `[[`, "summary")),
    coefficients = safe_bind_rows(lapply(results, `[[`, "coefficients"))
  )
}
