# Alternative linguistic-distance first stages and coverage diagnostics.

alternative_distance_adjustments <- function() iv_adjustment_sets()

alternative_distance_constructions <- function() iv_instrument_constructions()

alternative_distance_registry <- function() iv_specification_registry()

alternative_distance_variables <- function() {
  constructions <- alternative_distance_constructions()
  unique(c(
    unlist(lapply(constructions, `[[`, "excluded"), use.names = FALSE),
    unlist(lapply(constructions, `[[`, "included"), use.names = FALSE),
    unlist(lapply(constructions, `[[`, "coverage"), use.names = FALSE)
  ))
}

partial_r_squared_instrument_set <- function(data, treatment, excluded, included, controls, fixed_effect) {
  nuisance <- unique(c(controls, included))
  restricted_rhs <- iv_nuisance_terms(nuisance, fixed_effect)
  restricted <- if (length(restricted_rhs)) {
    stats::lm(stats::reformulate(restricted_rhs, response = treatment), data = data)
  } else {
    stats::lm(stats::reformulate(character(), response = treatment), data = data)
  }
  full_rhs <- c(excluded, nuisance)
  if (identical(fixed_effect, "region")) full_rhs <- c(full_rhs, "factor(region)")
  if (identical(fixed_effect, "state")) full_rhs <- c(full_rhs, "factor(state_code_2001)")
  full <- stats::lm(stats::reformulate(full_rhs, response = treatment), data = data)
  denominator <- stats::deviance(restricted)
  if (!is.finite(denominator) || denominator <= 0) return(NA_real_)
  max(0, (denominator - stats::deviance(full)) / denominator)
}

estimate_alternative_distance_spec <- function(data, specification, treatment) {
  controls <- unlist(specification$controls[[1]], use.names = FALSE)
  included <- unlist(specification$included_language_controls[[1]], use.names = FALSE)
  excluded <- unlist(specification$excluded_instruments[[1]], use.names = FALSE)
  fixed_effect <- specification$fixed_effect[[1]]
  rhs <- unique(c(excluded, included, controls))
  if (identical(fixed_effect, "region")) rhs <- c(rhs, "factor(region)")
  if (identical(fixed_effect, "state")) rhs <- c(rhs, "factor(state_code_2001)")
  fit <- stats::lm(stats::reformulate(rhs, response = treatment), data = data)
  cluster <- iv_specification_cluster(data, specification)
  joint <- clustered_joint_wald_test(fit, excluded, cluster)
  coefficients <- safe_bind_rows(lapply(excluded, function(term) {
    inference <- clustered_first_stage_inference(fit, term, cluster)
    data.frame(
      specification_id = specification$specification_id,
      term = term,
      estimate = unname(stats::coef(fit)[term]),
      std.error = unname(inference[["std.error"]]),
      statistic = unname(inference[["statistic"]]),
      p.value = unname(inference[["p.value"]]),
      stringsAsFactors = FALSE
    )
  }))
  summary <- data.frame(
    specification_id = specification$specification_id,
    sequence = specification$sequence,
    adjustment_id = specification$adjustment_id,
    adjustment = specification$adjustment,
    construction_id = specification$construction_id,
    construction = specification$construction,
    fixed_effect = fixed_effect,
    excluded_instruments = paste(excluded, collapse = ";"),
    included_language_controls = paste(included, collapse = ";"),
    n_excluded_instruments = length(excluded),
    joint_excluded_f = unname(joint[["statistic"]]),
    joint_excluded_p = unname(joint[["p.value"]]),
    partial_r_squared = partial_r_squared_instrument_set(
      data, treatment, excluded, included, controls, fixed_effect
    ),
    n = stats::nobs(fit),
    n_states = length(unique(data$state_code_2001)),
    n_regions = length(unique(data$region)),
    stringsAsFactors = FALSE
  )
  list(summary = summary, coefficients = coefficients)
}

prepare_alternative_distance_panel <- function(panel, treatment = "emi_exposure_all_children_0708") {
  x <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else as.data.frame(panel, stringsAsFactors = FALSE)
  needed <- unique(c(
    treatment, "state_code_2001", "district_code_2001", "region",
    census_2001_diagnostic_controls(), alternative_distance_variables()
  ))
  missing <- setdiff(needed, names(x))
  if (length(missing)) stop("Alternative-distance panel is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  numeric_vars <- setdiff(needed, c("state_code_2001", "district_code_2001", "region"))
  for (variable in numeric_vars) x[[variable]] <- num(x[[variable]])
  x$state_code_2001 <- plain_chr(x$state_code_2001)
  x$district_code_2001 <- plain_chr(x$district_code_2001)
  if (!"district_panel_id" %in% names(x)) {
    x$district_panel_id <- make_district_key(x$state_code_2001, x$district_code_2001, 2001L)
  }
  x$region <- as.character(x$region)
  keep <- stats::complete.cases(x[needed]) & nzchar(x$state_code_2001) &
    nzchar(x$district_code_2001) & nzchar(x$region)
  x <- x[keep, , drop = FALSE]
  rownames(x) <- NULL
  if (!nrow(x)) stop("No complete common support is available for alternative-distance diagnostics.", call. = FALSE)
  if (length(unique(x$region)) != length(panel_region_levels())) {
    stop("Alternative-distance common support does not contain all six panel regions.", call. = FALSE)
  }
  x
}

diagnose_alternative_distance_first_stages <- function(
  panel,
  treatment = "emi_exposure_all_children_0708"
) {
  data <- prepare_alternative_distance_panel(panel, treatment)
  registry <- alternative_distance_registry()
  estimated <- lapply(seq_len(nrow(registry)), function(i) {
    estimate_alternative_distance_spec(data, registry[i, , drop = FALSE], treatment)
  })
  structure(
    list(
      summary = safe_bind_rows(lapply(estimated, `[[`, "summary")),
      coefficients = safe_bind_rows(lapply(estimated, `[[`, "coefficients")),
      registry = registry,
      common_support = data.frame(
        treatment = treatment, n = nrow(data),
        n_states = length(unique(data$state_code_2001)),
        n_regions = length(unique(data$region)), stringsAsFactors = FALSE
      ),
      coverage_sensitivity = estimate_alternative_distance_by_coverage(data, registry, treatment),
      distance4_languages = data.frame(status = character()),
      unmapped_languages = data.frame(status = character()),
      distance4_leave_one_out = data.frame(status = character()),
      weak_iv_outcomes = data.frame(status = character()),
      anderson_rubin_grid = data.frame(status = character()),
      overidentification = data.frame(status = character()),
      monotonicity_summary = data.frame(status = character()),
      monotonicity_bins = data.frame(status = character()),
      monotonicity_state_slopes = data.frame(status = character()),
      basis_comparison = data.frame(status = character())
    ),
    class = "emi_alternative_distance_first_stages"
  )
}

save_alternative_distance_first_stages <- function(
  diagnostics,
  dir = "outputs/diagnostics/extended/instrument_relevance"
) {
  if (!inherits(diagnostics, "emi_alternative_distance_first_stages")) {
    stop("Expected alternative-distance first-stage diagnostics.", call. = FALSE)
  }
  registry <- diagnostics$registry
  diagnostic_specifications <- diagnostics$diagnostic_specifications %||% iv_diagnostic_specification_registry()
  for (column in c("controls", "included_language_controls", "excluded_instruments")) {
    registry[[column]] <- vapply(registry[[column]], paste, collapse = ";", FUN.VALUE = character(1))
    diagnostic_specifications[[column]] <- vapply(
      diagnostic_specifications[[column]], paste, collapse = ";", FUN.VALUE = character(1)
    )
  }
  output_manifest(c(
    alternative_summary = write_diagnostic_csv(
      diagnostics$summary, file.path(dir, "alternative_distance_first_stage_summary.csv")
    ),
    alternative_coefficients = write_diagnostic_csv(
      diagnostics$coefficients, file.path(dir, "alternative_distance_first_stage_coefficients.csv")
    ),
    alternative_registry = write_diagnostic_csv(
      registry, file.path(dir, "alternative_distance_first_stage_registry.csv")
    ),
    alternative_support = write_diagnostic_csv(
      diagnostics$common_support, file.path(dir, "alternative_distance_first_stage_common_support.csv")
    ),
    coverage_sensitivity = write_diagnostic_csv(
      diagnostics$coverage_sensitivity, file.path(dir, "alternative_distance_mapping_coverage_sensitivity.csv")
    ),
    distance4_languages = write_diagnostic_csv(
      diagnostics$distance4_languages, file.path(dir, "distance4_language_decomposition.csv")
    ),
    unmapped_languages = write_diagnostic_csv(
      diagnostics$unmapped_languages, file.path(dir, "unmapped_language_decomposition.csv")
    ),
    distance4_leave_one_out = write_diagnostic_csv(
      diagnostics$distance4_leave_one_out, file.path(dir, "distance4_leave_one_language_out.csv")
    ),
    weak_iv_outcomes = write_diagnostic_csv(
      diagnostics$weak_iv_outcomes, file.path(dir, "alternative_distance_weak_iv_outcomes.csv")
    ),
    anderson_rubin_grid = write_diagnostic_csv(
      diagnostics$anderson_rubin_grid, file.path(dir, "alternative_distance_anderson_rubin_grid.csv")
    ),
    diagnostic_applicability = write_diagnostic_csv(
      diagnostics$diagnostic_applicability %||% iv_diagnostic_applicability(diagnostics$registry),
      file.path(dir, "iv_diagnostic_applicability.csv")
    ),
    diagnostic_registry = write_diagnostic_csv(
      diagnostics$diagnostic_registry %||% iv_diagnostic_registry(),
      file.path(dir, "iv_diagnostic_registry.csv")
    ),
    diagnostic_specifications = write_diagnostic_csv(
      diagnostic_specifications,
      file.path(dir, "iv_specification_registry.csv")
    ),
    overidentification = write_diagnostic_csv(
      diagnostics$overidentification,
      file.path(dir, "iv_overidentification.csv")
    ),
    monotonicity_summary = write_diagnostic_csv(
      diagnostics$monotonicity_summary,
      file.path(dir, "iv_monotonicity_summary.csv")
    ),
    monotonicity_bins = write_diagnostic_csv(
      diagnostics$monotonicity_bins,
      file.path(dir, "iv_monotonicity_bins.csv")
    ),
    monotonicity_state_slopes = write_diagnostic_csv(
      diagnostics$monotonicity_state_slopes,
      file.path(dir, "iv_monotonicity_state_slopes.csv")
    ),
    basis_comparison = write_diagnostic_csv(
      diagnostics$basis_comparison,
      file.path(dir, "linguistic_distance_basis_comparison.csv")
    )
  ))
}

linguistic_mapping_coverage_thresholds <- function() c(0, 90, 95, 99)

estimate_alternative_distance_by_coverage <- function(data, registry, treatment) {
  safe_bind_rows(lapply(seq_len(nrow(registry)), function(i) {
    specification <- registry[i, , drop = FALSE]
    coverage_variable <- specification$mapping_coverage_variable[[1]]
    safe_bind_rows(lapply(linguistic_mapping_coverage_thresholds(), function(threshold) {
      sample <- data[
        is.finite(num(data[[coverage_variable]])) & num(data[[coverage_variable]]) >= threshold,
        , drop = FALSE
      ]
      if (!nrow(sample)) return(NULL)
      out <- estimate_alternative_distance_spec(sample, specification, treatment)$summary
      out$minimum_mapped_share <- threshold
      out$coverage_variable <- coverage_variable
      out$coverage_sample_n <- nrow(sample)
      out
    }))
  }))
}

prepare_language_rows_for_decomposition <- function(census_2001_languages, panel) {
  rows <- std(safe_df(census_2001_languages), 2001L)
  needed <- c("state_std", "district_std", "spkr_tot", "canonical_language")
  if (!all(needed %in% names(rows))) stop("C-16 decomposition requires cleaned district-language rows.", call. = FALSE)
  rows$language_identity <- census_mother_tongue_identity(rows)
  if (!"ling_degrees" %in% names(rows)) {
    rows$ling_degrees <- resolve_shastry_language_degrees(rows)
  }
  rows$district_panel_id <- make_district_key(rows$state_std, rows$district_std, 2001L)
  panel_df <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else as.data.frame(panel)
  keep_ids <- unique(plain_chr(panel_df$district_panel_id))
  rows <- rows[rows$district_panel_id %in% keep_ids & is.finite(num(rows$ling_degrees)), , drop = FALSE]
  rows
}

distance_four_language_decomposition <- function(census_2001_languages, panel) {
  rows <- prepare_language_rows_for_decomposition(census_2001_languages, panel)
  rows <- rows[num(rows$ling_degrees) == 4, , drop = FALSE]
  if (!nrow(rows)) return(data.frame())
  by_language_state <- aggregate(
    num(rows$spkr_tot),
    list(
      mother_tongue = plain_chr(rows$language_identity),
      canonical_language = plain_chr(rows$canonical_language),
      state_code_2001 = plain_chr(rows$state_std)
    ),
    sum,
    na.rm = TRUE
  )
  names(by_language_state)[4] <- "speakers"
  by_language_state$n_districts <- mapply(function(language, state) {
    index <- plain_chr(rows$language_identity) == language & plain_chr(rows$state_std) == state
    length(unique(rows$district_panel_id[index]))
  }, by_language_state$mother_tongue, by_language_state$state_code_2001)
  national <- aggregate(by_language_state$speakers, list(by_language_state$mother_tongue), sum)
  names(national) <- c("mother_tongue", "national_language_speakers")
  out <- merge(by_language_state, national, by = "mother_tongue", all.x = TRUE)
  out$speaker_share_of_distance4 <- 100 * out$speakers / sum(out$speakers)
  out[order(out$speakers, decreasing = TRUE), , drop = FALSE]
}

unmapped_language_decomposition <- function(census_2001_languages, panel) {
  rows <- std(safe_df(census_2001_languages), 2001L)
  rows$language_identity <- census_mother_tongue_identity(rows)
  if (!"ling_degrees" %in% names(rows)) {
    rows$ling_degrees <- resolve_shastry_language_degrees(rows)
  }
  rows$district_panel_id <- make_district_key(rows$state_std, rows$district_std, 2001L)
  panel_df <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else as.data.frame(panel)
  rows <- rows[
    rows$district_panel_id %in% plain_chr(panel_df$district_panel_id) &
      !is.finite(num(rows$ling_degrees)) & rows$language_identity != "English",
    , drop = FALSE
  ]
  if (!nrow(rows)) return(data.frame())
  out <- aggregate(
    num(rows$spkr_tot),
    list(
      mother_tongue = plain_chr(rows$language_identity),
      canonical_language = plain_chr(rows$canonical_language),
      state_code_2001 = plain_chr(rows$state_std)
    ),
    sum,
    na.rm = TRUE
  )
  names(out)[4] <- "unmapped_speakers"
  out$n_districts <- mapply(function(language, state) {
    index <- plain_chr(rows$language_identity) == language & plain_chr(rows$state_std) == state
    length(unique(rows$district_panel_id[index]))
  }, out$mother_tongue, out$state_code_2001)
  out$share_of_unmapped_speakers <- 100 * out$unmapped_speakers / sum(out$unmapped_speakers)
  out[order(out$unmapped_speakers, decreasing = TRUE), , drop = FALSE]
}

estimate_weak_iv_outcomes <- function(
  panel,
  outcome = "real_log_consumption_change",
  treatment = "emi_exposure_all_children_0708"
) {
  data <- prepare_alternative_distance_panel(panel, treatment)
  if (!outcome %in% names(data)) stop("Weak-IV outcome panel is missing ", outcome, ".", call. = FALSE)
  registry <- iv_diagnostic_specification_registry(outcome = outcome, treatment = treatment)
  estimated <- lapply(seq_len(nrow(registry)), function(i) {
    spec <- registry[i, , drop = FALSE]
    controls <- unlist(spec$controls[[1]], use.names = FALSE)
    included <- unlist(spec$included_language_controls[[1]], use.names = FALSE)
    excluded <- unlist(spec$excluded_instruments[[1]], use.names = FALSE)
    fixed <- iv_fixed_effect_terms(spec$fixed_effect[[1]])
    needed <- iv_specification_variables(spec)
    missing <- setdiff(needed, names(data))
    if (length(missing)) {
      stop(
        "Weak-IV specification ", spec$specification_id,
        " is missing columns: ", paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    x <- data[stats::complete.cases(data[needed]), , drop = FALSE]
    if (!nrow(x)) return(NULL)

    fit <- ivreg::ivreg(iv_specification_formula(spec), data = x, model = TRUE, x = TRUE, y = TRUE)
    cluster <- iv_specification_cluster(x, spec)
    inference <- iv_clustered_inference(fit, cluster)
    ct <- lmtest::coeftest(fit, vcov. = inference$vcov)
    row <- match(treatment, rownames(ct))
    reduced <- stats::lm(
      stats::reformulate(unique(c(excluded, included, controls, fixed)), response = outcome),
      data = x
    )
    reduced_test <- clustered_joint_wald_test(reduced, excluded, cluster)
    ar <- estimate_anderson_rubin_spec(x, spec)
    overidentification <- if (spec$n_excluded_instruments[[1]] > spec$n_endogenous[[1]]) {
      result <- ivreg_sargan_diagnostic(fit)
      cbind(
        data.frame(
          specification_id = spec$specification_id,
          n_endogenous = spec$n_endogenous,
          n_excluded_instruments = spec$n_excluded_instruments,
          stringsAsFactors = FALSE
        ),
        result
      )
    } else {
      data.frame(
        specification_id = spec$specification_id,
        n_endogenous = spec$n_endogenous,
        n_excluded_instruments = spec$n_excluded_instruments,
        test = "sargan",
        status = "not_applicable",
        statistic = NA_real_,
        df = NA_real_,
        p.value = NA_real_,
        reason = "Exactly identified.",
        stringsAsFactors = FALSE
      )
    }
    summary <- cbind(
      data.frame(
        specification_id = spec$specification_id,
        adjustment_id = spec$adjustment_id,
        construction_id = spec$construction_id,
        estimate_2sls = unname(stats::coef(fit)[treatment]),
        std_error_clustered = ct[row, 2],
        p_value_clustered = ct[row, 4],
        reduced_form_joint_f = reduced_test[["statistic"]],
        reduced_form_joint_p = reduced_test[["p.value"]],
        stringsAsFactors = FALSE
      ),
      ar$summary[setdiff(names(ar$summary), "specification_id")]
    )
    list(summary = summary, grid = ar$grid, overidentification = overidentification)
  })
  estimated <- Filter(Negate(is.null), estimated)
  list(
    summary = safe_bind_rows(lapply(estimated, `[[`, "summary")),
    ar_grid = safe_bind_rows(lapply(estimated, `[[`, "grid")),
    overidentification = safe_bind_rows(lapply(estimated, `[[`, "overidentification")),
    registry = registry,
    applicability = iv_diagnostic_applicability(registry)
  )
}

distance_four_leave_one_language_out <- function(census_2001_languages, panel, treatment = "emi_exposure_all_children_0708") {
  rows <- prepare_language_rows_for_decomposition(census_2001_languages, panel)
  rows <- rows[num(rows$ling_degrees) == 4, , drop = FALSE]
  if (!nrow(rows)) return(data.frame())
  panel_data <- prepare_alternative_distance_panel(panel, treatment)
  total <- aggregate(num(prepare_language_rows_for_decomposition(census_2001_languages, panel)$spkr_tot),
    list(prepare_language_rows_for_decomposition(census_2001_languages, panel)$district_panel_id), sum, na.rm = TRUE)
  names(total) <- c("district_panel_id", "all_speakers")
  languages <- sort(unique(plain_chr(rows$language_identity)))
  spec <- alternative_distance_registry()
  spec <- spec[spec$adjustment_id == "state_expanded" & spec$construction_id == "distance_shares_all_unmapped", , drop = FALSE]
  safe_bind_rows(lapply(languages, function(language) {
    lang <- rows[plain_chr(rows$language_identity) == language, , drop = FALSE]
    amount <- aggregate(num(lang$spkr_tot), list(lang$district_panel_id), sum, na.rm = TRUE)
    names(amount) <- c("district_panel_id", "language_speakers")
    share <- merge(total, amount, by = "district_panel_id", all.x = TRUE)
    share$language_speakers[is.na(share$language_speakers)] <- 0
    share$language_share <- 100 * share$language_speakers / share$all_speakers
    altered <- panel_data
    altered$ling_share_distance_4 <- altered$ling_share_distance_4 -
      share$language_share[match(altered$district_panel_id, share$district_panel_id)]
    result <- estimate_alternative_distance_spec(altered, spec, treatment)$summary
    data.frame(
      omitted_distance4_language = language,
      joint_excluded_f = result$joint_excluded_f,
      joint_excluded_p = result$joint_excluded_p,
      partial_r_squared = result$partial_r_squared,
      n = result$n,
      stringsAsFactors = FALSE
    )
  }))
}

compare_linguistic_distance_bases <- function(panel) {
  x <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else as.data.frame(panel)
  variables <- c(
    shastry = "ling_distance_nonzero_mean",
    glottolog = "ling_distance_glottolog_nonhindi_mean",
    dyen = "ling_distance_dyen_noncognate_pct"
  )
  available <- variables[variables %in% names(x)]
  if (length(available) < 2L) return(data.frame())

  pairs <- utils::combn(names(available), 2L, simplify = FALSE)
  safe_bind_rows(lapply(pairs, function(pair) {
    a <- available[[pair[[1]]]]
    b <- available[[pair[[2]]]]
    keep <- stats::complete.cases(x[c(a, b)])
    values <- x[keep, c(a, b), drop = FALSE]
    if (!nrow(values)) return(NULL)
    data.frame(
      basis_a = pair[[1]],
      variable_a = a,
      basis_b = pair[[2]],
      variable_b = b,
      n = nrow(values),
      pearson_correlation = stats::cor(values[[a]], values[[b]], method = "pearson"),
      spearman_correlation = stats::cor(values[[a]], values[[b]], method = "spearman"),
      mean_a = mean(values[[a]]),
      mean_b = mean(values[[b]]),
      stringsAsFactors = FALSE
    )
  }))
}

augment_alternative_distance_diagnostics <- function(diagnostics, panel, census_2001_languages, outcome = "real_log_consumption_change") {
  if (!inherits(diagnostics, "emi_alternative_distance_first_stages")) stop("Expected alternative-distance diagnostics.", call. = FALSE)
  data <- prepare_alternative_distance_panel(panel, diagnostics$common_support$treatment[[1]])
  diagnostics$coverage_sensitivity <- estimate_alternative_distance_by_coverage(
    data, diagnostics$registry, diagnostics$common_support$treatment[[1]]
  )
  diagnostics$distance4_languages <- distance_four_language_decomposition(census_2001_languages, panel)
  diagnostics$unmapped_languages <- unmapped_language_decomposition(census_2001_languages, panel)
  diagnostics$distance4_leave_one_out <- distance_four_leave_one_language_out(
    census_2001_languages, panel, diagnostics$common_support$treatment[[1]]
  )
  weak_iv <- estimate_weak_iv_outcomes(
    panel, outcome = outcome, treatment = diagnostics$common_support$treatment[[1]]
  )
  diagnostics$weak_iv_outcomes <- weak_iv$summary
  diagnostics$anderson_rubin_grid <- weak_iv$ar_grid
  diagnostics$overidentification <- weak_iv$overidentification
  diagnostics$diagnostic_applicability <- weak_iv$applicability
  diagnostics$diagnostic_registry <- iv_diagnostic_registry()
  diagnostics$diagnostic_specifications <- weak_iv$registry
  monotonicity <- run_iv_monotonicity_diagnostics(
    panel, specifications = weak_iv$registry
  )
  diagnostics$monotonicity_summary <- monotonicity$summary
  diagnostics$monotonicity_bins <- monotonicity$bins
  diagnostics$monotonicity_state_slopes <- monotonicity$state_slopes
  diagnostics$basis_comparison <- compare_linguistic_distance_bases(panel)
  diagnostics
}
