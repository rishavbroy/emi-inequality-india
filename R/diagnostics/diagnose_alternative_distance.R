# Alternative linguistic-distance first stages and coverage diagnostics.

alternative_distance_adjustments <- function() {
  list(
    unadjusted = list(label = "Unadjusted", fixed_effect = "none", controls = character()),
    region_main = list(label = "Six-region FE + main controls", fixed_effect = "region", controls = census_2001_main_controls()),
    region_expanded = list(label = "Six-region FE + expanded controls", fixed_effect = "region", controls = census_2001_absorption_controls()),
    state_main = list(label = "State FE + main controls", fixed_effect = "state", controls = census_2001_main_controls()),
    state_expanded = list(label = "State FE + expanded controls", fixed_effect = "state", controls = census_2001_absorption_controls())
  )
}

alternative_distance_constructions <- function() {
  list(
    nonzero_mean = list(label = "Mean distance among speakers above zero", excluded = "ling_distance_nonzero_mean", included = character()),
    distant_share = list(label = "Share speaking languages at distance three or higher", excluded = "ling_share_distance_ge3", included = character()),
    top3_legacy = list(label = "Legacy top-three weighted mean", excluded = "ling_distance_top3_legacy", included = character()),
    nonzero_mean_hindi_urdu = list(label = "Nonzero mean with combined Hindi-Urdu share", excluded = "ling_distance_nonzero_mean", included = "hindi_urdu_share"),
    nonzero_mean_hindi_urdu_separate = list(label = "Nonzero mean with separate Hindi and Urdu shares", excluded = "ling_distance_nonzero_mean", included = c("hindi_share", "urdu_share")),
    distance_shares_all = list(label = "Five distance shares; all-speaker denominator", excluded = linguistic_distance_excluded_instruments("all"), included = character()),
    distance_shares_all_unmapped = list(label = "Five distance shares with unmapped share controlled", excluded = linguistic_distance_excluded_instruments("all"), included = "ling_unmapped_speaker_share"),
    distance_shares_mapped = list(label = "Five distance shares; mapped-speaker denominator", excluded = linguistic_distance_excluded_instruments("mapped"), included = character())
  )
}

alternative_distance_registry <- function() {
  adjustments <- alternative_distance_adjustments()
  constructions <- alternative_distance_constructions()
  rows <- list()
  sequence <- 0L
  for (adjustment_id in names(adjustments)) {
    for (construction_id in names(constructions)) {
      sequence <- sequence + 1L
      adjustment <- adjustments[[adjustment_id]]
      construction <- constructions[[construction_id]]
      rows[[sequence]] <- data.frame(
        specification_id = paste(adjustment_id, construction_id, sep = "__"),
        adjustment_id = adjustment_id,
        adjustment = adjustment$label,
        construction_id = construction_id,
        construction = construction$label,
        fixed_effect = adjustment$fixed_effect,
        controls = I(list(order_first_stage_controls(adjustment$controls))),
        included_language_controls = I(list(construction$included)),
        excluded_instruments = I(list(construction$excluded)),
        sequence = sequence,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

alternative_distance_variables <- function() {
  constructions <- alternative_distance_constructions()
  unique(c(
    unlist(lapply(constructions, `[[`, "excluded"), use.names = FALSE),
    unlist(lapply(constructions, `[[`, "included"), use.names = FALSE),
    "ling_mapped_speaker_share"
  ))
}

clustered_joint_wald_test <- function(fit, terms, cluster) {
  inference <- tryCatch(iv_clustered_inference(fit, cluster), error = function(e) NULL)
  if (is.null(inference) || is.null(inference$vcov) || !requireNamespace("car", quietly = TRUE)) {
    return(c(statistic = NA_real_, p.value = NA_real_, df = length(terms)))
  }
  hypotheses <- paste0(terms, " = 0")
  test <- tryCatch(
    car::linearHypothesis(fit, hypotheses, vcov. = inference$vcov, test = "F"),
    error = function(e) NULL
  )
  if (is.null(test) || nrow(test) < 2L || !all(c("F", "Pr(>F)") %in% names(test))) {
    return(c(statistic = NA_real_, p.value = NA_real_, df = length(terms)))
  }
  c(
    statistic = suppressWarnings(as.numeric(test[["F"]][[2]])),
    p.value = suppressWarnings(as.numeric(test[["Pr(>F)"]][[2]])),
    df = length(terms)
  )
}

partial_r_squared_instrument_set <- function(data, treatment, excluded, included, controls, fixed_effect) {
  nuisance <- unique(c(controls, included))
  restricted_rhs <- first_stage_nuisance_terms(nuisance, fixed_effect)
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
  joint <- clustered_joint_wald_test(fit, excluded, data$state_code_2001)
  coefficients <- safe_bind_rows(lapply(excluded, function(term) {
    inference <- clustered_first_stage_inference(fit, term, data$state_code_2001)
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
      anderson_rubin_grid = data.frame(status = character())
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
  for (column in c("controls", "included_language_controls", "excluded_instruments")) {
    registry[[column]] <- vapply(registry[[column]], paste, collapse = ";", FUN.VALUE = character(1))
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
    )
  ))
}

linguistic_mapping_coverage_thresholds <- function() c(0, 90, 95, 99)

estimate_alternative_distance_by_coverage <- function(data, registry, treatment) {
  safe_bind_rows(lapply(linguistic_mapping_coverage_thresholds(), function(threshold) {
    sample <- data[num(data$ling_mapped_speaker_share) >= threshold, , drop = FALSE]
    if (!nrow(sample)) return(NULL)
    rows <- lapply(seq_len(nrow(registry)), function(i) {
      estimate_alternative_distance_spec(sample, registry[i, , drop = FALSE], treatment)$summary
    })
    out <- safe_bind_rows(rows)
    out$minimum_mapped_share <- threshold
    out$coverage_sample_n <- nrow(sample)
    out
  }))
}

prepare_language_rows_for_decomposition <- function(census_2001_languages, panel) {
  rows <- std(safe_df(census_2001_languages), 2001L)
  needed <- c("state_std", "district_std", "spkr_tot", "canonical_language")
  if (!all(needed %in% names(rows))) stop("C-16 decomposition requires cleaned district-language rows.", call. = FALSE)
  if (!"ling_degrees" %in% names(rows)) rows$ling_degrees <- linguistic_distance_degrees(rows$canonical_language)
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
      canonical_language = plain_chr(rows$canonical_language),
      state_code_2001 = plain_chr(rows$state_std)
    ),
    sum,
    na.rm = TRUE
  )
  names(by_language_state)[3] <- "speakers"
  by_language_state$n_districts <- mapply(function(language, state) {
    index <- plain_chr(rows$canonical_language) == language & plain_chr(rows$state_std) == state
    length(unique(rows$district_panel_id[index]))
  }, by_language_state$canonical_language, by_language_state$state_code_2001)
  national <- aggregate(by_language_state$speakers, list(by_language_state$canonical_language), sum)
  names(national) <- c("canonical_language", "national_language_speakers")
  out <- merge(by_language_state, national, by = "canonical_language", all.x = TRUE)
  out$speaker_share_of_distance4 <- 100 * out$speakers / sum(out$speakers)
  out[order(out$speakers, decreasing = TRUE), , drop = FALSE]
}

unmapped_language_decomposition <- function(census_2001_languages, panel) {
  rows <- std(safe_df(census_2001_languages), 2001L)
  if (!"ling_degrees" %in% names(rows)) rows$ling_degrees <- linguistic_distance_degrees(rows$canonical_language)
  rows$district_panel_id <- make_district_key(rows$state_std, rows$district_std, 2001L)
  panel_df <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else as.data.frame(panel)
  rows <- rows[rows$district_panel_id %in% plain_chr(panel_df$district_panel_id) & !is.finite(num(rows$ling_degrees)), , drop = FALSE]
  if (!nrow(rows)) return(data.frame())
  out <- aggregate(
    num(rows$spkr_tot),
    list(canonical_language = plain_chr(rows$canonical_language), state_code_2001 = plain_chr(rows$state_std)),
    sum,
    na.rm = TRUE
  )
  names(out)[3] <- "unmapped_speakers"
  out$n_districts <- mapply(function(language, state) {
    index <- plain_chr(rows$canonical_language) == language & plain_chr(rows$state_std) == state
    length(unique(rows$district_panel_id[index]))
  }, out$canonical_language, out$state_code_2001)
  out$share_of_unmapped_speakers <- 100 * out$unmapped_speakers / sum(out$unmapped_speakers)
  out[order(out$unmapped_speakers, decreasing = TRUE), , drop = FALSE]
}

weak_iv_outcome_registry <- function() {
  data.frame(
    specification_id = c("scalar_nonzero_mean", "distance_shares_all_unmapped", "distance_shares_mapped"),
    excluded = I(list(
      "ling_distance_nonzero_mean",
      linguistic_distance_excluded_instruments("all"),
      linguistic_distance_excluded_instruments("mapped")
    )),
    included = I(list(character(), "ling_unmapped_speaker_share", character())),
    stringsAsFactors = FALSE
  )
}

anderson_rubin_test <- function(data, outcome, treatment, excluded, included, controls, fixed_effect, beta0 = 0) {
  transformed <- ".ar_outcome"
  data[[transformed]] <- num(data[[outcome]]) - beta0 * num(data[[treatment]])
  rhs <- unique(c(excluded, included, controls))
  if (identical(fixed_effect, "region")) rhs <- c(rhs, "factor(region)")
  if (identical(fixed_effect, "state")) rhs <- c(rhs, "factor(state_code_2001)")
  fit <- stats::lm(stats::reformulate(rhs, response = transformed), data = data)
  test <- clustered_joint_wald_test(fit, excluded, data$state_code_2001)
  c(statistic = test[["statistic"]], p.value = test[["p.value"]])
}


anderson_rubin_grid <- function(data, outcome, treatment, excluded, included, controls, fixed_effect, level = 0.95, points = 401L) {
  scale <- stats::sd(num(data[[outcome]])) / stats::sd(num(data[[treatment]]))
  if (!is.finite(scale) || scale <= 0) scale <- 1
  beta <- seq(-10 * scale, 10 * scale, length.out = points)
  rows <- safe_bind_rows(lapply(beta, function(value) {
    test <- anderson_rubin_test(data, outcome, treatment, excluded, included, controls, fixed_effect, value)
    data.frame(beta = value, statistic = test[["statistic"]], p.value = test[["p.value"]], stringsAsFactors = FALSE)
  }))
  rows$accepted <- is.finite(rows$p.value) & rows$p.value >= 1 - level
  rows
}

estimate_weak_iv_outcomes <- function(
  panel,
  outcome = "real_log_consumption_change",
  treatment = "emi_exposure_all_children_0708"
) {
  data <- prepare_alternative_distance_panel(panel, treatment)
  if (!outcome %in% names(data)) stop("Weak-IV outcome panel is missing ", outcome, ".", call. = FALSE)
  data <- data[is.finite(num(data[[outcome]])), , drop = FALSE]
  registry <- weak_iv_outcome_registry()
  controls <- census_2001_absorption_controls()
  estimated <- lapply(seq_len(nrow(registry)), function(i) {
    excluded <- unlist(registry$excluded[[i]], use.names = FALSE)
    included <- unlist(registry$included[[i]], use.names = FALSE)
    rhs_structural <- c(treatment, included, controls, "factor(state_code_2001)")
    rhs_instruments <- c(excluded, included, controls, "factor(state_code_2001)")
    formula <- stats::as.formula(paste(
      outcome, "~", paste(rhs_structural, collapse = " + "), "|", paste(rhs_instruments, collapse = " + ")
    ))
    fit <- ivreg::ivreg(formula, data = data, model = TRUE, x = TRUE, y = TRUE)
    cluster <- iv_model_cluster(fit, data)
    inference <- iv_clustered_inference(fit, cluster)
    ct <- lmtest::coeftest(fit, vcov. = inference$vcov)
    row <- match(treatment, rownames(ct))
    reduced <- stats::lm(stats::reformulate(
      c(excluded, included, controls, "factor(state_code_2001)"), response = outcome
    ), data = data)
    reduced_test <- clustered_joint_wald_test(reduced, excluded, data$state_code_2001)
    ar0 <- anderson_rubin_test(data, outcome, treatment, excluded, included, controls, "state", 0)
    grid <- anderson_rubin_grid(data, outcome, treatment, excluded, included, controls, "state")
    accepted <- grid$beta[grid$accepted]
    summary <- data.frame(
      specification_id = registry$specification_id[[i]],
      estimate_2sls = unname(stats::coef(fit)[treatment]),
      std_error_clustered = ct[row, 2], p_value_clustered = ct[row, 4],
      reduced_form_joint_f = reduced_test[["statistic"]], reduced_form_joint_p = reduced_test[["p.value"]],
      anderson_rubin_f_beta0 = ar0[["statistic"]], anderson_rubin_p_beta0 = ar0[["p.value"]],
      ar_95_lower = if (length(accepted)) min(accepted) else NA_real_,
      ar_95_upper = if (length(accepted)) max(accepted) else NA_real_,
      ar_95_empty = !length(accepted),
      ar_95_left_truncated = length(accepted) && min(accepted) == min(grid$beta),
      ar_95_right_truncated = length(accepted) && max(accepted) == max(grid$beta),
      n = stats::nobs(fit), stringsAsFactors = FALSE
    )
    grid$specification_id <- registry$specification_id[[i]]
    list(summary = summary, grid = grid)
  })
  list(
    summary = safe_bind_rows(lapply(estimated, `[[`, "summary")),
    ar_grid = safe_bind_rows(lapply(estimated, `[[`, "grid"))
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
  languages <- sort(unique(plain_chr(rows$canonical_language)))
  spec <- alternative_distance_registry()
  spec <- spec[spec$adjustment_id == "state_expanded" & spec$construction_id == "distance_shares_all_unmapped", , drop = FALSE]
  safe_bind_rows(lapply(languages, function(language) {
    lang <- rows[plain_chr(rows$canonical_language) == language, , drop = FALSE]
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
  diagnostics
}
