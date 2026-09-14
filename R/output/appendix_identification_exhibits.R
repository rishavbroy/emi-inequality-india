# Final-paper appendix: linguistic distance as an instrumental variable.
#
# The manuscript needs to establish three points rather than reproduce every
# registered diagnostic: geographic absorption destroys first-stage relevance,
# alternative language measures do not restore it, and weak-IV-robust inference
# does not deliver a useful causal EMI estimate. The full diagnostic families
# remain available in machine-readable outputs and are tested in their owning
# analytical modules.

appendix_iv_relevance_summary <- function(alternative_first_stages, historical_first_stage) {
  if (!inherits(alternative_first_stages, "emi_alternative_distance_first_stages")) {
    stop("IV relevance summary requires canonical alternative-distance first stages.", call. = FALSE)
  }
  modern <- safe_df(alternative_first_stages$summary)
  required <- c(
    "adjustment_id", "construction_id", "joint_excluded_f",
    "partial_r_squared", "n"
  )
  missing <- setdiff(required, names(modern))
  if (length(missing)) {
    stop("IV relevance summary is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  registry <- data.frame(
    construction_id = c(
      "nonzero_mean", "top3_legacy", "glottolog_mean",
      "dyen_noncognate", "distant_share", "distance_shares_all"
    ),
    measure = c(
      "Speaker-weighted distance from Hindi",
      "Top-three-language distance from Hindi",
      "Genealogical distance from Hindi",
      "Lexical noncognacy with Hindi",
      "Share speaking languages distant from Hindi",
      "Language-distance composition (5 instruments)"
    ),
    stringsAsFactors = FALSE
  )
  adjustments <- c("unadjusted", "region_main", "state_main")

  rows <- lapply(seq_len(nrow(registry)), function(i) {
    construction <- registry$construction_id[[i]]
    x <- modern[
      plain_chr(modern$construction_id) == construction &
        plain_chr(modern$adjustment_id) %in% adjustments,
      , drop = FALSE
    ]
    if (nrow(x) != length(adjustments) || !setequal(plain_chr(x$adjustment_id), adjustments)) {
      stop("IV relevance summary requires all geographic adjustments for ", construction, ".", call. = FALSE)
    }
    x <- x[match(adjustments, plain_chr(x$adjustment_id)), , drop = FALSE]
    n <- unique(as.integer(x$n))
    if (length(n) != 1L || any(!is.finite(num(x$joint_excluded_f))) ||
        !is.finite(num(x$partial_r_squared[x$adjustment_id == "state_main"])[[1L]])) {
      stop("IV relevance summary requires finite common-support diagnostics for ", construction, ".", call. = FALSE)
    }
    data.frame(
      row_id = construction,
      measure = registry$measure[[i]],
      no_fe_f = num(x$joint_excluded_f[[1L]]),
      region_f = num(x$joint_excluded_f[[2L]]),
      state_f = num(x$joint_excluded_f[[3L]]),
      state_partial_r2 = num(x$partial_r_squared[[3L]]),
      n = n[[1L]],
      stringsAsFactors = FALSE
    )
  })

  if (!is.list(historical_first_stage)) {
    stop("IV relevance summary requires canonical historical first-stage output.", call. = FALSE)
  }
  historical <- safe_df(historical_first_stage$comparison)
  hist_ids <- c("instrument_only", "region_fe_census_controls", "state_fe_census_controls")
  historical <- historical[
    plain_chr(historical$sample) == "preferred_geography" &
      plain_chr(historical$specification_id) %in% hist_ids,
    , drop = FALSE
  ]
  historical <- historical[match(hist_ids, plain_chr(historical$specification_id)), , drop = FALSE]
  hist_required <- c(
    "specification_id", "excluded_instrument_f_1991",
    "partial_r_squared_1991", "n_1991", "status_1991"
  )
  if (length(setdiff(hist_required, names(historical))) || nrow(historical) != 3L ||
      any(is.na(historical$specification_id)) || any(plain_chr(historical$status_1991) != "estimated") ||
      any(!is.finite(num(historical$excluded_instrument_f_1991)))) {
    stop("IV relevance summary requires the preferred historical no-FE, region-FE, and state-FE designs.", call. = FALSE)
  }
  hist_n <- unique(as.integer(historical$n_1991))
  if (length(hist_n) != 1L) {
    stop("Historical first-stage comparison must use common support.", call. = FALSE)
  }
  rows[[length(rows) + 1L]] <- data.frame(
    row_id = "historical_1991",
    measure = "Historical 1991 speaker-weighted distance",
    no_fe_f = num(historical$excluded_instrument_f_1991[[1L]]),
    region_f = num(historical$excluded_instrument_f_1991[[2L]]),
    state_f = num(historical$excluded_instrument_f_1991[[3L]]),
    state_partial_r2 = num(historical$partial_r_squared_1991[[3L]]),
    n = hist_n[[1L]],
    stringsAsFactors = FALSE
  )

  csv <- safe_bind_rows(rows)
  out <- data.frame(
    `Linguistic measure` = csv$measure,
    `No geographic FE F` = sprintf("%.2f", csv$no_fe_f),
    `Region FE F` = sprintf("%.2f", csv$region_f),
    `State FE F` = sprintf("%.2f", csv$state_f),
    `State partial R2` = sprintf("%.3f", csv$state_partial_r2),
    N = formatC(csv$n, format = "d", big.mark = ","),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_iv_weak_inference <- function(
    dynamics, exclusion_sensitivity, robustness_evidence, alternative_inference) {
  if (!is.list(dynamics) || is.null(dynamics$summary)) {
    stop("Weak-IV inference summary requires canonical consumption-IV dynamics.", call. = FALSE)
  }
  x <- safe_df(dynamics$summary)
  wanted <- c("long_2022__change", "long_2023__change")
  x <- x[match(wanted, plain_chr(x$welfare_specification_id)), , drop = FALSE]
  required <- c(
    "welfare_specification_id", "outcome_round", "second_stage_estimate",
    "second_stage_std.error", "second_stage_p.value", "effective_f",
    "anderson_rubin_p_beta0", "ar_95_components", "ar_95_disconnected",
    "ar_95_sign_identified", "n", "status"
  )
  if (length(setdiff(required, names(x))) || nrow(x) != 2L ||
      any(is.na(x$welfare_specification_id)) || any(plain_chr(x$status) != "estimated") ||
      any(!is.finite(num(x$second_stage_estimate))) || any(!is.finite(num(x$effective_f)))) {
    stop("Weak-IV inference summary requires both estimated long-change specifications.", call. = FALSE)
  }

  sensitivity <- safe_df(exclusion_sensitivity$summary)
  sensitivity <- sensitivity[
    plain_chr(sensitivity$calibration_id) == "exact_exclusion" &
      plain_chr(sensitivity$specification_id) %in% paste0("consumption__", wanted),
    , drop = FALSE
  ]
  sensitivity <- sensitivity[match(paste0("consumption__", wanted), plain_chr(sensitivity$specification_id)), , drop = FALSE]
  sens_required <- c(
    "specification_id", "minimum_gamma_for_zero_95",
    "minimum_gamma_share_of_reduced_form_for_zero_95"
  )
  if (length(setdiff(sens_required, names(sensitivity))) || nrow(sensitivity) != 2L ||
      any(is.na(sensitivity$specification_id)) ||
      any(!is.finite(num(sensitivity$minimum_gamma_share_of_reduced_form_for_zero_95)))) {
    stop("Weak-IV inference summary requires exact-exclusion sensitivity for both long changes.", call. = FALSE)
  }

  families <- safe_df(robustness_evidence$family_summary)
  if (!nrow(families) || !all(c("n_models", "n_strong_first_stage") %in% names(families)) ||
      any(as.integer(families$n_strong_first_stage) != 0L)) {
    stop("Weak-IV inference summary requires every registered robustness family to remain weak.", call. = FALSE)
  }

  if (!inherits(alternative_inference, "emi_alternative_distance_first_stages")) {
    stop("Weak-IV inference summary requires registered multi-instrument sensitivity results.", call. = FALSE)
  }
  fas <- safe_df(alternative_inference$falsification_adaptive_summary)
  fas <- fas[
    plain_chr(fas$adjustment_id) == "state_main" &
      plain_chr(fas$construction_id) %in% c(
        "distance_shares_all", "distance_shares_all_unmapped", "distance_shares_mapped"
      ),
    , drop = FALSE
  ]
  if (nrow(fas) != 3L || any(plain_chr(fas$status) != "estimated") ||
      any(!as.logical(fas$fas_contains_zero)) ||
      any(!as.logical(fas$constituent_relevance_caution))) {
    stop("Weak-IV inference summary requires the registered state-FE rich-vector FAS results.", call. = FALSE)
  }

  csv <- data.frame(
    row_id = wanted,
    outcome = c("2022-23 long change", "2023-24 long change"),
    estimate = num(x$second_stage_estimate),
    std.error = num(x$second_stage_std.error),
    p.value = num(x$second_stage_p.value),
    effective_f = num(x$effective_f),
    ar_p_beta0 = num(x$anderson_rubin_p_beta0),
    ar_95_components = plain_chr(x$ar_95_components),
    ar_disconnected = as.logical(x$ar_95_disconnected),
    ar_sign_identified = as.logical(x$ar_95_sign_identified),
    minimum_gamma_for_zero_95 = num(sensitivity$minimum_gamma_for_zero_95),
    minimum_gamma_share_of_reduced_form_for_zero_95 = num(
      sensitivity$minimum_gamma_share_of_reduced_form_for_zero_95
    ),
    n = as.integer(x$n),
    stringsAsFactors = FALSE
  )
  if (any(!csv$ar_disconnected) || any(csv$ar_sign_identified)) {
    stop("Weak-IV inference summary expects disconnected AR sets that do not identify sign.", call. = FALSE)
  }
  out <- data.frame(
    Outcome = csv$outcome,
    `2SLS estimate` = sprintf("%.3f", csv$estimate),
    SE = sprintf("%.3f", csv$std.error),
    `Effective F` = sprintf("%.2f", csv$effective_f),
    `AR p(0)` = sprintf("%.3f", csv$ar_p_beta0),
    `Minimum direct-effect share` = sprintf(
      "%.1f%%", 100 * csv$minimum_gamma_share_of_reduced_form_for_zero_95
    ),
    N = formatC(csv$n, format = "d", big.mark = ","),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

make_appendix_identification_exhibits <- function(
    alternative_distance_first_stage, historical_linguistic_first_stage_robustness,
    alternative_distance_inference, consumption_iv_dynamics,
    consumption_robustness_evidence, consumption_exclusion_sensitivity) {
  list(
    appendix_iv_relevance_summary = appendix_iv_relevance_summary(
      alternative_distance_first_stage,
      historical_linguistic_first_stage_robustness
    ),
    appendix_iv_weak_inference = appendix_iv_weak_inference(
      consumption_iv_dynamics,
      consumption_exclusion_sensitivity,
      consumption_robustness_evidence,
      alternative_distance_inference
    )
  )
}

save_appendix_identification_exhibits <- function(exhibits, cfg) {
  table_names <- c("appendix_iv_relevance_summary", "appendix_iv_weak_inference")
  if (!is.list(exhibits) || !all(table_names %in% names(exhibits))) {
    stop("IV appendix table bundle is incomplete.", call. = FALSE)
  }
  save_appendix_tables(exhibits, table_names, cfg)
}
