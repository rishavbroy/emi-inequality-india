# Falsification-adaptive sets for overidentified linear IV specifications.
#
# For one endogenous regressor, Masten and Poirier (2021) show that the
# exclusion-based FAS is the interval spanning the just-identified IV estimands
# obtained by using each excluded instrument in turn while treating all other
# excluded instruments as included controls. The construction is an identified
# set, not a confidence interval. We report clustered uncertainty and conditional
# first-stage strength for each constituent estimate, but we do not screen weak
# constituents out of the FAS: doing so would replace the population relevance
# assumption with a sample-dependent selection rule.

iv_falsification_adaptive_specifications <- function(
    specifications = iv_diagnostic_specification_registry()) {
  specs <- as_iv_specifications(specifications)
  keep <- specs$adjustment_id %in% c("region_main", "state_main") &
    specs$construction_id %in% c(
      "distance_shares_all", "distance_shares_all_unmapped",
      "distance_shares_mapped"
    ) & specs$n_endogenous == 1L & specs$n_excluded_instruments > 1L
  out <- specs[keep, , drop = FALSE]
  expected <- 6L
  if (nrow(out) != expected) {
    stop(
      "Falsification-adaptive-set scope must contain exactly six registered five-share designs; found ",
      nrow(out), ".", call. = FALSE
    )
  }
  out$sequence <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}

iv_fas_component_formula <- function(specification, instrument) {
  spec <- as_single_iv_specification(specification)
  excluded <- plain_chr(unlist(spec$excluded_instruments[[1L]], use.names = FALSE))
  instrument <- plain_chr(instrument)[[1L]]
  if (!instrument %in% excluded) {
    stop("FAS component instrument is not registered as excluded by the IV specification.", call. = FALSE)
  }
  included <- unique(c(
    unlist(spec$included_language_controls[[1L]], use.names = FALSE),
    unlist(spec$controls[[1L]], use.names = FALSE),
    setdiff(excluded, instrument)
  ))
  make_iv_formula(
    spec$outcome[[1L]], spec$treatment[[1L]], instrument,
    controls = included,
    fixed_effects = iv_fixed_effect_terms(spec$fixed_effect[[1L]])
  )
}

estimate_iv_fas_component <- function(data, specification, instrument) {
  spec <- as_single_iv_specification(specification)
  excluded <- plain_chr(unlist(spec$excluded_instruments[[1L]], use.names = FALSE))
  instrument <- plain_chr(instrument)[[1L]]
  other_instruments <- setdiff(excluded, instrument)
  # iv_specification_variables() resolves transformed formula terms such as
  # factor(region) to their underlying data columns. Keep sample construction on
  # that canonical contract instead of treating formula expressions as columns.
  needed <- iv_specification_variables(spec)
  missing <- setdiff(needed, names(data))
  if (length(missing)) {
    stop(
      "FAS specification ", spec$specification_id[[1L]],
      " is missing columns: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  x <- as.data.frame(data)
  x <- x[stats::complete.cases(x[needed]), , drop = FALSE]
  if (nrow(x) < 3L) {
    return(data.frame(
      specification_id = spec$specification_id[[1L]],
      instrument = instrument, estimate = NA_real_, std.error = NA_real_,
      p.value = NA_real_, first_stage_estimate = NA_real_,
      first_stage_std.error = NA_real_, first_stage_f = NA_real_,
      first_stage_p.value = NA_real_, n = nrow(x), status = "not_estimated",
      reason = "Insufficient complete observations.", stringsAsFactors = FALSE
    ))
  }

  formula <- iv_fas_component_formula(spec, instrument)
  fit <- tryCatch(
    ivreg::ivreg(formula, data = x, model = TRUE, x = TRUE, y = TRUE),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(data.frame(
      specification_id = spec$specification_id[[1L]],
      instrument = instrument, estimate = NA_real_, std.error = NA_real_,
      p.value = NA_real_, first_stage_estimate = NA_real_,
      first_stage_std.error = NA_real_, first_stage_f = NA_real_,
      first_stage_p.value = NA_real_, n = nrow(x), status = "not_estimated",
      reason = conditionMessage(fit), stringsAsFactors = FALSE
    ))
  }

  cluster <- iv_specification_cluster(x, spec)
  inference <- iv_clustered_inference(fit, cluster)
  treatment_term <- model_term_inference(
    fit, spec$treatment[[1L]], inference$vcov
  )

  included <- unique(c(
    unlist(spec$included_language_controls[[1L]], use.names = FALSE),
    unlist(spec$controls[[1L]], use.names = FALSE),
    other_instruments,
    iv_fixed_effect_terms(spec$fixed_effect[[1L]])
  ))
  first_stage <- stats::lm(
    stats::reformulate(unique(c(instrument, included)), response = spec$treatment[[1L]]),
    data = x
  )
  first_stage_inference <- iv_clustered_inference(first_stage, cluster)
  first_stage_term <- model_term_inference(
    first_stage, instrument, first_stage_inference$vcov
  )
  first_stage_joint <- clustered_joint_wald_test(
    first_stage, instrument, cluster, inference = first_stage_inference
  )
  first_stage_f <- unname(first_stage_joint[["statistic"]])

  status <- if (
    identical(inference$status, "estimated") &&
      identical(first_stage_inference$status, "estimated") &&
      all(is.finite(treatment_term[c("estimate", "std.error", "p.value")])) &&
      all(is.finite(first_stage_term[c("estimate", "std.error", "p.value")]))
  ) "estimated" else "inference_unavailable"
  reasons <- unique(na.omit(c(
    if (identical(inference$status, "unavailable")) inference$reason else NA_character_,
    if (identical(first_stage_inference$status, "unavailable")) first_stage_inference$reason else NA_character_
  )))

  data.frame(
    specification_id = spec$specification_id[[1L]],
    instrument = instrument,
    estimate = treatment_term[["estimate"]],
    std.error = treatment_term[["std.error"]],
    p.value = treatment_term[["p.value"]],
    first_stage_estimate = first_stage_term[["estimate"]],
    first_stage_std.error = first_stage_term[["std.error"]],
    first_stage_f = first_stage_f,
    first_stage_p.value = unname(first_stage_joint[["p.value"]]),
    n = stats::nobs(fit),
    status = status,
    reason = if (length(reasons)) paste(reasons, collapse = "; ") else NA_character_,
    stringsAsFactors = FALSE
  )
}

estimate_iv_falsification_adaptive_set_spec <- function(data, specification) {
  spec <- as_single_iv_specification(specification)
  excluded <- plain_chr(unlist(spec$excluded_instruments[[1L]], use.names = FALSE))
  if (spec$n_endogenous[[1L]] != 1L || length(excluded) <= 1L) {
    stop("FAS requires one endogenous regressor and multiple excluded instruments.", call. = FALSE)
  }
  components <- safe_bind_rows(lapply(excluded, function(instrument) {
    estimate_iv_fas_component(data, spec, instrument)
  }))
  estimated <- components$status == "estimated" & is.finite(components$estimate)
  complete <- length(estimated) == length(excluded) && all(estimated)

  lower <- if (complete) min(components$estimate) else NA_real_
  upper <- if (complete) max(components$estimate) else NA_real_
  lower_instrument <- if (complete) components$instrument[which.min(components$estimate)] else NA_character_
  upper_instrument <- if (complete) components$instrument[which.max(components$estimate)] else NA_character_
  first_stage_f <- num(components$first_stage_f)

  summary <- data.frame(
    specification_id = spec$specification_id[[1L]],
    adjustment_id = spec$adjustment_id[[1L]],
    construction_id = spec$construction_id[[1L]],
    n_instruments = length(excluded),
    n_components_estimated = sum(estimated),
    fas_lower = lower,
    fas_upper = upper,
    fas_width = if (complete) upper - lower else NA_real_,
    fas_contains_zero = if (complete) lower <= 0 && upper >= 0 else NA,
    lower_endpoint_instrument = lower_instrument,
    upper_endpoint_instrument = upper_instrument,
    min_conditional_first_stage_f = if (all(is.finite(first_stage_f))) min(first_stage_f) else NA_real_,
    max_conditional_first_stage_f = if (all(is.finite(first_stage_f))) max(first_stage_f) else NA_real_,
    n_conditional_first_stage_f_below_10 = sum(is.finite(first_stage_f) & first_stage_f < 10),
    constituent_relevance_caution = if (all(is.finite(first_stage_f))) any(first_stage_f < 10) else NA,
    n = if (nrow(components) && length(unique(components$n)) == 1L) components$n[[1L]] else NA_integer_,
    status = if (complete) "estimated" else "incomplete",
    reason = if (complete) NA_character_ else "At least one just-identified constituent could not be estimated with clustered inference.",
    stringsAsFactors = FALSE
  )
  list(summary = summary, components = components)
}

estimate_iv_falsification_adaptive_sets <- function(
    data,
    specifications = iv_falsification_adaptive_specifications()) {
  specs <- iv_falsification_adaptive_specifications(specifications)
  fits <- lapply(seq_len(nrow(specs)), function(i) {
    estimate_iv_falsification_adaptive_set_spec(data, specs[i, , drop = FALSE])
  })
  list(
    summary = safe_bind_rows(lapply(fits, `[[`, "summary")),
    components = safe_bind_rows(lapply(fits, `[[`, "components")),
    specifications = specs
  )
}

validate_iv_falsification_adaptive_sets <- function(result, specifications) {
  specs <- iv_falsification_adaptive_specifications(specifications)
  summary <- safe_df(result$summary)
  components <- safe_df(result$components)
  if (!setequal(plain_chr(summary$specification_id), plain_chr(specs$specification_id))) {
    stop("FAS summaries do not match the registered six-specification scope.", call. = FALSE)
  }
  expected_components <- sum(specs$n_excluded_instruments)
  if (nrow(components) != expected_components) {
    stop(
      "FAS component count does not match the registered excluded-instrument count; expected ",
      expected_components, ", found ", nrow(components), ".", call. = FALSE
    )
  }
  by_spec <- table(plain_chr(components$specification_id))
  expected <- stats::setNames(specs$n_excluded_instruments, specs$specification_id)
  if (!identical(as.integer(by_spec[names(expected)]), as.integer(expected))) {
    stop("FAS constituent counts differ from the canonical IV specifications.", call. = FALSE)
  }
  result
}
