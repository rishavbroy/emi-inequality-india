# Canonical single-specification IV estimation helpers shared across outcomes.

estimate_iv_reduced_form_spec <- function(data, specification, cfg = list()) {
  spec <- as_single_iv_specification(specification)
  excluded <- unlist(spec$excluded_instruments[[1L]], use.names = FALSE)
  included <- unlist(spec$included_language_controls[[1L]], use.names = FALSE)
  controls <- unlist(spec$controls[[1L]], use.names = FALSE)
  if (length(excluded) != 1L) {
    stop("Reduced-form estimation currently requires one excluded instrument.", call. = FALSE)
  }

  needed <- iv_specification_variables(spec)
  missing <- setdiff(needed, names(data))
  if (length(missing)) {
    return(data.frame(
      specification_id = spec$specification_id[[1L]],
      term = excluded[[1L]],
      estimate = NA_real_, std.error = NA_real_,
      statistic = NA_real_, p.value = NA_real_, n = NA_integer_,
      status = "not_estimated",
      reason = paste0("Missing columns: ", paste(missing, collapse = ", ")),
      stringsAsFactors = FALSE
    ))
  }

  panel <- as.data.frame(data)
  x <- panel[stats::complete.cases(panel[needed]), , drop = FALSE]
  if (nrow(x) < 3L) {
    return(data.frame(
      specification_id = spec$specification_id[[1L]],
      term = excluded[[1L]],
      estimate = NA_real_, std.error = NA_real_,
      statistic = NA_real_, p.value = NA_real_, n = nrow(x),
      status = "not_estimated", reason = "Insufficient complete observations.",
      stringsAsFactors = FALSE
    ))
  }

  rhs <- unique(c(
    excluded, included, controls,
    iv_fixed_effect_terms(spec$fixed_effect[[1L]])
  ))
  fit <- stats::lm(
    stats::reformulate(rhs, response = spec$outcome[[1L]]),
    data = x
  )
  cluster <- iv_specification_cluster(x, spec)
  inference <- iv_clustered_inference(fit, cluster)
  if (identical(inference$status, "unavailable") && is_final_mode(cfg)) {
    stop(
      "Clustered reduced-form inference is unavailable for ",
      spec$specification_id[[1L]], ": ", inference$reason,
      call. = FALSE
    )
  }
  term <- model_term_inference(fit, excluded[[1L]], inference$vcov)

  data.frame(
    specification_id = spec$specification_id[[1L]],
    term = excluded[[1L]],
    estimate = term[["estimate"]],
    std.error = term[["std.error"]],
    statistic = term[["statistic"]],
    p.value = term[["p.value"]],
    n = stats::nobs(fit),
    status = if (all(is.finite(term[c("estimate", "std.error", "p.value")]))) {
      "estimated"
    } else {
      "inference_unavailable"
    },
    reason = if (identical(inference$status, "unavailable")) {
      inference$reason
    } else {
      NA_character_
    },
    stringsAsFactors = FALSE
  )
}

estimate_weak_iv_specification <- function(
    data, specification, cfg = list(), ar_points = 401L) {
  spec <- as_single_iv_specification(specification)
  treatment <- spec$treatment[[1L]]
  outcome <- spec$outcome[[1L]]
  controls <- unlist(spec$controls[[1L]], use.names = FALSE)
  included <- unlist(spec$included_language_controls[[1L]], use.names = FALSE)
  excluded <- unlist(spec$excluded_instruments[[1L]], use.names = FALSE)
  fixed <- iv_fixed_effect_terms(spec$fixed_effect[[1L]])
  needed <- iv_specification_variables(spec)
  missing <- setdiff(needed, names(data))
  if (length(missing)) {
    stop(
      "Weak-IV specification ", spec$specification_id[[1L]],
      " is missing columns: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  x <- as.data.frame(data)
  x <- x[stats::complete.cases(x[needed]), , drop = FALSE]
  if (!nrow(x)) return(NULL)

  fit <- ivreg::ivreg(
    iv_specification_formula(spec), data = x,
    model = TRUE, x = TRUE, y = TRUE
  )
  effective <- mop_effective_f(fit, x)
  if (!identical(effective$status, "estimated")) {
    stop(
      "Montiel Olea-Pflueger effective F is unavailable for ",
      spec$specification_id[[1L]], ": ", effective$reason,
      call. = FALSE
    )
  }
  cluster <- iv_specification_cluster(x, spec)
  inference <- iv_clustered_inference(fit, cluster)
  if (identical(inference$status, "unavailable")) {
    if (is_final_mode(cfg)) {
      stop(
        "Clustered IV inference is unavailable for ",
        spec$specification_id[[1L]], ": ", inference$reason,
        call. = FALSE
      )
    }
    return(list(
      summary = data.frame(
        specification_id = spec$specification_id[[1L]],
        adjustment_id = spec$adjustment_id[[1L]],
        construction_id = spec$construction_id[[1L]],
        estimate_2sls = NA_real_, std_error_clustered = NA_real_,
        p_value_clustered = NA_real_, effective_f = effective$statistic,
        effective_f_critical_value = effective$critical_value,
        effective_f_p_value = effective$p.value,
        effective_f_df = effective$effective_df,
        reduced_form_joint_f = NA_real_, reduced_form_joint_p = NA_real_,
        anderson_rubin_f_beta0 = NA_real_, anderson_rubin_p_beta0 = NA_real_,
        ar_95_lower = NA_real_, ar_95_upper = NA_real_, ar_95_empty = NA,
        ar_95_n_components = NA_integer_, ar_95_disconnected = NA,
        ar_95_contains_zero = NA, ar_95_grid_accepted_min = NA_real_,
        ar_95_grid_accepted_max = NA_real_, ar_95_left_truncated = NA,
        ar_95_right_truncated = NA, ar_95_components = NA_character_,
        n = nrow(x), status = "inference_unavailable", reason = inference$reason,
        stringsAsFactors = FALSE
      ),
      grid = data.frame(),
      overidentification = data.frame()
    ))
  }
  ct <- lmtest::coeftest(fit, vcov. = inference$vcov)
  row <- match(treatment, rownames(ct))
  if (is.na(row)) {
    stop("Endogenous treatment coefficient is absent from the IV fit.", call. = FALSE)
  }

  reduced <- stats::lm(
    stats::reformulate(unique(c(excluded, included, controls, fixed)), response = outcome),
    data = x
  )
  reduced_test <- clustered_joint_wald_test(reduced, excluded, cluster)
  ar <- estimate_anderson_rubin_spec(x, spec, points = ar_points)
  overidentification <- if (spec$n_excluded_instruments[[1L]] > spec$n_endogenous[[1L]]) {
    result <- ivreg_sargan_diagnostic(fit)
    cbind(
      data.frame(
        specification_id = spec$specification_id[[1L]],
        n_endogenous = spec$n_endogenous[[1L]],
        n_excluded_instruments = spec$n_excluded_instruments[[1L]],
        stringsAsFactors = FALSE
      ),
      result
    )
  } else {
    data.frame(
      specification_id = spec$specification_id[[1L]],
      n_endogenous = spec$n_endogenous[[1L]],
      n_excluded_instruments = spec$n_excluded_instruments[[1L]],
      test = "sargan", status = "not_applicable",
      statistic = NA_real_, df = NA_real_, p.value = NA_real_,
      reason = "Exactly identified.", stringsAsFactors = FALSE
    )
  }

  summary <- cbind(
    data.frame(
      specification_id = spec$specification_id[[1L]],
      adjustment_id = spec$adjustment_id[[1L]],
      construction_id = spec$construction_id[[1L]],
      estimate_2sls = unname(stats::coef(fit)[treatment]),
      std_error_clustered = ct[row, 2],
      p_value_clustered = ct[row, 4],
      effective_f = effective$statistic,
      effective_f_critical_value = effective$critical_value,
      effective_f_p_value = effective$p.value,
      effective_f_df = effective$effective_df,
      reduced_form_joint_f = reduced_test[["statistic"]],
      reduced_form_joint_p = reduced_test[["p.value"]],
      stringsAsFactors = FALSE
    ),
    ar$summary[setdiff(names(ar$summary), "specification_id")]
  )
  list(summary = summary, grid = ar$grid, overidentification = overidentification)
}
