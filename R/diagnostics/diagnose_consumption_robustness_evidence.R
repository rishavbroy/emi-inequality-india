# Cross-family evidence synthesis for registered consumption-IV robustness families.
# Estimation and multiplicity remain owned by the source family; this module only
# normalizes realized evidence for reviewer-facing comparison.

summarize_consumption_robustness_family <- function(
    dynamics,
    specifications,
    family,
    analysis_role) {
  if (!is.list(dynamics) || !"summary" %in% names(dynamics)) {
    stop("Consumption robustness evidence requires a dynamics summary.", call. = FALSE)
  }
  if (!nzchar(family) || !nzchar(analysis_role)) {
    stop("Consumption robustness evidence requires family and analysis_role labels.", call. = FALSE)
  }

  summary <- safe_df(dynamics$summary)
  specs <- as_iv_specifications(specifications)
  required_summary <- c(
    "specification_id", "welfare_specification_id", "welfare_outcome_id",
    "outcome_round", "estimand", "effective_f", "effective_f_critical_value",
    "reduced_form_p_holm_family", "anderson_rubin_p_beta0_holm_family",
    "ar_95_empty", "ar_95_disconnected", "ar_95_left_truncated",
    "ar_95_right_truncated", "n", "multiplicity_family"
  )
  missing_summary <- setdiff(required_summary, names(summary))
  if (length(missing_summary)) {
    stop(
      "Consumption robustness summary lacks evidence fields: ",
      paste(missing_summary, collapse = ", "), call. = FALSE
    )
  }
  required_specs <- c(
    "specification_id", "treatment", "adjustment_id", "construction_id",
    "excluded_instruments"
  )
  missing_specs <- setdiff(required_specs, names(specs))
  if (length(missing_specs)) {
    stop(
      "Consumption robustness specifications lack evidence fields: ",
      paste(missing_specs, collapse = ", "), call. = FALSE
    )
  }
  if (anyDuplicated(summary$specification_id) || anyDuplicated(specs$specification_id)) {
    stop("Consumption robustness evidence requires unique specification IDs.", call. = FALSE)
  }

  spec_frame <- data.frame(
    specification_id = plain_chr(specs$specification_id),
    treatment = plain_chr(specs$treatment),
    adjustment_id = plain_chr(specs$adjustment_id),
    construction_id = plain_chr(specs$construction_id),
    excluded_instruments = vapply(
      specs$excluded_instruments,
      function(x) paste(plain_chr(x), collapse = "+"),
      character(1)
    ),
    stringsAsFactors = FALSE
  )
  out <- merge(summary, spec_frame, by = "specification_id", all.x = TRUE, sort = FALSE)
  out <- out[match(summary$specification_id, out$specification_id), , drop = FALSE]
  if (anyNA(out$treatment) || anyNA(out$adjustment_id) || anyNA(out$construction_id)) {
    stop("Consumption robustness evidence could not match every realized model to its specification.", call. = FALSE)
  }

  effective_f <- num(out$effective_f)
  critical <- num(out$effective_f_critical_value)
  out$family <- family
  out$analysis_role <- analysis_role
  out$first_stage_strong <- is.finite(effective_f) & is.finite(critical) & effective_f >= critical
  out$reduced_form_family_signal <- num(out$reduced_form_p_holm_family) < 0.05
  out$ar_family_signal <- num(out$anderson_rubin_p_beta0_holm_family) < 0.05
  out$ar_95_bounded <-
    !(out$ar_95_empty %in% TRUE) &
    !(out$ar_95_left_truncated %in% TRUE) &
    !(out$ar_95_right_truncated %in% TRUE)

  keep <- c(
    "family", "analysis_role", "multiplicity_family", "specification_id",
    "welfare_specification_id", "welfare_outcome_id", "outcome_round", "estimand",
    "treatment", "adjustment_id", "construction_id", "excluded_instruments",
    "effective_f", "effective_f_critical_value", "first_stage_strong",
    "reduced_form_p_holm_family", "reduced_form_family_signal",
    "anderson_rubin_p_beta0_holm_family", "ar_family_signal",
    "ar_95_bounded", "ar_95_disconnected", "ar_95_left_truncated",
    "ar_95_right_truncated", "n"
  )
  out[keep]
}

build_consumption_robustness_evidence <- function(families) {
  if (!is.list(families) || is.null(names(families)) || any(!nzchar(names(families)))) {
    stop("Consumption robustness evidence families must be a named list.", call. = FALSE)
  }

  grids <- lapply(names(families), function(family) {
    entry <- families[[family]]
    required <- c("dynamics", "specifications", "analysis_role")
    if (!is.list(entry) || !all(required %in% names(entry))) {
      stop(
        "Consumption robustness family `", family,
        "` is missing dynamics, specifications, or analysis_role.", call. = FALSE
      )
    }
    summarize_consumption_robustness_family(
      entry$dynamics, entry$specifications, family, entry$analysis_role
    )
  })
  grid <- safe_bind_rows(grids)

  family_summary <- safe_bind_rows(lapply(names(families), function(family) {
    x <- grid[grid$family == family, , drop = FALSE]
    data.frame(
      family = family,
      analysis_role = unique(x$analysis_role)[[1L]],
      multiplicity_family = unique(x$multiplicity_family)[[1L]],
      n_welfare_templates = length(unique(x$welfare_specification_id)),
      n_models = nrow(x),
      n_strong_first_stage = sum(x$first_stage_strong %in% TRUE),
      max_effective_f = max(num(x$effective_f), na.rm = TRUE),
      n_reduced_form_family_signals = sum(x$reduced_form_family_signal %in% TRUE),
      n_ar_family_signals = sum(x$ar_family_signal %in% TRUE),
      n_bounded_ar_sets = sum(x$ar_95_bounded %in% TRUE),
      n_disconnected_ar_sets = sum(x$ar_95_disconnected %in% TRUE),
      n_grid_truncated_ar_sets = sum(
        x$ar_95_left_truncated %in% TRUE | x$ar_95_right_truncated %in% TRUE
      ),
      min_n = min(num(x$n), na.rm = TRUE),
      max_n = max(num(x$n), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  list(grid = grid, family_summary = family_summary)
}

save_consumption_robustness_evidence <- function(
    x, directory = "outputs/diagnostics/extended/consumption") {
  write_diagnostic_bundle(
    list(
      consumption_robustness_evidence_grid = safe_df(x$grid),
      consumption_robustness_family_summary = safe_df(x$family_summary)
    ),
    directory = directory
  )
}
