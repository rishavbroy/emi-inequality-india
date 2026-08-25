# Fixed-sample diagnostics for the alternative district-consumption outcomes.

consumption_outcome_comparison_controls <- function() {
  # Hold the non-outcome conditioning set fixed across specifications. Baseline
  # consumption is the dependent-variable pretest in ANCOVA, not an additional
  # legacy level control in every model.
  setdiff(legacy_2007_iv_controls(), "consumption_0708")
}

build_consumption_outcome_comparison_formulas <- function() {
  spec <- preferred_iv_variables()
  controls <- consumption_outcome_comparison_controls()
  state_fe <- "factor(state_code_2001)"
  list(
    nominal_log_change = make_iv_formula(
      "log_consumption_difference", spec$treatment, spec$instrument,
      controls = controls, fixed_effects = state_fe
    ),
    real_log_change_preferred = make_iv_formula(
      "real_log_consumption_change", spec$treatment, spec$instrument,
      controls = controls, fixed_effects = state_fe
    ),
    real_ancova = make_iv_formula(
      "log_real_consumption_1718", spec$treatment, spec$instrument,
      controls = c("log_real_consumption_0708", controls), fixed_effects = state_fe
    )
  )
}

consumption_outcome_common_sample <- function(panel, formulas = build_consumption_outcome_comparison_formulas()) {
  data <- as.data.frame(panel)
  required <- unique(unlist(lapply(formulas, all.vars), use.names = FALSE))
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(
      "Consumption-outcome comparison is missing variables: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  keep <- stats::complete.cases(data[required])
  out <- data[keep, , drop = FALSE]
  rownames(out) <- NULL
  if (!nrow(out)) stop("Consumption-outcome comparison has no complete common sample.", call. = FALSE)
  out
}

consumption_outcome_model_rows <- function(models, common_sample) {
  treatment <- preferred_iv_variables()$treatment
  all_terms <- tidy_iv_models(models, common_sample)
  tidy <- all_terms[all_terms$term == treatment, , drop = FALSE]
  model_order <- c("nominal_log_change", "real_log_change_preferred", "real_ancova")
  labels <- c(
    nominal_log_change = "Nominal log change",
    real_log_change_preferred = "Person-weighted real log change",
    real_ancova = "Real endpoint ANCOVA"
  )
  tidy$outcome_specification <- unname(labels[tidy$model])
  tidy$preferred <- tidy$model == "real_log_change_preferred"
  tidy$common_sample_n <- nrow(common_sample)

  baseline <- all_terms[
    all_terms$model == "real_ancova" &
      all_terms$term == "log_real_consumption_0708",
    c("estimate", "std.error", "p.value"), drop = FALSE
  ]
  tidy$ancova_baseline_estimate <- NA_real_
  tidy$ancova_baseline_std_error <- NA_real_
  tidy$ancova_baseline_p_value <- NA_real_
  if (nrow(baseline) == 1L) {
    row <- tidy$model == "real_ancova"
    tidy$ancova_baseline_estimate[row] <- baseline$estimate
    tidy$ancova_baseline_std_error[row] <- baseline$std.error
    tidy$ancova_baseline_p_value[row] <- baseline$p.value
  }

  tidy$model <- factor(tidy$model, levels = model_order)
  tidy <- tidy[order(tidy$model), , drop = FALSE]
  tidy$model <- as.character(tidy$model)
  rownames(tidy) <- NULL
  tidy
}


consumption_outcome_first_stage_rows <- function(models, common_sample, cfg) {
  instrument_name <- preferred_iv_variables()$instrument
  stages <- estimate_first_stage(models, common_sample, cfg)
  model_order <- c("nominal_log_change", "real_log_change_preferred", "real_ancova")
  rows <- safe_bind_rows(lapply(model_order, function(model_name) {
    x <- stages[stages$model == model_name, , drop = FALSE]
    if (!nrow(x)) {
      return(data.frame(
        model = model_name, instrument = instrument_name,
        estimate = NA_real_, std.error = NA_real_, partial_f = NA_real_,
        partial_p = NA_real_, nobs = NA_real_, status = "unavailable",
        reason = "No first-stage result was returned.", stringsAsFactors = FALSE
      ))
    }
    instrument <- x[x$term == instrument_name, , drop = FALSE]
    if (!nrow(instrument)) instrument <- x[1L, , drop = FALSE]
    instrument <- instrument[1L, , drop = FALSE]
    data.frame(
      model = model_name,
      instrument = instrument_name,
      estimate = instrument$estimate,
      std.error = instrument$std.error,
      partial_f = instrument$partial_f,
      partial_p = instrument$partial_p,
      nobs = instrument$nobs,
      status = instrument$status,
      reason = instrument$reason,
      stringsAsFactors = FALSE
    )
  }))
  rows$model <- factor(rows$model, levels = model_order)
  rows <- rows[order(rows$model), , drop = FALSE]
  rows$model <- as.character(rows$model)
  rownames(rows) <- NULL
  rows
}

summarise_consumption_outcome_sample <- function(common_sample) {
  vars <- c(
    nominal_log_change = "log_consumption_difference",
    real_log_change_preferred = "real_log_consumption_change",
    real_ancova_endpoint = "log_real_consumption_1718",
    real_ancova_baseline = "log_real_consumption_0708"
  )
  safe_bind_rows(lapply(names(vars), function(name) {
    x <- num(common_sample[[vars[[name]]]])
    data.frame(
      measure = name,
      n = sum(is.finite(x)),
      mean = mean(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      median = stats::median(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
}

compare_consumption_outcomes <- function(panel, cfg) {
  formulas <- build_consumption_outcome_comparison_formulas()
  common_sample <- consumption_outcome_common_sample(panel, formulas)
  models <- estimate_2sls(common_sample, formulas, cfg)
  list(
    coefficients = consumption_outcome_model_rows(models, common_sample),
    first_stage = consumption_outcome_first_stage_rows(models, common_sample, cfg),
    sample_summary = summarise_consumption_outcome_sample(common_sample),
    common_sample = common_sample,
    models = models,
    formulas = formulas
  )
}

consumption_price_diagnostic_contract <- function(households) {
  x <- safe_df(households)
  period_candidates <- c(subround = ".price_subround", panel = ".price_panel")
  present_periods <- period_candidates[period_candidates %in% names(x)]
  if (length(present_periods) != 1L) {
    stop(
      "Household price diagnostics require exactly one registered price-period field.",
      call. = FALSE
    )
  }

  weight_candidates <- c("survey_weight_price", "survey_weight")
  present_weights <- weight_candidates[weight_candidates %in% names(x)]
  if (!length(present_weights)) {
    stop("Household price diagnostics lack a survey-weight field.", call. = FALSE)
  }

  list(
    period_type = names(present_periods)[[1L]],
    period_col = unname(present_periods[[1L]]),
    weight_col = present_weights[[1L]]
  )
}

summarise_household_price_assignments <- function(households, round_id) {
  x <- safe_df(households)
  contract <- consumption_price_diagnostic_contract(x)
  required <- c(
    ".price_state_code", ".price_sector", contract$period_col,
    contract$weight_col, "price_deflator", "state_rule",
    "temporal_state_source"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Household price diagnostics are missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  period <- x[[contract$period_col]]
  weight <- num(x[[contract$weight_col]])
  if (any(!is.finite(weight) | weight <= 0)) {
    stop("Household price diagnostics require positive finite survey weights.", call. = FALSE)
  }

  x$assignment_type <- ifelse(x$state_rule == "direct", "direct", "fallback_or_inheritance")
  group <- interaction(
    x$.price_state_code, x$.price_sector, period,
    x$assignment_type, x$temporal_state_source,
    drop = TRUE, sep = "\r"
  )
  total_weight <- sum(weight)
  out <- safe_bind_rows(lapply(split(seq_len(nrow(x)), group), function(i) {
    w <- weight[i]
    d <- num(x$price_deflator[i])
    data.frame(
      round_id = as.character(round_id),
      state_code = x$.price_state_code[i[[1]]],
      sector = x$.price_sector[i[[1]]],
      period_type = contract$period_type,
      period_group = as.integer(period[i[[1]]]),
      assignment_type = x$assignment_type[i[[1]]],
      temporal_state_source = x$temporal_state_source[i[[1]]],
      households = length(i),
      survey_weight = sum(w),
      survey_weight_share_pct = 100 * sum(w) / total_weight,
      deflator_min = min(d, na.rm = TRUE),
      deflator_median = stats::median(d, na.rm = TRUE),
      deflator_max = max(d, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  out[order(
    out$round_id, out$state_code, out$sector,
    out$period_type, out$period_group, out$assignment_type
  ), , drop = FALSE]
}

compare_district_consumption_constructions <- function(panel) {
  x <- as.data.frame(panel)
  variables <- c(
    "consumption_0708", "consumption_0708_household_weighted",
    "real_consumption_0708", "real_consumption_0708_household_weighted",
    "consumption_1718", "consumption_1718_household_weighted",
    "real_consumption_1718", "real_consumption_1718_household_weighted",
    "log_consumption_difference", "real_log_consumption_change"
  )
  missing <- setdiff(variables, names(x))
  if (length(missing)) {
    stop("District consumption comparison is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  safe_bind_rows(lapply(variables, function(variable) {
    value <- num(x[[variable]])
    data.frame(
      variable = variable,
      n = sum(is.finite(value)),
      mean = mean(value, na.rm = TRUE),
      sd = stats::sd(value, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      median = stats::median(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
}

save_consumption_price_diagnostics <- function(comparison, price_households, panel) {
  if (!is.list(price_households) || !length(price_households) ||
      is.null(names(price_households)) || any(!nzchar(names(price_households)))) {
    stop("Consumption price diagnostics require a named list of survey household objects.", call. = FALSE)
  }
  dir <- "outputs/diagnostics/extended/consumption"
  unname(unlist(list(
    outcome_coefficients = write_diagnostic_csv(
      comparison$coefficients,
      file.path(dir, "outcome_fixed_sample_coefficients.csv")
    ),
    outcome_first_stage = write_diagnostic_csv(
      comparison$first_stage,
      file.path(dir, "outcome_fixed_sample_first_stage.csv")
    ),
    outcome_sample = write_diagnostic_csv(
      comparison$sample_summary,
      file.path(dir, "outcome_fixed_sample_summary.csv")
    ),
    price_assignments = write_diagnostic_csv(
      safe_bind_rows(lapply(names(price_households), function(round_id) {
        summarise_household_price_assignments(price_households[[round_id]], round_id)
      })),
      file.path(dir, "household_price_assignments.csv")
    ),
    district_constructions = write_diagnostic_csv(
      compare_district_consumption_constructions(panel),
      file.path(dir, "district_consumption_constructions.csv")
    )
  ), use.names = FALSE))
}

read_consumption_welfare_comparisons <- function(path) {
  x <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "comparison_id", "left_round", "right_round", "comparison_family"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Consumption welfare comparison registry is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!nrow(x) || anyDuplicated(x$comparison_id) ||
      any(!nzchar(trimws(plain_chr(x$comparison_id)))) ||
      any(!nzchar(trimws(plain_chr(x$left_round)))) ||
      any(!nzchar(trimws(plain_chr(x$right_round)))) ||
      any(x$left_round == x$right_round)) {
    stop("Consumption welfare comparison registry contains invalid round pairs.", call. = FALSE)
  }
  x
}

consumption_welfare_pair_rows <- function(
    welfare, outcome_registry, comparison) {
  x <- safe_df(welfare)
  outcomes <- safe_df(outcome_registry)
  cmp <- safe_df(comparison)
  if (nrow(cmp) != 1L) {
    stop("A single welfare comparison specification is required.", call. = FALSE)
  }

  required_welfare <- c(
    "district_2001", "round_id", "outcome_id", "estimate",
    "preferred_eligible", "n_households", "n_fsu", "kish_effective_n"
  )
  missing_welfare <- setdiff(required_welfare, names(x))
  if (length(missing_welfare)) {
    stop(
      "Consumption welfare comparison input lacks canonical fields: ",
      paste(missing_welfare, collapse = ", "),
      call. = FALSE
    )
  }
  if (!all(c("outcome_id", "transform") %in% names(outcomes))) {
    stop("Consumption welfare outcome registry lacks comparison metadata.", call. = FALSE)
  }

  left_id <- plain_chr(cmp$left_round[[1L]])
  right_id <- plain_chr(cmp$right_round[[1L]])
  left <- x[x$round_id == left_id, , drop = FALSE]
  right <- x[x$round_id == right_id, , drop = FALSE]
  if (!nrow(left) || !nrow(right)) {
    stop(
      "Consumption welfare comparison references unavailable round(s): ",
      left_id, " / ", right_id,
      call. = FALSE
    )
  }

  common_outcomes <- intersect(
    unique(plain_chr(left$outcome_id)),
    unique(plain_chr(right$outcome_id))
  )
  common_outcomes <- intersect(common_outcomes, plain_chr(outcomes$outcome_id))
  if (!length(common_outcomes)) {
    stop("Consumption welfare comparison has no common registered outcomes.", call. = FALSE)
  }

  safe_bind_rows(lapply(common_outcomes, function(outcome_id) {
    keep <- c(
      "district_2001", "estimate", "preferred_eligible",
      "n_households", "n_fsu", "kish_effective_n"
    )
    a <- left[left$outcome_id == outcome_id, keep, drop = FALSE]
    b <- right[right$outcome_id == outcome_id, keep, drop = FALSE]
    names(a)[-1L] <- paste0(names(a)[-1L], "_left")
    names(b)[-1L] <- paste0(names(b)[-1L], "_right")
    joined <- merge(a, b, by = "district_2001", all = FALSE, sort = FALSE)

    finite <- is.finite(joined$estimate_left) & is.finite(joined$estimate_right)
    preferred <- finite &
      joined$preferred_eligible_left %in% TRUE &
      joined$preferred_eligible_right %in% TRUE
    transform <- plain_chr(outcomes$transform[
      match(outcome_id, plain_chr(outcomes$outcome_id))
    ][[1L]])
    proportional <- rep(NA_real_, nrow(joined))
    positive_level <- finite & identical(transform, "identity") &
      joined$estimate_left > 0
    proportional[positive_level] <- (
      joined$estimate_right[positive_level] -
        joined$estimate_left[positive_level]
    ) / joined$estimate_left[positive_level]

    data.frame(
      comparison_id = plain_chr(cmp$comparison_id[[1L]]),
      comparison_family = plain_chr(cmp$comparison_family[[1L]]),
      left_round = left_id,
      right_round = right_id,
      district_2001 = joined$district_2001,
      outcome_id = outcome_id,
      transform = transform,
      estimate_left = joined$estimate_left,
      estimate_right = joined$estimate_right,
      absolute_change = joined$estimate_right - joined$estimate_left,
      proportional_change = proportional,
      finite_common = finite,
      preferred_common = preferred,
      preferred_eligible_left = joined$preferred_eligible_left,
      preferred_eligible_right = joined$preferred_eligible_right,
      n_households_left = joined$n_households_left,
      n_households_right = joined$n_households_right,
      n_fsu_left = joined$n_fsu_left,
      n_fsu_right = joined$n_fsu_right,
      kish_effective_n_left = joined$kish_effective_n_left,
      kish_effective_n_right = joined$kish_effective_n_right,
      stringsAsFactors = FALSE
    )
  }))
}

build_consumption_welfare_changes <- function(
    welfare, outcome_registry, comparison_registry) {
  comparisons <- safe_df(comparison_registry)
  if (!nrow(comparisons)) {
    stop("Consumption welfare comparison registry is empty.", call. = FALSE)
  }
  out <- safe_bind_rows(lapply(seq_len(nrow(comparisons)), function(i) {
    consumption_welfare_pair_rows(
      welfare, outcome_registry, comparisons[i, , drop = FALSE]
    )
  }))
  out[order(
    out$comparison_id, out$outcome_id, out$district_2001
  ), , drop = FALSE]
}

summarize_consumption_welfare_pair <- function(changes) {
  x <- safe_df(changes)
  required <- c(
    "comparison_id", "comparison_family", "left_round", "right_round",
    "outcome_id", "estimate_left", "estimate_right", "absolute_change",
    "proportional_change", "finite_common", "preferred_common",
    "n_households_left", "n_households_right",
    "kish_effective_n_left", "kish_effective_n_right"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Consumption welfare changes lack comparison fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!nrow(x) || length(unique(x$comparison_id)) != 1L ||
      length(unique(x$outcome_id)) != 1L) {
    stop(
      "A single comparison/outcome welfare-change group is required.",
      call. = FALSE
    )
  }

  finite <- x$finite_common %in% TRUE
  preferred <- x$preferred_common %in% TRUE
  use_preferred <- sum(preferred) >= 3L
  use <- if (use_preferred) preferred else finite

  pearson <- spearman <- median_abs_difference <- median_prop_change <- NA_real_
  if (sum(use) >= 3L) {
    pearson <- stats::cor(
      x$estimate_left[use], x$estimate_right[use], method = "pearson"
    )
    spearman <- stats::cor(
      x$estimate_left[use], x$estimate_right[use], method = "spearman"
    )
    median_abs_difference <- stats::median(abs(x$absolute_change[use]))
    prop <- x$proportional_change[use]
    prop <- prop[is.finite(prop)]
    if (length(prop)) median_prop_change <- stats::median(prop)
  }

  data.frame(
    comparison_id = x$comparison_id[[1L]],
    comparison_family = x$comparison_family[[1L]],
    left_round = x$left_round[[1L]],
    right_round = x$right_round[[1L]],
    outcome_id = x$outcome_id[[1L]],
    common_districts = sum(finite),
    common_preferred_districts = sum(preferred),
    comparison_districts = sum(use),
    comparison_basis = if (use_preferred) {
      "preferred_common_support"
    } else {
      "all_finite_common_support"
    },
    pearson_correlation = pearson,
    spearman_correlation = spearman,
    median_absolute_difference = median_abs_difference,
    median_proportional_change = median_prop_change,
    median_households_left = if (sum(use)) {
      stats::median(x$n_households_left[use])
    } else NA_real_,
    median_households_right = if (sum(use)) {
      stats::median(x$n_households_right[use])
    } else NA_real_,
    median_kish_effective_n_left = if (sum(use)) {
      stats::median(x$kish_effective_n_left[use])
    } else NA_real_,
    median_kish_effective_n_right = if (sum(use)) {
      stats::median(x$kish_effective_n_right[use])
    } else NA_real_,
    status = if (sum(use) < 3L) {
      "insufficient_common_support"
    } else if (use_preferred) {
      "estimated_preferred_common_support"
    } else {
      "estimated_all_common_support"
    },
    stringsAsFactors = FALSE
  )
}

compare_consumption_welfare_pair <- function(
    welfare, outcome_registry, comparison) {
  changes <- consumption_welfare_pair_rows(
    welfare, outcome_registry, comparison
  )
  groups <- split(
    seq_len(nrow(changes)),
    interaction(changes$comparison_id, changes$outcome_id, drop = TRUE)
  )
  safe_bind_rows(lapply(groups, function(i) {
    summarize_consumption_welfare_pair(changes[i, , drop = FALSE])
  }))
}

compare_consumption_welfare <- function(
    welfare, outcome_registry, comparison_registry) {
  changes <- build_consumption_welfare_changes(
    welfare, outcome_registry, comparison_registry
  )
  groups <- split(
    seq_len(nrow(changes)),
    interaction(changes$comparison_id, changes$outcome_id, drop = TRUE)
  )
  safe_bind_rows(lapply(groups, function(i) {
    summarize_consumption_welfare_pair(changes[i, , drop = FALSE])
  }))
}

save_consumption_welfare_changes <- function(
    changes,
    path = "outputs/diagnostics/extended/consumption/consumption_welfare_changes.csv") {
  write_diagnostic_csv(safe_df(changes), path)
}

save_consumption_welfare_comparability <- function(
    comparison,
    path = "outputs/diagnostics/extended/consumption/consumption_welfare_comparability.csv") {
  write_diagnostic_csv(safe_df(comparison), path)
}
