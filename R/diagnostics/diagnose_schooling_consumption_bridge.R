# Descriptive schooling-to-consumption bridge.
#
# This family fills the deliberately non-causal D -> Y gap in the empirical
# map. It reuses the registered long-run real-MPCE outcomes and the same
# three-column adjustment ladder used by the district mechanism diagnostics.
# Each schooling margin enters alone; ANCOVA specifications retain their
# registered 2004-05 baseline outcome. For each welfare estimand, all five
# schooling margins and the complete adjustment ladder share one complete-case
# district sample so treatment comparisons are not sample-composition artifacts.

schooling_consumption_bridge_treatment_registry <- function(
    construct_registry = read_analysis_construct_registry()) {
  treatment_id <- c(
    "enrollment", "emi_among_enrolled", "emi_all_children",
    "public_emi_all_children", "private_emi_all_children"
  )
  variables <- c(
    "enrollment_rate_0708",
    "emi_share_enrolled_0708",
    "emi_exposure_all_children_0708",
    "public_emi_exposure_all_children_0708",
    "private_emi_exposure_all_children_0708"
  )
  constructs <- analysis_construct_rows(construct_registry, variables)
  if (any(!constructs$stage %in% c("schooling_access", "schooling_treatment", "institutional_bundle")) ||
      any(!constructs$role %in% c("endogenous_treatment", "descriptive_treatment"))) {
    stop("Schooling-consumption treatments violate the canonical construct-role contract.", call. = FALSE)
  }
  data.frame(
    treatment_id = treatment_id,
    treatment = constructs$variable,
    label = constructs$label,
    stringsAsFactors = FALSE
  )
}

schooling_consumption_bridge_welfare_registry <- function(consumption_registry) {
  x <- safe_df(consumption_registry)
  required <- c(
    "welfare_specification_id", "outcome_id", "outcome_round",
    "baseline_round", "estimand", "analysis_transform", "sample_rule"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Schooling-consumption bridge welfare registry lacks fields: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  ids <- c(
    "long_2022__ancova", "long_2022__change",
    "long_2023__ancova", "long_2023__change"
  )
  out <- x[x$welfare_specification_id %in% ids, required, drop = FALSE]
  out <- out[match(ids, out$welfare_specification_id), , drop = FALSE]
  if (nrow(out) != length(ids) || any(is.na(out$welfare_specification_id)) ||
      anyDuplicated(out$welfare_specification_id)) {
    stop(
      "Schooling-consumption bridge requires the four registered long-run welfare specifications.",
      call. = FALSE
    )
  }
  if (any(out$outcome_id != "real_mean_mpce") ||
      any(out$analysis_transform != "log") ||
      any(!out$estimand %in% c("ancova", "change"))) {
    stop(
      "Schooling-consumption bridge long-run welfare contract changed unexpectedly.",
      call. = FALSE
    )
  }
  rownames(out) <- NULL
  out
}

schooling_consumption_bridge_adjustment_registry <- function(control_registry = NULL) {
  district_mechanism_adjustment_registry(control_registry)
}

schooling_consumption_bridge_specifications <- function(
    consumption_registry, control_registry = NULL) {
  treatments <- schooling_consumption_bridge_treatment_registry()
  welfare <- schooling_consumption_bridge_welfare_registry(consumption_registry)
  adjustments <- schooling_consumption_bridge_adjustment_registry(control_registry)
  rows <- list()
  k <- 0L
  for (i in seq_len(nrow(welfare))) {
    for (j in seq_len(nrow(treatments))) {
      for (a in seq_len(nrow(adjustments))) {
        k <- k + 1L
        specification_id <- paste(
          "schooling_consumption",
          welfare$welfare_specification_id[[i]],
          treatments$treatment_id[[j]],
          adjustments$specification_id[[a]],
          sep = "__"
        )
        rows[[k]] <- data.frame(
          analysis_id = paste("schooling_consumption_bridge", specification_id, sep = "__"),
          specification_id = specification_id,
          welfare_specification_id = welfare$welfare_specification_id[[i]],
          outcome_round = welfare$outcome_round[[i]],
          estimand = welfare$estimand[[i]],
          treatment_id = treatments$treatment_id[[j]],
          treatment = treatments$treatment[[j]],
          adjustment_id = adjustments$specification_id[[a]],
          adjustment_label = adjustments$label[[a]],
          fixed_effect = adjustments$fixed_effect[[a]],
          controls = I(list(adjustments$controls[[a]])),
          sequence = k,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  if (nrow(out) != 60L || anyDuplicated(out$specification_id)) {
    stop("Schooling-consumption bridge specification family must contain exactly 60 unique cells.", call. = FALSE)
  }
  out
}

prepare_schooling_consumption_bridge_sample <- function(
    panel, treatment, welfare_specification, adjustments) {
  x <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else safe_df(panel)
  welfare <- safe_df(welfare_specification)
  if (nrow(welfare) != 1L) {
    stop("Schooling-consumption bridge requires one welfare specification.", call. = FALSE)
  }
  outcome <- consumption_iv_variable_name(
    welfare$welfare_specification_id[[1L]], "outcome"
  )
  baseline <- if (welfare$estimand[[1L]] == "ancova") {
    consumption_iv_variable_name(welfare$welfare_specification_id[[1L]], "baseline")
  } else {
    character()
  }
  controls <- unique(unlist(adjustments$controls, use.names = FALSE))
  needed <- unique(c(
    "target_unit_2001", "state_code_2001", "region",
    treatment, outcome, baseline, controls
  ))
  missing <- setdiff(needed, names(x))
  if (length(missing)) {
    stop(
      "Schooling-consumption bridge panel is missing fields: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  numeric_vars <- unique(c(treatment, outcome, baseline, controls))
  for (variable in numeric_vars) x[[variable]] <- num(x[[variable]])
  x$target_unit_2001 <- plain_chr(x$target_unit_2001)
  x$state_code_2001 <- plain_chr(x$state_code_2001)
  x$region <- plain_chr(x$region)
  keep <- stats::complete.cases(x[needed]) &
    nzchar(x$target_unit_2001) & nzchar(x$state_code_2001) & nzchar(x$region)
  x <- x[keep, needed, drop = FALSE]
  rownames(x) <- NULL
  if (!nrow(x)) stop("No complete schooling-consumption bridge sample is available.", call. = FALSE)
  if (anyDuplicated(x$target_unit_2001)) {
    stop("Schooling-consumption bridge sample is not unique by Census-2001 target.", call. = FALSE)
  }
  x
}

fit_schooling_consumption_bridge_specification <- function(
    sample, treatment, welfare_specification, adjustment) {
  x <- safe_df(sample)
  welfare <- safe_df(welfare_specification)
  adjustment <- as.data.frame(adjustment, stringsAsFactors = FALSE)
  if (nrow(welfare) != 1L || nrow(adjustment) != 1L || !is.list(adjustment$controls)) {
    stop("Schooling-consumption bridge fit requires one welfare and one adjustment row.", call. = FALSE)
  }
  outcome <- consumption_iv_variable_name(
    welfare$welfare_specification_id[[1L]], "outcome"
  )
  controls <- adjustment$controls[[1L]]
  if (welfare$estimand[[1L]] == "ancova") {
    controls <- c(
      controls,
      consumption_iv_variable_name(welfare$welfare_specification_id[[1L]], "baseline")
    )
  }
  rhs <- c(
    treatment,
    controls,
    iv_fixed_effect_terms(adjustment$fixed_effect[[1L]])
  )
  fit <- stats::lm(stats::reformulate(rhs, response = outcome), data = x)
  inference <- clustered_lm_term_inference(fit, treatment, x$state_code_2001)
  coefficient <- unname(stats::coef(fit)[[treatment]])
  data.frame(
    n = stats::nobs(fit),
    n_states = length(unique(x$state_code_2001)),
    estimate_per_percentage_point = coefficient,
    estimate_per_10_percentage_points = 10 * coefficient,
    std_error_state_clustered = unname(inference[["std.error"]]),
    p_value_state_clustered = unname(inference[["p.value"]]),
    status = "estimated",
    stringsAsFactors = FALSE
  )
}

diagnose_schooling_consumption_bridge <- function(
    panel, consumption_registry, control_registry = NULL) {
  treatments <- schooling_consumption_bridge_treatment_registry()
  welfare <- schooling_consumption_bridge_welfare_registry(consumption_registry)
  adjustments <- schooling_consumption_bridge_adjustment_registry(control_registry)
  specifications <- schooling_consumption_bridge_specifications(
    consumption_registry, control_registry
  )

  estimates <- safe_bind_rows(lapply(seq_len(nrow(welfare)), function(i) {
    welfare_row <- welfare[i, , drop = FALSE]
    # One complete-case sample across every registered schooling margin makes
    # treatment comparisons interpretable and avoids silently attributing
    # differences in slopes to different district support. The same sample also
    # contains the union of controls used by the adjustment ladder.
    sample <- prepare_schooling_consumption_bridge_sample(
      panel, treatments$treatment, welfare_row, adjustments
    )
    fitted <- safe_bind_rows(lapply(seq_len(nrow(treatments)), function(j) {
      treatment_row <- treatments[j, , drop = FALSE]
      safe_bind_rows(lapply(seq_len(nrow(adjustments)), function(a) {
        result <- fit_schooling_consumption_bridge_specification(
          sample, treatment_row$treatment[[1L]], welfare_row,
          adjustments[a, , drop = FALSE]
        )
        result$specification_id <- paste(
          "schooling_consumption",
          welfare_row$welfare_specification_id[[1L]],
          treatment_row$treatment_id[[1L]],
          adjustments$specification_id[[a]],
          sep = "__"
        )
        result$welfare_specification_id <- welfare_row$welfare_specification_id[[1L]]
        result$outcome_round <- welfare_row$outcome_round[[1L]]
        result$estimand <- welfare_row$estimand[[1L]]
        result$treatment_id <- treatment_row$treatment_id[[1L]]
        result$treatment <- treatment_row$treatment[[1L]]
        result$adjustment_id <- adjustments$specification_id[[a]]
        result
      }))
    }))
    if (length(unique(fitted$n)) != 1L) {
      stop(
        "Schooling-consumption bridge did not preserve common treatment support for ",
        welfare_row$welfare_specification_id[[1L]], ".",
        call. = FALSE
      )
    }
    fitted
  }))

  estimates$p_value_holm_welfare <- NA_real_
  for (id in unique(estimates$welfare_specification_id)) {
    i <- which(estimates$welfare_specification_id == id)
    estimates$p_value_holm_welfare[i] <- holm_adjust_finite(
      estimates$p_value_state_clustered[i]
    )
  }
  estimates$p_value_holm_family <- holm_adjust_finite(
    estimates$p_value_state_clustered
  )
  estimates <- estimates[
    match(specifications$specification_id, estimates$specification_id),
    , drop = FALSE
  ]
  estimates$analysis_id <- specifications$analysis_id
  rownames(estimates) <- NULL
  if (nrow(estimates) != nrow(specifications) || any(is.na(estimates$specification_id))) {
    stop("Schooling-consumption bridge estimates do not match the registered family.", call. = FALSE)
  }

  list(
    treatments = treatments,
    welfare = welfare,
    specifications = specifications,
    estimates = estimates
  )
}

save_schooling_consumption_bridge <- function(
    diagnostics,
    dir = "outputs/diagnostics/extended/consumption") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  c(
    treatments = write_diagnostic_csv(
      diagnostics$treatments,
      file.path(dir, "schooling_consumption_bridge_treatments.csv")
    ),
    welfare = write_diagnostic_csv(
      diagnostics$welfare,
      file.path(dir, "schooling_consumption_bridge_welfare.csv")
    ),
    specifications = write_diagnostic_csv(
      collapse_diagnostic_list_columns(diagnostics$specifications, "controls"),
      file.path(dir, "schooling_consumption_bridge_specifications.csv")
    ),
    estimates = write_diagnostic_csv(
      diagnostics$estimates,
      file.path(dir, "schooling_consumption_bridge_estimates.csv")
    )
  )
}

# Descriptive conversion-gradient heterogeneity.
#
# This deliberately small family asks whether the 2007-08 schooling margins
# most relevant to the paper are more strongly associated with 2004-05 to
# 2022-23 real-consumption change in districts with different predetermined
# capacities. These are descriptive heterogeneous associations, not causal
# treatment-effect heterogeneity.
schooling_consumption_conversion_moderator_registry <- function(control_registry = NULL) {
  registry <- resolve_census_2001_control_registry(control_registry)
  variables <- c(
    "adult_secondary_plus_share_2001",
    "urban_share_2001",
    "st_share_2001"
  )
  index <- match(variables, registry$variable)
  if (anyNA(index)) {
    stop("Conversion-gradient moderators must be registered Census-2001 controls.", call. = FALSE)
  }
  data.frame(
    modifier_id = c("baseline_human_capital", "urbanization", "st_concentration"),
    modifier = variables,
    label = plain_chr(registry$label[index]),
    stringsAsFactors = FALSE
  )
}

schooling_consumption_conversion_specifications <- function(
    consumption_registry, control_registry = NULL) {
  welfare <- schooling_consumption_bridge_welfare_registry(consumption_registry)
  welfare <- welfare[welfare$welfare_specification_id == "long_2022__change", , drop = FALSE]
  treatments <- schooling_consumption_bridge_treatment_registry()
  treatments <- treatments[
    treatments$treatment_id %in% c("emi_all_children", "private_emi_all_children"),
    , drop = FALSE
  ]
  moderators <- schooling_consumption_conversion_moderator_registry(control_registry)
  grid <- merge(treatments, moderators, by = NULL, sort = FALSE)
  grid$welfare_specification_id <- welfare$welfare_specification_id[[1L]]
  grid$outcome_round <- welfare$outcome_round[[1L]]
  grid$estimand <- welfare$estimand[[1L]]
  grid$adjustment_id <- "state_main"
  grid$specification_id <- paste(
    grid$treatment_id, grid$modifier_id, sep = "__"
  )
  grid$analysis_id <- paste(
    "schooling_consumption_conversion", grid$specification_id, sep = "__"
  )
  grid <- grid[c(
    "analysis_id", "specification_id", "welfare_specification_id", "outcome_round",
    "estimand", "treatment_id", "treatment", "modifier_id", "modifier",
    "adjustment_id"
  )]
  if (nrow(grid) != 6L || anyDuplicated(grid$analysis_id)) {
    stop("Schooling-consumption conversion family must contain six unique cells.", call. = FALSE)
  }
  rownames(grid) <- NULL
  grid
}

fit_schooling_consumption_conversion_specification <- function(
    sample, treatment, welfare_specification, adjustment, modifier) {
  x <- safe_df(sample)
  welfare <- safe_df(welfare_specification)
  adjustment <- as.data.frame(adjustment, stringsAsFactors = FALSE)
  if (nrow(welfare) != 1L || nrow(adjustment) != 1L || !is.list(adjustment$controls)) {
    stop("Conversion-gradient fit requires one welfare and one adjustment row.", call. = FALSE)
  }
  needed <- c(treatment, modifier, "state_code_2001")
  if (!all(needed %in% names(x))) {
    stop("Conversion-gradient sample lacks treatment, modifier, or state identifier.", call. = FALSE)
  }
  modifier_values <- num(x[[modifier]])
  modifier_mean <- mean(modifier_values)
  modifier_sd <- stats::sd(modifier_values)
  if (!is.finite(modifier_sd) || modifier_sd <= 0) {
    stop("Conversion-gradient modifier must vary on the common sample.", call. = FALSE)
  }
  x$.modifier_z <- (modifier_values - modifier_mean) / modifier_sd

  outcome <- consumption_iv_variable_name(
    welfare$welfare_specification_id[[1L]], "outcome"
  )
  controls <- setdiff(adjustment$controls[[1L]], modifier)
  interaction_term <- paste0(treatment, ":.modifier_z")
  rhs <- c(
    paste0(treatment, " * .modifier_z"), controls,
    iv_fixed_effect_terms(adjustment$fixed_effect[[1L]])
  )
  fit <- stats::lm(stats::reformulate(rhs, response = outcome), data = x)
  treatment_inference <- clustered_lm_term_inference(
    fit, treatment, x$state_code_2001
  )
  interaction_inference <- clustered_lm_term_inference(
    fit, interaction_term, x$state_code_2001
  )
  coefficients <- stats::coef(fit)
  data.frame(
    n = stats::nobs(fit),
    n_states = length(unique(x$state_code_2001)),
    modifier_mean = modifier_mean,
    modifier_sd = modifier_sd,
    schooling_slope_at_mean_modifier_per_10pp = 10 * unname(coefficients[[treatment]]),
    schooling_slope_std_error_state_clustered = 10 * unname(treatment_inference[["std.error"]]),
    schooling_slope_p_value_state_clustered = unname(treatment_inference[["p.value"]]),
    interaction_per_10pp_schooling_per_modifier_sd =
      10 * unname(coefficients[[interaction_term]]),
    interaction_std_error_state_clustered =
      10 * unname(interaction_inference[["std.error"]]),
    interaction_p_value_state_clustered = unname(interaction_inference[["p.value"]]),
    status = "estimated",
    stringsAsFactors = FALSE
  )
}

diagnose_schooling_consumption_conversion <- function(
    panel, consumption_registry, control_registry = NULL) {
  specifications <- schooling_consumption_conversion_specifications(
    consumption_registry, control_registry
  )
  welfare_registry <- schooling_consumption_bridge_welfare_registry(consumption_registry)
  welfare <- welfare_registry[
    welfare_registry$welfare_specification_id == "long_2022__change", , drop = FALSE
  ]
  adjustments <- schooling_consumption_bridge_adjustment_registry(control_registry)
  adjustment <- adjustments[adjustments$specification_id == "state_main", , drop = FALSE]
  treatments <- unique(plain_chr(specifications$treatment))
  sample_adjustment <- adjustment
  sample_adjustment$controls[[1L]] <- unique(c(
    adjustment$controls[[1L]], plain_chr(specifications$modifier)
  ))
  sample <- prepare_schooling_consumption_bridge_sample(
    panel, treatments, welfare, sample_adjustment
  )

  estimates <- safe_bind_rows(lapply(seq_len(nrow(specifications)), function(i) {
    spec <- specifications[i, , drop = FALSE]
    result <- fit_schooling_consumption_conversion_specification(
      sample,
      spec$treatment[[1L]],
      welfare,
      adjustment,
      spec$modifier[[1L]]
    )
    result$analysis_id <- spec$analysis_id[[1L]]
    result$specification_id <- spec$specification_id[[1L]]
    result$treatment_id <- spec$treatment_id[[1L]]
    result$treatment <- spec$treatment[[1L]]
    result$modifier_id <- spec$modifier_id[[1L]]
    result$modifier <- spec$modifier[[1L]]
    result
  }))
  if (length(unique(estimates$n)) != 1L) {
    stop("Conversion-gradient family must use one common district sample.", call. = FALSE)
  }
  estimates$interaction_p_value_holm_family <- holm_adjust_finite(
    estimates$interaction_p_value_state_clustered
  )
  estimates <- estimates[
    match(specifications$specification_id, estimates$specification_id), , drop = FALSE
  ]
  rownames(estimates) <- NULL
  list(specifications = specifications, estimates = estimates)
}

save_schooling_consumption_conversion <- function(
    diagnostics,
    dir = "outputs/diagnostics/extended/consumption") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  unname(c(
    specifications = write_diagnostic_csv(
      diagnostics$specifications,
      file.path(dir, "schooling_consumption_conversion_specifications.csv")
    ),
    estimates = write_diagnostic_csv(
      diagnostics$estimates,
      file.path(dir, "schooling_consumption_conversion_estimates.csv")
    )
  ))
}
