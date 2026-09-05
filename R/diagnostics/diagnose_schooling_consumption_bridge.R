# Descriptive schooling-to-consumption bridge.
#
# This family fills the deliberately non-causal D -> Y gap in the empirical
# map. It reuses the registered long-run real-MPCE outcomes and the same
# three-column adjustment ladder used by the district mechanism diagnostics.
# Each schooling margin enters alone; ANCOVA specifications retain their
# registered 2004-05 baseline outcome. A treatment/outcome pair uses one
# complete-case sample across all three columns so attenuation is not a sample
# composition artifact.

schooling_consumption_bridge_treatment_registry <- function() {
  data.frame(
    treatment_id = c(
      "enrollment", "emi_among_enrolled", "emi_all_children",
      "public_emi_all_children", "private_emi_all_children"
    ),
    treatment = c(
      "enrollment_rate_0708",
      "emi_share_enrolled_0708",
      "emi_exposure_all_children_0708",
      "public_emi_exposure_all_children_0708",
      "private_emi_exposure_all_children_0708"
    ),
    label = c(
      "Enrollment",
      "English medium among enrolled children",
      "English-medium exposure among all children",
      "Public English-medium exposure among all children",
      "Private English-medium exposure among all children"
    ),
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
        rows[[k]] <- data.frame(
          specification_id = paste(
            "schooling_consumption",
            welfare$welfare_specification_id[[i]],
            treatments$treatment_id[[j]],
            adjustments$specification_id[[a]],
            sep = "__"
          ),
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
    safe_bind_rows(lapply(seq_len(nrow(treatments)), function(j) {
      treatment_row <- treatments[j, , drop = FALSE]
      sample <- prepare_schooling_consumption_bridge_sample(
        panel, treatment_row$treatment[[1L]], welfare_row, adjustments
      )
      fitted <- safe_bind_rows(lapply(seq_len(nrow(adjustments)), function(a) {
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
      if (length(unique(fitted$n)) != 1L) {
        stop(
          "Schooling-consumption bridge adjustment ladder changed sample size for ",
          welfare_row$welfare_specification_id[[1L]], " / ", treatment_row$treatment_id[[1L]], ".",
          call. = FALSE
        )
      }
      fitted
    }))
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
