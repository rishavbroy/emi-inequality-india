# Predetermined Census-1991 baseline balance diagnostics.

historical_baseline_1991_panel <- function(
    baseline, geography, district_panel, historical_distance = NULL,
    external_historical_distance = NULL,
    treatment = preferred_iv_variables()$treatment) {
  baseline <- validate_census_1991_district_keys(baseline, "SHRUG 1991 baseline")
  required_variables <- c("population_1991", historical_baseline_1991_variables())
  missing <- setdiff(required_variables, names(baseline))
  if (length(missing)) stop("SHRUG 1991 baseline lacks variables: ", paste(missing, collapse = ", "), call. = FALSE)
  required_geography <- c("source_districts", "transition")
  missing <- setdiff(required_geography, names(geography))
  if (length(missing)) stop("Historical baseline geography lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  mapping <- historical_preferred_geography_panel(geography$source_districts, geography$transition)

  panel <- if (inherits(district_panel, "sf")) sf::st_drop_geometry(district_panel) else safe_df(district_panel)
  required_panel <- c("state_code_2001", "district_code_2001", treatment)
  optional_panel <- intersect(
    c("ling_distance_nonzero_mean"),
    names(panel)
  )
  required_panel <- c(required_panel, optional_panel)
  missing <- setdiff(c("state_code_2001", "district_code_2001", treatment), names(panel))
  if (length(missing)) stop("Historical baseline treatment panel lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  panel$state_code_2001 <- pad_admin_code(panel$state_code_2001, 2L)
  panel$district_code_2001 <- pad_admin_code(panel$district_code_2001, 2L)
  if (anyDuplicated(panel[c("state_code_2001", "district_code_2001")])) {
    stop("Historical baseline treatment panel has duplicate Census-2001 district keys.", call. = FALSE)
  }
  treatment_data <- panel[required_panel]
  names(treatment_data)[names(treatment_data) == treatment] <- "emie_exposure"
  treatment_data$emie_exposure <- num(treatment_data$emie_exposure)
  if ("ling_distance_nonzero_mean" %in% names(treatment_data)) {
    names(treatment_data)[names(treatment_data) == "ling_distance_nonzero_mean"] <-
      "ling_distance_nonzero_mean_2001"
    treatment_data$ling_distance_nonzero_mean_2001 <-
      num(treatment_data$ling_distance_nonzero_mean_2001)
  }

  out <- merge(baseline, mapping, by = census_1991_keys(), all.x = TRUE, sort = FALSE)
  out <- merge(
    out, treatment_data,
    by = c("state_code_2001", "district_code_2001"), all.x = TRUE, sort = FALSE
  )
  if (!is.null(historical_distance)) {
    distance <- safe_df(historical_distance)
    required <- c(
      census_1991_keys(), "min_accepted_coverage", "max_distance_bound_width",
      "historical_language_status", "ling_distance_nonzero_mean_1991"
    )
    missing <- setdiff(required, names(distance))
    if (length(missing)) stop("Historical distance for baseline balance lacks: ", paste(missing, collapse = ", "), call. = FALSE)
    distance$state_code_1991 <- pad_admin_code(distance$state_code_1991, 2L)
    distance$district_code_1991 <- pad_admin_code(distance$district_code_1991, 2L)
    if (anyDuplicated(distance[census_1991_keys()])) stop("Historical distance for baseline balance has duplicate district keys.", call. = FALSE)
    threshold <- unique(num(distance$min_accepted_coverage))
    threshold <- threshold[is.finite(threshold)]
    if (length(threshold) != 1L) stop("Historical baseline balance requires one explicit historical-language coverage threshold.", call. = FALSE)
    bound_width <- unique(num(distance$max_distance_bound_width))
    bound_width <- bound_width[is.finite(bound_width)]
    if (length(bound_width) != 1L) stop("Historical baseline balance requires one explicit historical-language distance-bound threshold.", call. = FALSE)
    distance <- distance[c(required)]
    out <- merge(out, distance, by = census_1991_keys(), all.x = TRUE, sort = FALSE)
    out$historical_ld_eligible <- out$historical_language_status %in% "eligible" &
      is.finite(num(out$ling_distance_nonzero_mean_1991))
  }

  if (!is.null(external_historical_distance)) {
    external <- safe_df(external_historical_distance)
    required <- c(
      census_1991_keys(),
      "linguistic_distance_1991_helms_lim"
    )
    missing <- setdiff(required, names(external))
    if (length(missing)) {
      stop(
        "External historical distance for baseline balance lacks: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    external$state_code_1991 <- pad_admin_code(external$state_code_1991, 2L)
    external$district_code_1991 <- pad_admin_code(external$district_code_1991, 2L)
    if (anyDuplicated(external[census_1991_keys()])) {
      stop(
        "External historical distance for baseline balance has duplicate district keys.",
        call. = FALSE
      )
    }
    external <- external[c(required)]
    names(external)[names(external) == "linguistic_distance_1991_helms_lim"] <-
      "ling_distance_helms_lim_1991"
    out <- merge(
      out, external,
      by = census_1991_keys(), all.x = TRUE, sort = FALSE
    )
    out$helms_lim_ld_eligible <-
      is.finite(num(out$ling_distance_helms_lim_1991))
  } else {
    out$helms_lim_ld_eligible <- rep(FALSE, nrow(out))
  }

  rownames(out) <- NULL
  out
}

historical_baseline_predictors <- function(panel) {
  x <- safe_df(panel)
  out <- c(eventual_emie = "emie_exposure")
  if ("ling_distance_nonzero_mean_2001" %in% names(x)) {
    out <- c(out, census_2001_ld = "ling_distance_nonzero_mean_2001")
  }
  if ("ling_distance_helms_lim_1991" %in% names(x)) {
    out <- c(out, helms_lim_ld_1991 = "ling_distance_helms_lim_1991")
  }
  if (all(c(
      "historical_ld_eligible",
      "ling_distance_nonzero_mean_1991"
    ) %in% names(x))) {
    out <- c(out, historical_ld_1991 = "ling_distance_nonzero_mean_1991")
  }
  out
}

historical_weighted_sd <- function(x, weight) {
  x <- num(x)
  weight <- num(weight)
  keep <- is.finite(x) & is.finite(weight) & weight > 0
  if (sum(keep) < 2L || length(unique(x[keep])) < 2L) return(NA_real_)
  sqrt(unname(stats::cov.wt(cbind(x[keep]), wt = weight[keep])$cov[1L, 1L]))
}

historical_weighted_term_inference <- function(
    data, predictor, outcome, weight, state, fixed_effect = c("none", "state")) {
  fixed_effect <- match.arg(fixed_effect)
  x <- safe_df(data)
  needed <- c(predictor, outcome, weight, state)
  missing <- setdiff(needed, names(x))
  if (length(missing)) {
    stop("Historical weighted inference lacks columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  rhs <- c(
    predictor,
    if (fixed_effect == "state") paste0("factor(", state, ")") else character()
  )
  fit <- stats::lm(
    stats::reformulate(rhs, response = outcome),
    data = x,
    weights = num(x[[weight]])
  )
  inference <- clustered_lm_term_inference(fit, predictor, x[[state]])
  estimate <- unname(stats::coef(fit)[[predictor]])
  predictor_sd <- historical_weighted_sd(x[[predictor]], x[[weight]])
  outcome_sd <- historical_weighted_sd(x[[outcome]], x[[weight]])
  standardized <- if (
      is.finite(estimate) && is.finite(predictor_sd) &&
      is.finite(outcome_sd) && outcome_sd > 0) {
    estimate * predictor_sd / outcome_sd
  } else {
    NA_real_
  }

  list(
    estimate = estimate,
    std.error = unname(inference[["std.error"]]),
    p.value = unname(inference[["p.value"]]),
    standardized_effect = standardized,
    n = stats::nobs(fit),
    n_states = length(unique(x[[state]])),
    population_weight = sum(num(x[[weight]]))
  )
}

historical_weighted_joint_inference <- function(
    data, predictor, covariates, weight, state) {
  x <- safe_df(data)
  needed <- unique(c(predictor, covariates, weight, state))
  missing <- setdiff(needed, names(x))
  if (length(missing)) {
    stop("Historical joint inference lacks columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  fit <- stats::lm(
    stats::reformulate(
      c(covariates, paste0("factor(", state, ")")),
      response = predictor
    ),
    data = x,
    weights = num(x[[weight]])
  )
  joint <- clustered_joint_wald_test(fit, covariates, x[[state]])
  estimability <- joint_wald_estimability(fit, covariates, joint)
  estimated <- identical(unname(estimability[["status"]]), "estimated")

  list(
    joint_f = if (estimated) unname(joint[["statistic"]]) else NA_real_,
    joint_p = if (estimated) unname(joint[["p.value"]]) else NA_real_,
    n = stats::nobs(fit),
    n_states = length(unique(x[[state]])),
    population_weight = sum(num(x[[weight]])),
    status = unname(estimability[["status"]]),
    reason = unname(estimability[["reason"]])
  )
}

historical_baseline_balance_sample <- function(panel, predictor, covariates, exact_only = FALSE) {
  x <- safe_df(panel)
  keep <- x$preferred_language_persistence %in% TRUE & num(x$n_transition_targets) == 1L
  if (identical(predictor, "emie_exposure")) {
    keep <- keep & is.finite(num(x$emie_exposure))
  } else if (identical(predictor, "ling_distance_nonzero_mean_2001")) {
    keep <- keep & is.finite(num(x$ling_distance_nonzero_mean_2001))
  } else if (identical(predictor, "ling_distance_helms_lim_1991")) {
    keep <- keep & x$helms_lim_ld_eligible %in% TRUE
  } else if (identical(predictor, "ling_distance_nonzero_mean_1991")) {
    keep <- keep & x$historical_ld_eligible %in% TRUE
  }
  if (exact_only) keep <- keep & x$exact_language_persistence %in% TRUE
  needed <- unique(c(predictor, covariates, "population_1991", "state_code_1991"))
  missing <- setdiff(needed, names(x))
  if (length(missing)) {
    stop(
      "Historical baseline balance sample lacks columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  keep <- keep & stats::complete.cases(x[needed]) & num(x$population_1991) > 0 & nzchar(plain_chr(x$state_code_1991))
  x[keep, , drop = FALSE]
}

estimate_historical_baseline_balance <- function(
    panel, predictor, covariate, fixed_effect = c("none", "state"), exact_only = FALSE) {
  fixed_effect <- match.arg(fixed_effect)
  x <- historical_baseline_balance_sample(panel, predictor, covariate, exact_only)
  sample_name <- if (exact_only) "exact_one_to_one" else "preferred_geography"
  metadata <- historical_baseline_1991_metadata()
  row <- metadata[match(covariate, metadata$variable), , drop = FALSE]
  if (!nrow(x)) {
    return(data.frame(
      sample = sample_name, predictor = predictor, fixed_effect = fixed_effect,
      variable = covariate, domain = row$domain, source = row$source, label = row$label,
      estimate = NA_real_, std.error = NA_real_, p.value = NA_real_, standardized_effect = NA_real_,
      n = 0L, n_states = 0L, population_1991 = 0, status = "not_estimated",
      stringsAsFactors = FALSE
    ))
  }
  inference <- historical_weighted_term_inference(
    x, predictor, covariate, "population_1991", "state_code_1991", fixed_effect
  )
  data.frame(
    sample = sample_name, predictor = predictor, fixed_effect = fixed_effect,
    variable = covariate, domain = row$domain, source = row$source, label = row$label,
    estimate = inference$estimate, std.error = inference$std.error,
    p.value = inference$p.value, standardized_effect = inference$standardized_effect,
    n = inference$n, n_states = inference$n_states,
    population_1991 = inference$population_weight, status = "estimated",
    stringsAsFactors = FALSE
  )
}

estimate_historical_baseline_joint_balance <- function(
    panel, predictor, domain, exact_only = FALSE) {
  metadata <- historical_baseline_1991_metadata()
  variables <- metadata$variable[metadata$domain == domain]
  x <- historical_baseline_balance_sample(panel, predictor, variables, exact_only)
  sample_name <- if (exact_only) "exact_one_to_one" else "preferred_geography"
  if (!nrow(x)) {
    return(data.frame(
      sample = sample_name, predictor = predictor, domain = domain,
      tested_covariates = paste(variables, collapse = ";"), n_tested_covariates = length(variables),
      joint_f = NA_real_, joint_p = NA_real_, n = 0L, n_states = 0L,
      population_1991 = 0, status = "not_estimated", reason = "no_complete_cases",
      stringsAsFactors = FALSE
    ))
  }
  inference <- historical_weighted_joint_inference(
    x, predictor, variables, "population_1991", "state_code_1991"
  )
  data.frame(
    sample = sample_name, predictor = predictor, domain = domain,
    tested_covariates = paste(variables, collapse = ";"), n_tested_covariates = length(variables),
    joint_f = inference$joint_f, joint_p = inference$joint_p,
    n = inference$n, n_states = inference$n_states,
    population_1991 = inference$population_weight,
    status = inference$status, reason = inference$reason,
    stringsAsFactors = FALSE
  )
}

historical_baseline_predictor_coverage <- function(panel) {
  x <- safe_df(panel)
  predictors <- historical_baseline_predictors(x)
  safe_bind_rows(lapply(names(predictors), function(predictor_id) {
    predictor <- unname(predictors[[predictor_id]])
    preferred <- historical_baseline_balance_sample(
      x, predictor, character(), exact_only = FALSE
    )
    exact <- historical_baseline_balance_sample(
      x, predictor, character(), exact_only = TRUE
    )
    safe_bind_rows(list(
      data.frame(
        predictor_id = predictor_id,
        predictor = predictor,
        sample = "preferred_geography",
        n = nrow(preferred),
        n_states = length(unique(preferred$state_code_1991)),
        population_1991 = sum(num(preferred$population_1991)),
        stringsAsFactors = FALSE
      ),
      data.frame(
        predictor_id = predictor_id,
        predictor = predictor,
        sample = "exact_one_to_one",
        n = nrow(exact),
        n_states = length(unique(exact$state_code_1991)),
        population_1991 = sum(num(exact$population_1991)),
        stringsAsFactors = FALSE
      )
    ))
  }))
}

build_historical_baseline_balance_1991 <- function(
    baseline, geography, district_panel, historical_distance = NULL,
    external_historical_distance = NULL,
    treatment = preferred_iv_variables()$treatment) {
  panel <- historical_baseline_1991_panel(
    baseline, geography, district_panel, historical_distance,
    external_historical_distance, treatment
  )
  predictors <- historical_baseline_predictors(panel)
  metadata <- historical_baseline_1991_metadata()
  estimates <- safe_bind_rows(lapply(names(predictors), function(name) {
    predictor <- predictors[[name]]
    safe_bind_rows(lapply(c(FALSE, TRUE), function(exact_only) {
      safe_bind_rows(lapply(c("none", "state"), function(fixed_effect) {
        safe_bind_rows(lapply(metadata$variable, function(variable) {
          out <- estimate_historical_baseline_balance(
            panel, predictor, variable, fixed_effect, exact_only
          )
          out$predictor_id <- name
          out
        }))
      }))
    }))
  }))
  joint <- safe_bind_rows(lapply(names(predictors), function(name) {
    predictor <- predictors[[name]]
    safe_bind_rows(lapply(c(FALSE, TRUE), function(exact_only) {
      safe_bind_rows(lapply(unique(metadata$domain), function(domain) {
        out <- estimate_historical_baseline_joint_balance(panel, predictor, domain, exact_only)
        out$predictor_id <- name
        out
      }))
    }))
  }))
  list(
    panel = panel,
    coverage = summarize_historical_baseline_1991_coverage(baseline),
    predictor_coverage = historical_baseline_predictor_coverage(panel),
    estimates = estimates,
    joint_balance = joint
  )
}

historical_baseline_g2_panel <- function(
    controls, district_panel,
    treatment = preferred_iv_variables()$treatment) {
  x <- safe_df(controls)
  required_controls <- c(
    "state_code_2001", "district_code_2001",
    "geography_spec_id", "source_coverage_threshold",
    "population_1991", historical_baseline_1991_pca_variables()
  )
  missing <- setdiff(required_controls, names(x))
  if (length(missing)) {
    stop(
      "G2 historical baseline controls lack: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  panel <- if (inherits(district_panel, "sf")) {
    sf::st_drop_geometry(district_panel)
  } else {
    safe_df(district_panel)
  }
  required_panel <- c("state_code_2001", "district_code_2001", treatment)
  if ("ling_distance_nonzero_mean" %in% names(panel)) {
    required_panel <- c(required_panel, "ling_distance_nonzero_mean")
  }
  missing <- setdiff(
    c("state_code_2001", "district_code_2001", treatment),
    names(panel)
  )
  if (length(missing)) {
    stop(
      "G2 historical baseline treatment panel lacks: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  panel$state_code_2001 <- pad_admin_code(panel$state_code_2001, 2L)
  panel$district_code_2001 <- pad_admin_code(
    panel$district_code_2001, 2L
  )
  if (anyDuplicated(panel[c("state_code_2001", "district_code_2001")])) {
    stop(
      "G2 historical baseline treatment panel has duplicate Census-2001 keys.",
      call. = FALSE
    )
  }
  predictors <- panel[required_panel]
  names(predictors)[names(predictors) == treatment] <- "emie_exposure"
  if ("ling_distance_nonzero_mean" %in% names(predictors)) {
    names(predictors)[
      names(predictors) == "ling_distance_nonzero_mean"
    ] <- "ling_distance_nonzero_mean_2001"
  }
  merge(
    x, predictors,
    by = c("state_code_2001", "district_code_2001"),
    all.x = TRUE, sort = FALSE
  )
}

historical_baseline_g2_sample <- function(panel, predictor, variables) {
  x <- safe_df(panel)
  needed <- unique(c(
    predictor, variables, "population_1991", "state_code_2001"
  ))
  missing <- setdiff(needed, names(x))
  if (length(missing)) {
    stop(
      "G2 historical baseline sample lacks: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  keep <- stats::complete.cases(x[needed]) &
    num(x$population_1991) > 0 &
    nzchar(plain_chr(x$state_code_2001))
  x[keep, , drop = FALSE]
}

estimate_historical_baseline_g2_balance <- function(
    panel, predictor, covariate,
    fixed_effect = c("none", "state")) {
  fixed_effect <- match.arg(fixed_effect)
  x <- historical_baseline_g2_sample(
    panel, predictor, covariate
  )
  metadata <- historical_baseline_1991_metadata()
  row <- metadata[match(covariate, metadata$variable), , drop = FALSE]
  threshold <- unique(num(panel$source_coverage_threshold))
  if (length(threshold) != 1L) {
    stop(
      "G2 historical baseline estimate requires one coverage threshold.",
      call. = FALSE
    )
  }
  if (!nrow(x)) {
    return(data.frame(
      geography_spec_id = "G2_population_interpolated",
      source_coverage_threshold = threshold,
      predictor = predictor,
      fixed_effect = fixed_effect,
      variable = covariate,
      domain = row$domain,
      source = row$source,
      label = row$label,
      estimate = NA_real_,
      std.error = NA_real_,
      p.value = NA_real_,
      standardized_effect = NA_real_,
      n = 0L,
      n_states = 0L,
      population_1991 = 0,
      status = "not_estimated",
      stringsAsFactors = FALSE
    ))
  }
  inference <- historical_weighted_term_inference(
    x, predictor, covariate,
    "population_1991", "state_code_2001", fixed_effect
  )
  data.frame(
    geography_spec_id = "G2_population_interpolated",
    source_coverage_threshold = threshold,
    predictor = predictor,
    fixed_effect = fixed_effect,
    variable = covariate,
    domain = row$domain,
    source = row$source,
    label = row$label,
    estimate = inference$estimate,
    std.error = inference$std.error,
    p.value = inference$p.value,
    standardized_effect = inference$standardized_effect,
    n = inference$n,
    n_states = inference$n_states,
    population_1991 = inference$population_weight,
    status = "estimated",
    stringsAsFactors = FALSE
  )
}

estimate_historical_baseline_g2_joint_balance <- function(
    panel, predictor, domain) {
  metadata <- historical_baseline_1991_metadata()
  variables <- metadata$variable[
    metadata$source == "PCA91" &
      metadata$domain == domain
  ]
  threshold <- unique(num(panel$source_coverage_threshold))
  if (length(threshold) != 1L) {
    stop(
      "G2 historical joint balance requires one coverage threshold.",
      call. = FALSE
    )
  }
  x <- historical_baseline_g2_sample(panel, predictor, variables)
  if (!nrow(x)) {
    return(data.frame(
      geography_spec_id = "G2_population_interpolated",
      source_coverage_threshold = threshold,
      predictor = predictor,
      domain = domain,
      tested_covariates = paste(variables, collapse = ";"),
      n_tested_covariates = length(variables),
      joint_f = NA_real_,
      joint_p = NA_real_,
      n = 0L,
      n_states = 0L,
      population_1991 = 0,
      status = "not_estimated",
      reason = "no_complete_cases",
      stringsAsFactors = FALSE
    ))
  }
  inference <- historical_weighted_joint_inference(
    x, predictor, variables,
    "population_1991", "state_code_2001"
  )
  data.frame(
    geography_spec_id = "G2_population_interpolated",
    source_coverage_threshold = threshold,
    predictor = predictor,
    domain = domain,
    tested_covariates = paste(variables, collapse = ";"),
    n_tested_covariates = length(variables),
    joint_f = inference$joint_f,
    joint_p = inference$joint_p,
    n = inference$n,
    n_states = inference$n_states,
    population_1991 = inference$population_weight,
    status = inference$status,
    reason = inference$reason,
    stringsAsFactors = FALSE
  )
}

build_historical_baseline_g2_sensitivity <- function(
    pca, population_crosswalk, district_panel,
    coverage_thresholds = c(.90, .95, .99),
    treatment = preferred_iv_variables()$treatment) {
  thresholds <- sort(unique(as.numeric(coverage_thresholds)))
  if (!length(thresholds) || any(!is.finite(thresholds)) ||
      any(thresholds < 0 | thresholds > 1)) {
    stop(
      "G2 historical baseline coverage thresholds must lie in [0, 1].",
      call. = FALSE
    )
  }
  metadata <- historical_baseline_1991_metadata()
  pca_metadata <- metadata[metadata$source == "PCA91", , drop = FALSE]
  domains <- unique(pca_metadata$domain)

  builds <- lapply(thresholds, function(threshold) {
    baseline <- build_population_interpolated_pca_baseline_1991(
      pca, population_crosswalk,
      coverage_threshold = threshold
    )
    panel <- historical_baseline_g2_panel(
      baseline$controls, district_panel, treatment
    )
    predictors <- historical_baseline_predictors(panel)
    estimates <- safe_bind_rows(lapply(names(predictors), function(id) {
      predictor <- predictors[[id]]
      rows <- safe_bind_rows(lapply(
        c("none", "state"),
        function(fixed_effect) {
          safe_bind_rows(lapply(
            pca_metadata$variable,
            function(variable) {
              estimate_historical_baseline_g2_balance(
                panel, predictor, variable, fixed_effect
              )
            }
          ))
        }
      ))
      rows$predictor_id <- id
      rows
    }))
    joint <- safe_bind_rows(lapply(names(predictors), function(id) {
      predictor <- predictors[[id]]
      rows <- safe_bind_rows(lapply(
        domains,
        function(domain) {
          estimate_historical_baseline_g2_joint_balance(
            panel, predictor, domain
          )
        }
      ))
      rows$predictor_id <- id
      rows
    }))
    list(
      controls = baseline$controls,
      coverage = baseline$coverage,
      estimates = estimates,
      joint_balance = joint
    )
  })

  list(
    controls = safe_bind_rows(lapply(builds, `[[`, "controls")),
    coverage = safe_bind_rows(lapply(builds, `[[`, "coverage")),
    estimates = safe_bind_rows(lapply(builds, `[[`, "estimates")),
    joint_balance = safe_bind_rows(
      lapply(builds, `[[`, "joint_balance")
    )
  )
}

historical_baseline_human_domains <- function() {
  metadata <- historical_baseline_1991_metadata()
  unique(metadata$domain[metadata$source == "PCA91"])
}

historical_baseline_common_predictors <- function(
    baseline_balance, g2_sensitivity) {
  baseline_ids <- unique(plain_chr(
    safe_df(baseline_balance$estimates)$predictor_id
  ))
  g2_ids <- unique(plain_chr(
    safe_df(g2_sensitivity$estimates)$predictor_id
  ))
  intersect(baseline_ids, g2_ids)
}

normalize_historical_baseline_geography_estimates <- function(
    baseline_balance, g2_sensitivity) {
  baseline <- safe_df(baseline_balance$estimates)
  g2 <- safe_df(g2_sensitivity$estimates)
  required_baseline <- c(
    "sample", "predictor_id", "predictor", "fixed_effect",
    "variable", "domain", "source", "label",
    "estimate", "std.error", "p.value", "standardized_effect",
    "n", "n_states", "population_1991", "status"
  )
  required_g2 <- c(
    "geography_spec_id", "source_coverage_threshold",
    "predictor_id", "predictor", "fixed_effect",
    "variable", "domain", "source", "label",
    "estimate", "std.error", "p.value", "standardized_effect",
    "n", "n_states", "population_1991", "status"
  )
  missing <- setdiff(required_baseline, names(baseline))
  if (length(missing)) {
    stop(
      "Historical baseline geography comparison lacks baseline estimate fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  missing <- setdiff(required_g2, names(g2))
  if (length(missing)) {
    stop(
      "Historical baseline geography comparison lacks G2 estimate fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  predictors <- historical_baseline_common_predictors(
    baseline_balance, g2_sensitivity
  )
  domains <- historical_baseline_human_domains()
  baseline <- baseline[
    baseline$source == "PCA91" &
      baseline$domain %in% domains &
      baseline$predictor_id %in% predictors &
      baseline$sample %in% c(
        "preferred_geography", "exact_one_to_one"
      ),
    required_baseline,
    drop = FALSE
  ]
  baseline$geography_variant <- ifelse(
    baseline$sample == "exact_one_to_one",
    "G0_exact_only",
    "preferred_historical_geography"
  )
  baseline$source_coverage_threshold <- NA_real_
  baseline$sample <- NULL

  g2 <- g2[
    g2$source == "PCA91" &
      g2$domain %in% domains &
      g2$predictor_id %in% predictors,
    required_g2,
    drop = FALSE
  ]
  g2$geography_variant <- plain_chr(g2$geography_spec_id)
  g2$geography_spec_id <- NULL

  columns <- c(
    "geography_variant", "source_coverage_threshold",
    "predictor_id", "predictor", "fixed_effect",
    "variable", "domain", "source", "label",
    "estimate", "std.error", "p.value", "standardized_effect",
    "n", "n_states", "population_1991", "status"
  )
  out <- safe_bind_rows(list(
    baseline[columns],
    g2[columns]
  ))
  key <- c(
    "geography_variant", "source_coverage_threshold",
    "predictor_id", "fixed_effect", "variable"
  )
  duplicate_key <- data.frame(
    geography_variant = plain_chr(out$geography_variant),
    source_coverage_threshold = ifelse(
      is.na(out$source_coverage_threshold),
      "NA",
      format(
        num(out$source_coverage_threshold),
        digits = 15, trim = TRUE, scientific = FALSE
      )
    ),
    predictor_id = plain_chr(out$predictor_id),
    fixed_effect = plain_chr(out$fixed_effect),
    variable = plain_chr(out$variable),
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(duplicate_key[key])) {
    stop(
      "Historical baseline geography comparison has duplicate estimate specifications.",
      call. = FALSE
    )
  }
  out
}

normalize_historical_baseline_geography_joint <- function(
    baseline_balance, g2_sensitivity) {
  baseline <- safe_df(baseline_balance$joint_balance)
  g2 <- safe_df(g2_sensitivity$joint_balance)
  required_baseline <- c(
    "sample", "predictor_id", "predictor", "domain",
    "tested_covariates", "n_tested_covariates",
    "joint_f", "joint_p", "n", "n_states",
    "population_1991", "status", "reason"
  )
  required_g2 <- c(
    "geography_spec_id", "source_coverage_threshold",
    "predictor_id", "predictor", "domain",
    "tested_covariates", "n_tested_covariates",
    "joint_f", "joint_p", "n", "n_states",
    "population_1991", "status", "reason"
  )
  missing <- setdiff(required_baseline, names(baseline))
  if (length(missing)) {
    stop(
      "Historical baseline geography comparison lacks baseline joint fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  missing <- setdiff(required_g2, names(g2))
  if (length(missing)) {
    stop(
      "Historical baseline geography comparison lacks G2 joint fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  predictors <- historical_baseline_common_predictors(
    baseline_balance, g2_sensitivity
  )
  domains <- historical_baseline_human_domains()
  baseline <- baseline[
    baseline$domain %in% domains &
      baseline$predictor_id %in% predictors &
      baseline$sample %in% c(
        "preferred_geography", "exact_one_to_one"
      ),
    required_baseline,
    drop = FALSE
  ]
  baseline$geography_variant <- ifelse(
    baseline$sample == "exact_one_to_one",
    "G0_exact_only",
    "preferred_historical_geography"
  )
  baseline$source_coverage_threshold <- NA_real_
  baseline$sample <- NULL

  g2 <- g2[
    g2$domain %in% domains &
      g2$predictor_id %in% predictors,
    required_g2,
    drop = FALSE
  ]
  g2$geography_variant <- plain_chr(g2$geography_spec_id)
  g2$geography_spec_id <- NULL

  columns <- c(
    "geography_variant", "source_coverage_threshold",
    "predictor_id", "predictor", "domain",
    "tested_covariates", "n_tested_covariates",
    "joint_f", "joint_p", "n", "n_states",
    "population_1991", "status", "reason"
  )
  out <- safe_bind_rows(list(
    baseline[columns],
    g2[columns]
  ))
  duplicate_key <- data.frame(
    geography_variant = plain_chr(out$geography_variant),
    source_coverage_threshold = ifelse(
      is.na(out$source_coverage_threshold),
      "NA",
      format(
        num(out$source_coverage_threshold),
        digits = 15, trim = TRUE, scientific = FALSE
      )
    ),
    predictor_id = plain_chr(out$predictor_id),
    domain = plain_chr(out$domain),
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(duplicate_key)) {
    stop(
      "Historical baseline geography comparison has duplicate joint specifications.",
      call. = FALSE
    )
  }
  out
}

historical_finite_min <- function(x) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (length(x)) min(x) else NA_real_
}

historical_finite_max <- function(x) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (length(x)) max(x) else NA_real_
}

historical_baseline_target_support <- function(
    baseline_balance, g2_sensitivity) {
  panel <- safe_df(baseline_balance$panel)
  g2 <- safe_df(g2_sensitivity$controls)
  required_panel <- c(
    "state_code_2001", "district_code_2001",
    "population_1991", "preferred_language_persistence",
    "exact_language_persistence", "n_transition_targets"
  )
  missing <- setdiff(required_panel, names(panel))
  if (length(missing)) {
    stop(
      "Historical baseline target support lacks baseline panel fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  required_g2 <- c(
    "state_code_2001", "district_code_2001",
    "population_1991", "geography_spec_id",
    "source_coverage_threshold"
  )
  missing <- setdiff(required_g2, names(g2))
  if (length(missing)) {
    stop(
      "Historical baseline target support lacks G2 control fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  panel$state_code_2001 <- pad_admin_code(panel$state_code_2001, 2L)
  panel$district_code_2001 <- pad_admin_code(
    panel$district_code_2001, 2L
  )
  preferred_keep <- panel$preferred_language_persistence %in% TRUE &
    num(panel$n_transition_targets) == 1L &
    !is.na(panel$state_code_2001) &
    !is.na(panel$district_code_2001) &
    is.finite(num(panel$population_1991)) &
    num(panel$population_1991) > 0
  exact_keep <- preferred_keep &
    panel$exact_language_persistence %in% TRUE

  aggregate_panel <- function(keep, geography_variant) {
    x <- panel[keep, c(
      "state_code_2001", "district_code_2001", "population_1991"
    ), drop = FALSE]
    if (!nrow(x)) return(data.frame())
    x$target_unit_id <- geography_transition_unit_id(
      2001L, x$state_code_2001, x$district_code_2001
    )
    rows <- lapply(split(x, x$target_unit_id), function(part) {
      data.frame(
        geography_variant = geography_variant,
        source_coverage_threshold = NA_real_,
        target_unit_id = part$target_unit_id[[1L]],
        state_code_2001 = part$state_code_2001[[1L]],
        district_code_2001 = part$district_code_2001[[1L]],
        population_1991 = sum(num(part$population_1991)),
        n_source_units = nrow(part),
        stringsAsFactors = FALSE
      )
    })
    safe_bind_rows(rows)
  }

  preferred <- aggregate_panel(
    preferred_keep, "preferred_historical_geography"
  )
  exact <- aggregate_panel(exact_keep, "G0_exact_only")

  g2$state_code_2001 <- pad_admin_code(g2$state_code_2001, 2L)
  g2$district_code_2001 <- pad_admin_code(
    g2$district_code_2001, 2L
  )
  g2 <- g2[
    is.finite(num(g2$population_1991)) &
      num(g2$population_1991) > 0 &
      !is.na(g2$state_code_2001) &
      !is.na(g2$district_code_2001),
    ,
    drop = FALSE
  ]
  if (nrow(g2)) {
    g2$target_unit_id <- geography_transition_unit_id(
      2001L, g2$state_code_2001, g2$district_code_2001
    )
    g2_support <- g2[c(
      "geography_spec_id", "source_coverage_threshold",
      "target_unit_id", "state_code_2001",
      "district_code_2001", "population_1991"
    )]
    names(g2_support)[names(g2_support) == "geography_spec_id"] <-
      "geography_variant"
    g2_support$n_source_units <- NA_integer_
  } else {
    g2_support <- data.frame()
  }

  out <- safe_bind_rows(list(preferred, exact, g2_support))
  key <- data.frame(
    geography_variant = plain_chr(out$geography_variant),
    threshold = ifelse(
      is.na(out$source_coverage_threshold),
      "NA",
      format(
        num(out$source_coverage_threshold),
        digits = 15, trim = TRUE, scientific = FALSE
      )
    ),
    target_unit_id = plain_chr(out$target_unit_id),
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(key)) {
    stop(
      "Historical baseline target support has duplicate target units within a geography variant.",
      call. = FALSE
    )
  }
  out
}

historical_baseline_format_percent <- function(x) {
  x <- num(x)
  out <- rep(NA_character_, length(x))
  finite <- is.finite(x)
  out[finite] <- format(
    100 * x[finite],
    digits = 15,
    trim = TRUE,
    scientific = FALSE,
    drop0trailing = TRUE
  )
  out
}

historical_baseline_support_variant_id <- function(
    geography_variant, source_coverage_threshold) {
  threshold_label <- historical_baseline_format_percent(
    source_coverage_threshold
  )
  ifelse(
    is.na(source_coverage_threshold),
    plain_chr(geography_variant),
    paste0(
      plain_chr(geography_variant),
      "_coverage_",
      threshold_label,
      "pct"
    )
  )
}

historical_baseline_support_variant_levels <- function(target_support) {
  x <- safe_df(target_support)
  required <- c(
    "geography_variant", "source_coverage_threshold"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Historical baseline support ordering lacks: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  variants <- unique(x[required])
  variants$variant_id <- historical_baseline_support_variant_id(
    variants$geography_variant,
    variants$source_coverage_threshold
  )
  preferred_order <- match(
    plain_chr(variants$geography_variant),
    c(
      "preferred_historical_geography",
      "G0_exact_only",
      "G2_population_interpolated"
    )
  )
  preferred_order[is.na(preferred_order)] <- 100L
  threshold_order <- num(variants$source_coverage_threshold)
  threshold_order[!is.finite(threshold_order)] <- -Inf

  variants <- variants[order(
    preferred_order,
    threshold_order,
    plain_chr(variants$variant_id)
  ), , drop = FALSE]
  plain_chr(variants$variant_id)
}

summarize_historical_baseline_target_overlap <- function(target_support) {
  x <- safe_df(target_support)
  required <- c(
    "geography_variant", "source_coverage_threshold",
    "target_unit_id", "state_code_2001", "population_1991"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Historical baseline target-overlap support lacks: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!nrow(x)) return(data.frame())

  x$variant_id <- historical_baseline_support_variant_id(
    x$geography_variant, x$source_coverage_threshold
  )
  variants <- historical_baseline_support_variant_levels(x)
  pairs <- utils::combn(variants, 2L, simplify = FALSE)

  safe_bind_rows(lapply(pairs, function(pair) {
    a <- x[x$variant_id == pair[[1L]], , drop = FALSE]
    b <- x[x$variant_id == pair[[2L]], , drop = FALSE]
    a_ids <- unique(plain_chr(a$target_unit_id))
    b_ids <- unique(plain_chr(b$target_unit_id))
    shared <- intersect(a_ids, b_ids)
    union_ids <- union(a_ids, b_ids)
    a_shared <- a[a$target_unit_id %in% shared, , drop = FALSE]
    b_shared <- b[b$target_unit_id %in% shared, , drop = FALSE]
    data.frame(
      variant_a = pair[[1L]],
      variant_b = pair[[2L]],
      n_targets_a = length(a_ids),
      n_targets_b = length(b_ids),
      n_shared_targets = length(shared),
      n_only_a = length(setdiff(a_ids, b_ids)),
      n_only_b = length(setdiff(b_ids, a_ids)),
      target_jaccard = if (length(union_ids)) {
        length(shared) / length(union_ids)
      } else {
        NA_real_
      },
      n_states_a = length(unique(plain_chr(a$state_code_2001))),
      n_states_b = length(unique(plain_chr(b$state_code_2001))),
      n_shared_states = length(intersect(
        unique(plain_chr(a$state_code_2001)),
        unique(plain_chr(b$state_code_2001))
      )),
      population_1991_a = sum(num(a$population_1991)),
      population_1991_b = sum(num(b$population_1991)),
      shared_target_population_1991_a =
        sum(num(a_shared$population_1991)),
      shared_target_population_1991_b =
        sum(num(b_shared$population_1991)),
      shared_population_share_a =
        sum(num(a_shared$population_1991)) /
          sum(num(a$population_1991)),
      shared_population_share_b =
        sum(num(b_shared$population_1991)) /
          sum(num(b$population_1991)),
      stringsAsFactors = FALSE
    )
  }))
}

summarize_historical_baseline_geography_support <- function(estimates) {
  x <- safe_df(estimates)
  required <- c(
    "geography_variant", "source_coverage_threshold",
    "predictor_id", "fixed_effect", "n", "n_states",
    "population_1991"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Historical baseline geography support lacks: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  group_key <- interaction(
    plain_chr(x$geography_variant),
    ifelse(
      is.na(x$source_coverage_threshold),
      "NA",
      format(
        num(x$source_coverage_threshold),
        digits = 15, trim = TRUE, scientific = FALSE
      )
    ),
    plain_chr(x$predictor_id),
    plain_chr(x$fixed_effect),
    drop = TRUE,
    lex.order = TRUE
  )
  safe_bind_rows(lapply(split(x, group_key), function(part) {
    data.frame(
      geography_variant = part$geography_variant[[1L]],
      source_coverage_threshold =
        part$source_coverage_threshold[[1L]],
      predictor_id = part$predictor_id[[1L]],
      fixed_effect = part$fixed_effect[[1L]],
      n_min = historical_finite_min(part$n),
      n_max = historical_finite_max(part$n),
      n_states_min = historical_finite_min(part$n_states),
      n_states_max = historical_finite_max(part$n_states),
      population_1991_min = historical_finite_min(
        part$population_1991
      ),
      population_1991_max = historical_finite_max(
        part$population_1991
      ),
      stringsAsFactors = FALSE
    )
  }))
}

summarize_historical_baseline_g2_threshold_stability <- function(
    joint_balance) {
  x <- safe_df(joint_balance)
  x <- x[
    x$geography_variant == "G2_population_interpolated",
    ,
    drop = FALSE
  ]
  if (!nrow(x)) return(data.frame())
  required <- c(
    "source_coverage_threshold", "predictor_id", "domain",
    "joint_f", "joint_p", "n", "n_states", "population_1991",
    "status"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "G2 historical baseline threshold stability lacks: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  group_key <- interaction(
    plain_chr(x$predictor_id),
    plain_chr(x$domain),
    drop = TRUE,
    lex.order = TRUE
  )
  safe_bind_rows(lapply(split(x, group_key), function(part) {
    thresholds <- sort(unique(num(part$source_coverage_threshold)))
    estimated <- part$status == "estimated" &
      is.finite(num(part$joint_p))
    p <- num(part$joint_p[estimated])
    f <- num(part$joint_f[estimated])
    n <- num(part$n)
    pop <- num(part$population_1991)
    data.frame(
      predictor_id = part$predictor_id[[1L]],
      domain = part$domain[[1L]],
      n_thresholds = length(thresholds),
      min_threshold = min(thresholds),
      max_threshold = max(thresholds),
      joint_p_min = if (length(p)) min(p) else NA_real_,
      joint_p_max = if (length(p)) max(p) else NA_real_,
      joint_f_min = if (length(f)) min(f) else NA_real_,
      joint_f_max = if (length(f)) max(f) else NA_real_,
      n_min = historical_finite_min(n),
      n_max = historical_finite_max(n),
      population_1991_min = historical_finite_min(pop),
      population_1991_max = historical_finite_max(pop),
      all_status_estimated = all(part$status == "estimated"),
      stringsAsFactors = FALSE
    )
  }))
}

build_historical_baseline_geography_comparison <- function(
    baseline_balance, g2_sensitivity) {
  required_baseline <- c("estimates", "joint_balance")
  missing <- setdiff(required_baseline, names(baseline_balance))
  if (length(missing)) {
    stop(
      "Historical baseline comparison input lacks: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  required_g2 <- c("estimates", "joint_balance")
  missing <- setdiff(required_g2, names(g2_sensitivity))
  if (length(missing)) {
    stop(
      "G2 historical baseline comparison input lacks: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  estimates <- normalize_historical_baseline_geography_estimates(
    baseline_balance, g2_sensitivity
  )
  joint <- normalize_historical_baseline_geography_joint(
    baseline_balance, g2_sensitivity
  )
  target_support <- historical_baseline_target_support(
    baseline_balance, g2_sensitivity
  )
  list(
    estimates = estimates,
    joint_balance = joint,
    support = summarize_historical_baseline_geography_support(
      estimates
    ),
    target_support = target_support,
    target_overlap = summarize_historical_baseline_target_overlap(
      target_support
    ),
    g2_threshold_stability =
      summarize_historical_baseline_g2_threshold_stability(joint)
  )
}

save_historical_baseline_geography_comparison <- function(
    x,
    directory = "outputs/diagnostics/extended/instrument_relevance") {
  required <- c(
    "estimates", "joint_balance", "support",
    "target_support", "target_overlap",
    "g2_threshold_stability"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Historical baseline geography comparison output lacks: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    estimates = file.path(
      directory, "historical_baseline_1991_geography_estimates.csv"
    ),
    joint_balance = file.path(
      directory, "historical_baseline_1991_geography_joint_balance.csv"
    ),
    support = file.path(
      directory, "historical_baseline_1991_geography_support.csv"
    ),
    target_support = file.path(
      directory, "historical_baseline_1991_geography_target_support.csv"
    ),
    target_overlap = file.path(
      directory, "historical_baseline_1991_geography_target_overlap.csv"
    ),
    g2_threshold_stability = file.path(
      directory,
      "historical_baseline_1991_g2_threshold_stability.csv"
    )
  )
  write_diagnostic_csv(x$estimates, paths[["estimates"]])
  write_diagnostic_csv(
    x$joint_balance, paths[["joint_balance"]]
  )
  write_diagnostic_csv(x$support, paths[["support"]])
  write_diagnostic_csv(
    x$target_support, paths[["target_support"]]
  )
  write_diagnostic_csv(
    x$target_overlap, paths[["target_overlap"]]
  )
  write_diagnostic_csv(
    x$g2_threshold_stability,
    paths[["g2_threshold_stability"]]
  )
  unname(paths)
}

save_historical_baseline_g2_sensitivity <- function(
    x,
    directory = "outputs/diagnostics/extended/instrument_relevance") {
  required <- c("controls", "coverage", "estimates", "joint_balance")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "G2 historical baseline output lacks: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  paths <- c(
    controls = file.path(
      directory, "historical_baseline_1991_g2_controls.csv"
    ),
    coverage = file.path(
      directory, "historical_baseline_1991_g2_coverage.csv"
    ),
    estimates = file.path(
      directory, "historical_baseline_1991_g2_balance.csv"
    ),
    joint_balance = file.path(
      directory, "historical_baseline_1991_g2_balance_joint.csv"
    )
  )
  write_diagnostic_csv(x$controls, paths[["controls"]])
  write_diagnostic_csv(x$coverage, paths[["coverage"]])
  write_diagnostic_csv(x$estimates, paths[["estimates"]])
  write_diagnostic_csv(x$joint_balance, paths[["joint_balance"]])
  unname(paths)
}

save_historical_baseline_balance_1991 <- function(
    x, directory = "outputs/diagnostics/extended/instrument_relevance") {
  paths <- c(
    coverage = file.path(directory, "historical_baseline_1991_coverage.csv"),
    predictor_coverage = file.path(
      directory, "historical_baseline_1991_predictor_coverage.csv"
    ),
    estimates = file.path(directory, "historical_baseline_1991_balance.csv"),
    joint_balance = file.path(directory, "historical_baseline_1991_balance_joint.csv")
  )
  write_diagnostic_csv(x$coverage, paths[["coverage"]])
  write_diagnostic_csv(
    x$predictor_coverage, paths[["predictor_coverage"]]
  )
  write_diagnostic_csv(x$estimates, paths[["estimates"]])
  write_diagnostic_csv(x$joint_balance, paths[["joint_balance"]])
  unname(paths)
}
