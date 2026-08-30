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
