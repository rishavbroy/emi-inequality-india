# Predetermined Census-1991 baseline balance diagnostics.

historical_baseline_1991_panel <- function(
    baseline, geography, district_panel, historical_distance = NULL,
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
  missing <- setdiff(required_panel, names(panel))
  if (length(missing)) stop("Historical baseline treatment panel lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  panel$state_code_2001 <- pad_admin_code(panel$state_code_2001, 2L)
  panel$district_code_2001 <- pad_admin_code(panel$district_code_2001, 2L)
  if (anyDuplicated(panel[c("state_code_2001", "district_code_2001")])) {
    stop("Historical baseline treatment panel has duplicate Census-2001 district keys.", call. = FALSE)
  }
  treatment_data <- panel[required_panel]
  names(treatment_data)[names(treatment_data) == treatment] <- "emie_exposure"
  treatment_data$emie_exposure <- num(treatment_data$emie_exposure)

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
  rownames(out) <- NULL
  out
}

historical_baseline_predictors <- function(panel) {
  out <- c(eventual_emie = "emie_exposure")
  if (all(c("historical_ld_eligible", "ling_distance_nonzero_mean_1991") %in% names(panel))) {
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

historical_baseline_balance_sample <- function(panel, predictor, covariates, exact_only = FALSE) {
  x <- safe_df(panel)
  keep <- x$preferred_language_persistence %in% TRUE & num(x$n_transition_targets) == 1L
  if (identical(predictor, "emie_exposure")) {
    keep <- keep & is.finite(num(x$emie_exposure))
  } else if (identical(predictor, "ling_distance_nonzero_mean_1991")) {
    keep <- keep & x$historical_ld_eligible %in% TRUE
  }
  if (exact_only) keep <- keep & x$exact_language_persistence %in% TRUE
  needed <- unique(c(predictor, covariates, "population_1991", "state_code_1991"))
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
  rhs <- c(predictor, if (fixed_effect == "state") "factor(state_code_1991)" else character())
  fit <- stats::lm(
    stats::reformulate(rhs, response = covariate), data = x, weights = num(x$population_1991)
  )
  inference <- clustered_lm_term_inference(fit, predictor, x$state_code_1991)
  estimate <- unname(stats::coef(fit)[[predictor]])
  predictor_sd <- historical_weighted_sd(x[[predictor]], x$population_1991)
  covariate_sd <- historical_weighted_sd(x[[covariate]], x$population_1991)
  standardized <- if (is.finite(estimate) && is.finite(predictor_sd) && is.finite(covariate_sd) && covariate_sd > 0) {
    estimate * predictor_sd / covariate_sd
  } else {
    NA_real_
  }
  data.frame(
    sample = sample_name, predictor = predictor, fixed_effect = fixed_effect,
    variable = covariate, domain = row$domain, source = row$source, label = row$label,
    estimate = estimate, std.error = unname(inference[["std.error"]]),
    p.value = unname(inference[["p.value"]]), standardized_effect = standardized,
    n = stats::nobs(fit), n_states = length(unique(x$state_code_1991)),
    population_1991 = sum(num(x$population_1991)), status = "estimated",
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
  rhs <- c(variables, "factor(state_code_1991)")
  fit <- stats::lm(
    stats::reformulate(rhs, response = predictor), data = x, weights = num(x$population_1991)
  )
  joint <- clustered_joint_wald_test(fit, variables, x$state_code_1991)
  estimability <- joint_wald_estimability(fit, variables, joint)
  estimated <- identical(unname(estimability[["status"]]), "estimated")
  data.frame(
    sample = sample_name, predictor = predictor, domain = domain,
    tested_covariates = paste(variables, collapse = ";"), n_tested_covariates = length(variables),
    joint_f = if (estimated) unname(joint[["statistic"]]) else NA_real_,
    joint_p = if (estimated) unname(joint[["p.value"]]) else NA_real_,
    n = stats::nobs(fit), n_states = length(unique(x$state_code_1991)),
    population_1991 = sum(num(x$population_1991)),
    status = unname(estimability[["status"]]), reason = unname(estimability[["reason"]]),
    stringsAsFactors = FALSE
  )
}

build_historical_baseline_balance_1991 <- function(
    baseline, geography, district_panel, historical_distance = NULL,
    treatment = preferred_iv_variables()$treatment) {
  panel <- historical_baseline_1991_panel(
    baseline, geography, district_panel, historical_distance, treatment
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
    estimates = estimates,
    joint_balance = joint
  )
}

save_historical_baseline_balance_1991 <- function(
    x, directory = "outputs/diagnostics/extended/instrument_relevance") {
  paths <- c(
    coverage = file.path(directory, "historical_baseline_1991_coverage.csv"),
    estimates = file.path(directory, "historical_baseline_1991_balance.csv"),
    joint_balance = file.path(directory, "historical_baseline_1991_balance_joint.csv")
  )
  write_diagnostic_csv(x$coverage, paths[["coverage"]])
  write_diagnostic_csv(x$estimates, paths[["estimates"]])
  write_diagnostic_csv(x$joint_balance, paths[["joint_balance"]])
  unname(paths)
}
