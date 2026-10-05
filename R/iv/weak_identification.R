# Weak-identification-robust inference shared by IV specifications.

# sample-start: code-mop-effective-f
mop_effective_f <- function(
    model, analysis_data, tau = 0.10, size = 0.05) {
  unavailable <- function(reason) {
    list(
      statistic = NA_real_, critical_value = NA_real_, p.value = NA_real_,
      effective_df = NA_real_, tau = tau, size = size,
      status = "not_estimated", reason = reason
    )
  }

  if (!inherits(model, "ivreg")) {
    return(unavailable("Montiel Olea-Pflueger effective F requires an ivreg model."))
  }
  if (!requireNamespace("momentfit", quietly = TRUE)) {
    return(unavailable("Package 'momentfit' is not installed."))
  }
  if (!is.finite(tau) || tau <= 0 || !is.finite(size) || size <= 0 || size >= 1) {
    return(unavailable("Montiel Olea-Pflueger tau and size must be valid probabilities."))
  }

  endogenous <- suppressWarnings(as.integer(model$endogenous))
  if (length(endogenous) != 1L || !is.finite(endogenous)) {
    return(unavailable("Montiel Olea-Pflueger effective F requires exactly one endogenous regressor."))
  }

  regression_formula <- tryCatch(
    stats::formula(model, component = "regressors"),
    error = function(e) NULL
  )
  instrument_formula <- tryCatch(
    stats::formula(stats::delete.response(stats::terms(model, component = "instruments"))),
    error = function(e) NULL
  )
  if (is.null(regression_formula) || is.null(instrument_formula)) {
    return(unavailable("Could not recover ivreg component formulas for Montiel Olea-Pflueger effective F."))
  }
  needed <- unique(c(all.vars(regression_formula), all.vars(instrument_formula)))
  data <- iv_analysis_frame(analysis_data, needed)
  missing <- setdiff(needed, names(data))
  if (length(missing)) {
    return(unavailable(paste(
      "Missing variables for Montiel Olea-Pflueger effective F:",
      paste(missing, collapse = ", ")
    )))
  }
  moment_data <- data[needed]
  if (any(!stats::complete.cases(moment_data))) {
    return(unavailable("Regressors or instruments used by the fitted IV model contain missing values."))
  }

  stored_cluster <- attr(model, "cluster_state", exact = TRUE)
  cluster <- if (
    length(stored_cluster) == stats::nobs(model) &&
      !anyNA(stored_cluster) && length(unique(stored_cluster)) >= 2L
  ) {
    as.vector(stored_cluster)
  } else {
    iv_model_cluster(model, data)
  }
  covariance <- if (is.null(cluster)) "MDS" else "CL"
  covariance_options <- if (is.null(cluster)) {
    list(type = "HC0")
  } else {
    # MOPtest() uses clustered HC0 moment covariance with its finite-cluster
    # adjustment here. The separately reported first-stage Wald F uses HC1, so the
    # two statistics follow different covariance conventions.
    list(
      cluster = data.frame(cluster = cluster),
      type = "HC0", cadjust = TRUE, multi0 = FALSE
    )
  }

  result <- tryCatch({
    moment_model <- momentfit::momentModel(
      regression_formula,
      instrument_formula,
      data = moment_data,
      vcov = covariance,
      vcovOptions = covariance_options
    )
    momentfit::MOPtest(
      moment_model,
      tau = tau,
      size = size,
      estMethod = "TSLS",
      simplified = TRUE,
      print = FALSE
    )
  }, error = function(e) e)
  if (inherits(result, "error")) return(unavailable(conditionMessage(result)))

  values <- suppressWarnings(as.numeric(result[c("Feff", "critValue", "pvalue", "Keff")]))
  names(values) <- c("statistic", "critical_value", "p.value", "effective_df")
  if (length(values) != 4L || any(!is.finite(values))) {
    return(unavailable("momentfit::MOPtest() did not return finite effective-F statistics."))
  }

  c(
    as.list(values),
    list(tau = tau, size = size, status = "estimated", reason = NA_character_)
  )
}
# sample-end: code-mop-effective-f

# sample-start: code-anderson-rubin
anderson_rubin_test <- function(
  data, outcome, treatment, excluded, included = character(),
  controls = character(), fixed_effect = "none", cluster, beta0 = 0
) {
  transformed <- ".ar_outcome"
  data[[transformed]] <- num(data[[outcome]]) - beta0 * num(data[[treatment]])
  rhs <- unique(c(excluded, included, controls, iv_fixed_effect_terms(fixed_effect)))
  fit <- stats::lm(stats::reformulate(rhs, response = transformed), data = data)
  test <- clustered_joint_wald_test(fit, excluded, cluster)
  c(statistic = test[["statistic"]], p.value = test[["p.value"]])
}

# Weak-IV-robust exclusion sensitivity for one scalar excluded instrument.
# For a candidate structural effect beta, regress Y - beta D on Z and the same
# nuisance terms used by the registered IV specification. The coefficient on Z
# is the direct effect gamma that would reconcile that beta with the data.
# Subtracting any fixed gamma * Z changes that coefficient but not the residuals,
# so the clustered standard error is invariant to gamma. This makes the union of
# Anderson--Rubin confidence sets over gamma in [lower, upper] available without
# a numerical gamma grid.
bounded_exclusion_ar_profile_point <- function(
    data, outcome, treatment, instrument, included = character(),
    controls = character(), fixed_effect = "none", cluster, beta0) {
  transformed <- ".bounded_exclusion_ar_outcome"
  x <- as.data.frame(data)
  x[[transformed]] <- num(x[[outcome]]) - beta0 * num(x[[treatment]])
  rhs <- unique(c(instrument, included, controls, iv_fixed_effect_terms(fixed_effect)))
  fit <- stats::lm(stats::reformulate(rhs, response = transformed), data = x)
  inference <- iv_clustered_inference(fit, cluster)
  term <- model_term_inference(fit, instrument, inference$vcov)
  joint <- clustered_joint_wald_test(fit, instrument, cluster, inference)
  data.frame(
    beta = beta0,
    direct_effect_estimate = term[["estimate"]],
    direct_effect_std.error = term[["std.error"]],
    reference_df = joint[["df_denom"]],
    status = if (
      identical(inference$status, "estimated") &&
        all(is.finite(term[c("estimate", "std.error")])) &&
        is.finite(joint[["df_denom"]]) && joint[["df_denom"]] > 0 &&
        term[["std.error"]] > 0
    ) "estimated" else "inference_unavailable",
    reason = if (identical(inference$status, "unavailable")) inference$reason else NA_character_,
    stringsAsFactors = FALSE
  )
}

bounded_exclusion_ar_profile <- function(
    data, outcome, treatment, excluded, included = character(),
    controls = character(), fixed_effect = "none", cluster, beta_values) {
  excluded <- plain_chr(excluded)
  if (length(excluded) != 1L || is.na(excluded[[1L]]) || !nzchar(excluded[[1L]])) {
    stop("Bounded exclusion sensitivity requires exactly one excluded instrument.", call. = FALSE)
  }
  beta_values <- num(beta_values)
  if (!length(beta_values) || any(!is.finite(beta_values))) {
    stop("Bounded exclusion sensitivity requires finite beta values.", call. = FALSE)
  }
  instrument <- excluded[[1L]]

  safe_bind_rows(lapply(beta_values, function(beta0) {
    bounded_exclusion_ar_profile_point(
      data, outcome, treatment, instrument, included, controls, fixed_effect,
      cluster = cluster, beta0 = beta0
    )
  }))
}

# sample-end: code-anderson-rubin

bounded_exclusion_ar_grid <- function(
    profile, gamma_lower, gamma_upper, level = 0.95) {
  x <- safe_df(profile)
  required <- c(
    "beta", "direct_effect_estimate", "direct_effect_std.error",
    "reference_df", "status"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Bounded exclusion AR profile is missing columns: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  gamma_lower <- num(gamma_lower)[[1L]]
  gamma_upper <- num(gamma_upper)[[1L]]
  if (!is.finite(gamma_lower) || !is.finite(gamma_upper) || gamma_lower > gamma_upper) {
    stop("Bounded exclusion sensitivity requires finite ordered gamma bounds.", call. = FALSE)
  }
  if (!is.finite(level) || level <= 0 || level >= 1) {
    stop("Bounded exclusion sensitivity level must lie strictly between zero and one.", call. = FALSE)
  }

  estimate <- num(x$direct_effect_estimate)
  se <- num(x$direct_effect_std.error)
  df <- num(x$reference_df)
  closest <- pmin(pmax(estimate, gamma_lower), gamma_upper)
  distance <- estimate - closest
  statistic <- ifelse(
    x$status == "estimated" & is.finite(se) & se > 0,
    (distance / se)^2, NA_real_
  )
  p_value <- ifelse(
    is.finite(statistic) & is.finite(df) & df > 0,
    stats::pf(statistic, 1, df, lower.tail = FALSE), NA_real_
  )
  accepted <- is.finite(p_value) & p_value >= 1 - level

  data.frame(
    beta = num(x$beta),
    gamma_lower = gamma_lower,
    gamma_upper = gamma_upper,
    closest_gamma = closest,
    direct_effect_estimate = estimate,
    direct_effect_std.error = se,
    statistic = statistic,
    p.value = p_value,
    accepted = accepted,
    boundary_refined = FALSE,
    stringsAsFactors = FALSE
  )
}

refine_bounded_exclusion_ar_grid <- function(
    grid, inputs, gamma_lower, gamma_upper, level = 0.95) {
  evaluate <- function(beta0) {
    profile <- bounded_exclusion_ar_profile_point(
      inputs$data, inputs$outcome, inputs$treatment, inputs$instrument,
      inputs$included, inputs$controls, inputs$fixed_effect,
      cluster = inputs$cluster, beta0 = beta0
    )
    bounded_exclusion_ar_grid(
      profile, gamma_lower = gamma_lower, gamma_upper = gamma_upper, level = level
    )
  }
  refine_acceptance_grid_boundaries(grid, evaluate, level = level)
}

bounded_exclusion_ar_summary <- function(grid, level = 0.95) {
  x <- safe_df(grid)
  required <- c("beta", "accepted", "gamma_lower", "gamma_upper")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Bounded exclusion AR grid is missing columns: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  components <- anderson_rubin_acceptance_components(x)
  bounded_interval <- nrow(components) == 1L &&
    !components$touches_left_grid_edge[[1L]] &&
    !components$touches_right_grid_edge[[1L]]
  information <- classify_anderson_rubin_information(components)
  zero <- x[abs(num(x$beta)) == min(abs(num(x$beta))), , drop = FALSE]
  if (nrow(zero) != 1L) zero <- zero[1L, , drop = FALSE]

  data.frame(
    gamma_lower = num(x$gamma_lower[[1L]]),
    gamma_upper = num(x$gamma_upper[[1L]]),
    exclusion_ar_p_beta0 = num(zero$p.value[[1L]]),
    exclusion_ar_95_lower = if (bounded_interval) components$lower[[1L]] else NA_real_,
    exclusion_ar_95_upper = if (bounded_interval) components$upper[[1L]] else NA_real_,
    exclusion_ar_95_empty = !nrow(components),
    exclusion_ar_95_n_components = nrow(components),
    exclusion_ar_95_disconnected = nrow(components) > 1L,
    exclusion_ar_95_contains_zero = if (nrow(components)) any(components$contains_zero) else FALSE,
    exclusion_ar_95_grid_accepted_min = if (nrow(components)) min(components$lower) else NA_real_,
    exclusion_ar_95_grid_accepted_max = if (nrow(components)) max(components$upper) else NA_real_,
    exclusion_ar_95_left_truncated = nrow(components) && any(components$touches_left_grid_edge),
    exclusion_ar_95_right_truncated = nrow(components) && any(components$touches_right_grid_edge),
    exclusion_ar_95_components = format_anderson_rubin_components(components),
    exclusion_ar_95_information = information,
    exclusion_ar_95_sign_identified = information %in% c("positive_sign_only", "negative_sign_only"),
    stringsAsFactors = FALSE
  )
}

bounded_exclusion_ar_minimum_gamma_for_zero <- function(profile, level = 0.95) {
  x <- safe_df(profile)
  if (!nrow(x)) return(NA_real_)
  zero <- x[abs(num(x$beta)) == min(abs(num(x$beta))), , drop = FALSE]
  if (nrow(zero) != 1L) zero <- zero[1L, , drop = FALSE]
  estimate <- num(zero$direct_effect_estimate[[1L]])
  se <- num(zero$direct_effect_std.error[[1L]])
  df <- num(zero$reference_df[[1L]])
  if (!all(is.finite(c(estimate, se, df))) || se <= 0 || df <= 0) return(NA_real_)
  critical <- stats::qt((1 + level) / 2, df = df)
  magnitude <- max(abs(estimate) - critical * se, 0)
  sign(estimate) * magnitude
}

normalize_anderson_rubin_acceptance_grid <- function(grid) {
  x <- safe_df(grid)
  required <- c("beta", "accepted")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Anderson-Rubin acceptance grid is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!nrow(x)) {
    return(data.frame(beta = numeric(), accepted = logical()))
  }

  beta <- num(x$beta)
  if (any(!is.finite(beta))) {
    stop("Anderson-Rubin acceptance grid contains non-finite beta values.", call. = FALSE)
  }

  raw <- tolower(trimws(plain_chr(x$accepted)))
  missing_flag <- is.na(raw) | !nzchar(raw) | raw == "na"
  truthy <- raw %in% c("true", "t", "1")
  falsy <- raw %in% c("false", "f", "0")
  invalid <- !(missing_flag | truthy | falsy)
  if (any(invalid)) {
    stop(
      "Anderson-Rubin acceptance grid contains invalid accepted flags.",
      call. = FALSE
    )
  }

  accepted <- truthy
  accepted[missing_flag] <- FALSE
  data.frame(beta = beta, accepted = accepted, stringsAsFactors = FALSE)
}

anderson_rubin_acceptance_components <- function(grid) {
  x <- normalize_anderson_rubin_acceptance_grid(grid)
  if (!nrow(x)) return(data.frame())

  ord <- order(x$beta)
  beta <- x$beta[ord]
  accepted <- x$accepted[ord]
  if (!any(accepted)) return(data.frame())

  run_start <- accepted & c(TRUE, !accepted[-length(accepted)])
  run_id <- cumsum(run_start)
  ids <- unique(run_id[accepted])

  rows <- lapply(seq_along(ids), function(index) {
    positions <- which(accepted & run_id == ids[[index]])
    data.frame(
      component = index,
      lower = min(beta[positions]),
      upper = max(beta[positions]),
      touches_left_grid_edge = min(positions) == 1L,
      touches_right_grid_edge = max(positions) == length(beta),
      contains_zero = min(beta[positions]) <= 0 && max(beta[positions]) >= 0,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

classify_anderson_rubin_information <- function(components) {
  x <- safe_df(components)
  if (!nrow(x)) return("empty_acceptance_set")
  required <- c("lower", "upper", "contains_zero")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Anderson-Rubin components are missing columns for information classification: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  lower <- num(x$lower)
  upper <- num(x$upper)
  if (any(!is.finite(lower)) || any(!is.finite(upper)) || any(lower > upper)) {
    stop("Anderson-Rubin component bounds are invalid.", call. = FALSE)
  }
  contains_zero <- as.logical(plain_chr(x$contains_zero))
  if (any(contains_zero %in% TRUE)) return("zero_included")
  if (all(lower > 0)) return("positive_sign_only")
  if (all(upper < 0)) return("negative_sign_only")
  if (any(upper < 0) && any(lower > 0)) return("zero_excluded_both_signs")
  "zero_excluded_unclassified"
}

format_anderson_rubin_components <- function(components) {
  if (!nrow(components)) return(NA_character_)
  intervals <- sprintf("[%.6g, %.6g]", components$lower, components$upper)
  truncated <- components$touches_left_grid_edge | components$touches_right_grid_edge
  intervals[truncated] <- paste0(intervals[truncated], " [grid-truncated]")
  paste(intervals, collapse = " U ")
}

anderson_rubin_beta_values <- function(data, outcome, treatment, points = 401L) {
  points <- as.integer(points)
  if (!is.finite(points) || points < 3L) {
    stop("Anderson-Rubin beta grids require at least three points.", call. = FALSE)
  }
  scale <- stats::sd(num(data[[outcome]])) / stats::sd(num(data[[treatment]]))
  if (!is.finite(scale) || scale <= 0) scale <- 1
  seq(-10 * scale, 10 * scale, length.out = points)
}

refine_acceptance_grid_boundaries <- function(grid, evaluate, level = 0.95) {
  x <- safe_df(grid)
  required <- c("beta", "p.value", "accepted")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Acceptance grid is missing columns for boundary refinement: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  if (!nrow(x)) return(x)
  if (!is.function(evaluate)) {
    stop("Acceptance boundary refinement requires an evaluator function.", call. = FALSE)
  }
  if (!is.finite(level) || level <= 0 || level >= 1) {
    stop("Acceptance boundary refinement level must lie strictly between zero and one.", call. = FALSE)
  }

  if (!"boundary_refined" %in% names(x)) x$boundary_refined <- FALSE
  x$boundary_refined <- as.logical(x$boundary_refined)
  base <- x[!x$boundary_refined, , drop = FALSE]
  if (nrow(base) < 2L) return(x)
  base <- base[order(num(base$beta)), , drop = FALSE]
  accepted <- as.logical(base$accepted)
  transitions <- which(accepted[-nrow(base)] != accepted[-1L])
  if (!length(transitions)) return(x)

  alpha <- 1 - level
  refined <- lapply(transitions, function(i) {
    lower <- num(base$beta[[i]])
    upper <- num(base$beta[[i + 1L]])
    f_lower <- num(base$p.value[[i]]) - alpha
    f_upper <- num(base$p.value[[i + 1L]]) - alpha
    if (!all(is.finite(c(lower, upper, f_lower, f_upper))) || f_lower * f_upper > 0) {
      return(NULL)
    }

    objective <- function(value) {
      row <- safe_df(evaluate(value))
      if (nrow(row) != 1L || !"p.value" %in% names(row)) return(NA_real_)
      num(row$p.value[[1L]]) - alpha
    }
    root <- tryCatch(
      stats::uniroot(
        objective, interval = c(lower, upper),
        f.lower = f_lower, f.upper = f_upper,
        tol = 1e-8, check.conv = TRUE
      )$root,
      error = function(e) NA_real_
    )
    if (!is.finite(root)) return(NULL)

    row <- safe_df(evaluate(root))
    if (nrow(row) != 1L || !"p.value" %in% names(row) || !is.finite(num(row$p.value[[1L]]))) {
      return(NULL)
    }
    row$beta <- root
    row$accepted <- TRUE
    row$boundary_refined <- TRUE
    row
  })
  refined <- safe_bind_rows(refined)
  if (!nrow(refined)) return(x)

  out <- safe_bind_rows(list(x, refined))
  out <- out[order(num(out$beta), !as.logical(out$boundary_refined)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

anderson_rubin_grid <- function(
  data, outcome, treatment, excluded, included = character(),
  controls = character(), fixed_effect = "none", cluster,
  level = 0.95, points = 401L
) {
  alpha <- 1 - level
  evaluate <- function(value) {
    test <- anderson_rubin_test(
      data, outcome, treatment, excluded, included, controls, fixed_effect,
      cluster = cluster, beta0 = value
    )
    data.frame(
      beta = value,
      statistic = test[["statistic"]],
      p.value = test[["p.value"]],
      stringsAsFactors = FALSE
    )
  }

  beta <- anderson_rubin_beta_values(data, outcome, treatment, points)
  rows <- safe_bind_rows(lapply(beta, evaluate))
  rows$accepted <- is.finite(rows$p.value) & rows$p.value >= alpha
  rows$boundary_refined <- FALSE
  refine_acceptance_grid_boundaries(rows, evaluate, level = level)
}

bounded_exclusion_ar_specification_inputs <- function(data, specification) {
  specification <- as_single_iv_specification(specification)
  controls <- unlist(specification$controls[[1L]], use.names = FALSE)
  included <- unlist(specification$included_language_controls[[1L]], use.names = FALSE)
  excluded <- unlist(specification$excluded_instruments[[1L]], use.names = FALSE)
  if (length(excluded) != 1L) {
    stop(
      "Bounded exclusion sensitivity currently requires one excluded scalar instrument.",
      call. = FALSE
    )
  }
  needed <- iv_specification_variables(specification)
  missing <- setdiff(needed, names(data))
  if (length(missing)) {
    stop(
      "Bounded exclusion sensitivity is missing columns: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  x <- as.data.frame(data)
  x <- x[stats::complete.cases(x[needed]), , drop = FALSE]
  if (!nrow(x)) stop("Bounded exclusion sensitivity has no complete observations.", call. = FALSE)
  cluster <- iv_specification_cluster(x, specification)
  if (is.null(cluster)) {
    stop("Bounded exclusion sensitivity requires an aligned state cluster.", call. = FALSE)
  }

  list(
    data = x,
    outcome = specification$outcome[[1L]],
    treatment = specification$treatment[[1L]],
    instrument = plain_chr(excluded)[[1L]],
    excluded = excluded,
    included = included,
    controls = controls,
    fixed_effect = specification$fixed_effect[[1L]],
    cluster = cluster,
    specification_id = specification$specification_id[[1L]]
  )
}

bounded_exclusion_ar_profile_from_inputs <- function(inputs, points = 401L) {
  points <- as.integer(points)
  if (!is.finite(points) || points < 3L || points %% 2L != 1L) {
    stop(
      "Bounded exclusion sensitivity requires an odd beta grid with at least three points so beta = 0 is evaluated exactly.",
      call. = FALSE
    )
  }
  beta <- anderson_rubin_beta_values(
    inputs$data, inputs$outcome, inputs$treatment, points
  )
  profile <- bounded_exclusion_ar_profile(
    inputs$data, inputs$outcome, inputs$treatment, inputs$excluded,
    inputs$included, inputs$controls, inputs$fixed_effect,
    cluster = inputs$cluster, beta_values = beta
  )
  if (any(profile$status != "estimated")) {
    stop(
      "Bounded exclusion sensitivity could not obtain clustered inference for every beta grid point.",
      call. = FALSE
    )
  }
  profile$specification_id <- inputs$specification_id
  profile
}

estimate_anderson_rubin_spec <- function(
    data, specification, level = 0.95, points = 401L, invert = TRUE) {
  specification <- as_single_iv_specification(specification)
  controls <- unlist(specification$controls[[1]], use.names = FALSE)
  included <- unlist(specification$included_language_controls[[1]], use.names = FALSE)
  excluded <- unlist(specification$excluded_instruments[[1]], use.names = FALSE)
  outcome <- specification$outcome[[1]]
  treatment <- specification$treatment[[1]]
  fixed_effect <- specification$fixed_effect[[1]]
  needed <- iv_specification_variables(specification)
  missing <- setdiff(needed, names(data))
  if (length(missing)) {
    return(list(
      summary = data.frame(
        specification_id = specification$specification_id,
        status = "not_estimated",
        reason = paste0("Missing columns: ", paste(missing, collapse = ", ")),
        stringsAsFactors = FALSE
      ),
      grid = data.frame()
    ))
  }
  x <- data[stats::complete.cases(data[needed]), , drop = FALSE]
  if (!nrow(x)) {
    return(list(
      summary = data.frame(
        specification_id = specification$specification_id,
        status = "not_estimated", reason = "No complete observations.", stringsAsFactors = FALSE
      ),
      grid = data.frame()
    ))
  }
  cluster <- iv_specification_cluster(x, specification)
  ar0 <- anderson_rubin_test(
    x, outcome, treatment, excluded, included, controls, fixed_effect,
    cluster = cluster, beta0 = 0
  )
  grid <- if (isTRUE(invert)) {
    anderson_rubin_grid(
      x, outcome, treatment, excluded, included, controls, fixed_effect,
      cluster = cluster, level = level, points = points
    )
  } else {
    data.frame()
  }
  components <- if (nrow(grid)) {
    anderson_rubin_acceptance_components(grid)
  } else {
    data.frame()
  }
  if (nrow(grid)) grid$specification_id <- specification$specification_id

  if (nrow(components)) {
    components$lower <- num(components$lower)
    components$upper <- num(components$upper)
    components$touches_left_grid_edge <- as.logical(
      plain_chr(components$touches_left_grid_edge)
    )
    components$touches_right_grid_edge <- as.logical(
      plain_chr(components$touches_right_grid_edge)
    )
    components$contains_zero <- as.logical(plain_chr(components$contains_zero))
  }

  bounded_interval <- isTRUE(invert) && nrow(components) == 1L &&
    !components$touches_left_grid_edge[[1]] &&
    !components$touches_right_grid_edge[[1]]
  contains_zero <- if (isTRUE(invert)) {
    if (nrow(components)) any(components$contains_zero) else FALSE
  } else if (is.finite(ar0[["p.value"]])) {
    ar0[["p.value"]] >= 1 - level
  } else {
    NA
  }
  information <- if (isTRUE(invert)) {
    classify_anderson_rubin_information(components)
  } else {
    NA_character_
  }

  list(
    summary = data.frame(
      specification_id = specification$specification_id,
      anderson_rubin_f_beta0 = ar0[["statistic"]],
      anderson_rubin_p_beta0 = ar0[["p.value"]],
      ar_95_lower = if (bounded_interval) components$lower[[1]] else NA_real_,
      ar_95_upper = if (bounded_interval) components$upper[[1]] else NA_real_,
      ar_95_empty = if (isTRUE(invert)) !nrow(components) else NA,
      ar_95_n_components = if (isTRUE(invert)) nrow(components) else NA_integer_,
      ar_95_disconnected = if (isTRUE(invert)) nrow(components) > 1L else NA,
      ar_95_contains_zero = contains_zero,
      ar_95_grid_accepted_min = if (nrow(components)) min(components$lower) else NA_real_,
      ar_95_grid_accepted_max = if (nrow(components)) max(components$upper) else NA_real_,
      ar_95_left_truncated = if (isTRUE(invert)) {
        nrow(components) > 0L && any(components$touches_left_grid_edge)
      } else NA,
      ar_95_right_truncated = if (isTRUE(invert)) {
        nrow(components) > 0L && any(components$touches_right_grid_edge)
      } else NA,
      ar_95_components = if (isTRUE(invert)) {
        format_anderson_rubin_components(components)
      } else NA_character_,
      ar_95_information = information,
      ar_95_sign_identified = if (isTRUE(invert)) {
        information %in% c("positive_sign_only", "negative_sign_only")
      } else NA,
      n = nrow(x), status = "estimated", reason = NA_character_,
      stringsAsFactors = FALSE
    ),
    grid = grid
  )
}
