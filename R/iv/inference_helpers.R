# Shared clustered Wald inference helpers for IV estimation and diagnostics.

wald_f_from_vcov <- function(fit, terms, vcov) {
  coefficients <- stats::coef(fit)
  available <- terms[
    terms %in% names(coefficients) &
      terms %in% rownames(vcov) &
      terms %in% colnames(vcov) &
      is.finite(coefficients[terms])
  ]
  if (!length(available) || length(available) != length(terms)) {
    return(c(statistic = NA_real_, p.value = NA_real_, df = length(terms)))
  }

  beta <- unname(coefficients[available])
  variance <- vcov[available, available, drop = FALSE]
  if (!all(is.finite(variance)) || qr(variance)$rank != length(available)) {
    return(c(statistic = NA_real_, p.value = NA_real_, df = length(terms)))
  }

  q <- length(available)
  statistic <- as.numeric(crossprod(beta, solve(variance, beta)) / q)
  residual_df <- stats::df.residual(fit)
  p_value <- if (is.finite(statistic) && is.finite(residual_df) && residual_df > 0) {
    stats::pf(statistic, q, residual_df, lower.tail = FALSE)
  } else {
    NA_real_
  }
  c(statistic = statistic, p.value = p_value, df = q)
}

clustered_joint_wald_test <- function(fit, terms, cluster, inference = NULL) {
  if (is.null(inference)) {
    inference <- tryCatch(iv_clustered_inference(fit, cluster), error = function(e) NULL)
  }
  if (is.null(inference) || is.null(inference$vcov)) {
    return(c(statistic = NA_real_, p.value = NA_real_, df = length(terms)))
  }
  wald_f_from_vcov(fit, terms, inference$vcov)
}

joint_wald_estimability <- function(fit, terms, joint) {
  residual_df <- tryCatch(stats::df.residual(fit), error = function(e) NA_real_)
  if (!is.finite(residual_df) || residual_df <= 0) {
    return(c(status = "not_estimable", reason = "no_residual_degrees_of_freedom"))
  }

  coefficients <- tryCatch(stats::coef(fit), error = function(e) NULL)
  if (is.null(coefficients) || any(!terms %in% names(coefficients)) ||
      any(!is.finite(coefficients[terms]))) {
    return(c(status = "not_estimable", reason = "tested_terms_aliased"))
  }

  statistic <- if ("statistic" %in% names(joint)) as.numeric(joint[["statistic"]]) else NA_real_
  p_value <- if ("p.value" %in% names(joint)) as.numeric(joint[["p.value"]]) else NA_real_
  if (!is.finite(statistic) || !is.finite(p_value)) {
    return(c(status = "not_estimable", reason = "clustered_joint_inference_unavailable"))
  }
  c(status = "estimated", reason = NA_character_)
}


iv_nuisance_terms <- function(controls = character(), fixed_effect = "none") {
  unique(c(controls, iv_fixed_effect_terms(fixed_effect)))
}

residualize_iv_variables <- function(data, variables, controls = character(), fixed_effect = "none") {
  variables <- unique(plain_chr(variables))
  if (!length(variables) || anyNA(variables) || any(!nzchar(variables))) {
    stop("IV residualization requires one or more variable names.", call. = FALSE)
  }
  missing <- setdiff(variables, names(data))
  if (length(missing)) {
    stop("IV residualization is missing variables: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  y <- as.matrix(data[variables])
  storage.mode(y) <- "double"
  rhs <- iv_nuisance_terms(controls, fixed_effect)
  if (!length(rhs)) {
    out <- sweep(y, 2L, colMeans(y), FUN = "-")
  } else {
    design <- stats::model.matrix(stats::reformulate(rhs), data = data)
    out <- stats::lm.fit(design, y)$residuals
  }
  # lm.fit() simplifies one-column matrix responses to a vector. Preserve the
  # documented matrix contract so single- and multi-variable callers behave
  # identically and the scalar wrapper remains a trivial column extraction.
  out <- matrix(
    out, nrow = nrow(data), ncol = length(variables),
    dimnames = list(NULL, variables)
  )
  out
}

residualize_iv_variable <- function(data, variable, controls = character(), fixed_effect = "none") {
  unname(residualize_iv_variables(data, variable, controls, fixed_effect)[, 1L])
}

model_term_inference <- function(fit, term, vcov = NULL) {
  coefficients <- tryCatch(stats::coef(fit), error = function(e) NULL)
  if (is.null(coefficients) || !term %in% names(coefficients) ||
      !is.finite(coefficients[[term]])) {
    return(c(
      estimate = NA_real_, std.error = NA_real_,
      statistic = NA_real_, p.value = NA_real_
    ))
  }

  if (is.null(vcov)) {
    vcov <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  }
  se <- NA_real_
  if (!is.null(vcov) && length(dim(vcov)) == 2L &&
      term %in% rownames(vcov) && term %in% colnames(vcov) &&
      is.finite(vcov[term, term]) && vcov[term, term] >= 0) {
    se <- sqrt(vcov[term, term])
  }

  estimate <- unname(coefficients[[term]])
  statistic <- if (is.finite(se) && se > 0) estimate / se else NA_real_
  residual_df <- tryCatch(stats::df.residual(fit), error = function(e) NA_real_)
  p_value <- if (is.finite(statistic) && is.finite(residual_df) && residual_df > 0) {
    2 * stats::pt(abs(statistic), df = residual_df, lower.tail = FALSE)
  } else if (is.finite(statistic)) {
    2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
  } else {
    NA_real_
  }

  c(
    estimate = estimate, std.error = se,
    statistic = statistic, p.value = p_value
  )
}
