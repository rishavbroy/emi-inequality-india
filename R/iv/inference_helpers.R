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

clustered_joint_wald_test <- function(fit, terms, cluster) {
  inference <- tryCatch(iv_clustered_inference(fit, cluster), error = function(e) NULL)
  if (is.null(inference) || is.null(inference$vcov)) {
    return(c(statistic = NA_real_, p.value = NA_real_, df = length(terms)))
  }

  if (requireNamespace("car", quietly = TRUE)) {
    hypotheses <- paste0(terms, " = 0")
    test <- tryCatch(
      car::linearHypothesis(fit, hypotheses, vcov. = inference$vcov, test = "F"),
      error = function(e) NULL
    )
    if (!is.null(test) && nrow(test) >= 2L && all(c("F", "Pr(>F)") %in% names(test))) {
      result <- c(
        statistic = suppressWarnings(as.numeric(test[["F"]][[2]])),
        p.value = suppressWarnings(as.numeric(test[["Pr(>F)"]][[2]])),
        df = length(terms)
      )
      if (all(is.finite(result[c("statistic", "p.value")]))) return(result)
    }
  }

  wald_f_from_vcov(fit, terms, inference$vcov)
}


iv_nuisance_terms <- function(controls = character(), fixed_effect = "none") {
  unique(c(controls, iv_fixed_effect_terms(fixed_effect)))
}

residualize_iv_variable <- function(data, variable, controls = character(), fixed_effect = "none") {
  rhs <- iv_nuisance_terms(controls, fixed_effect)
  if (!length(rhs)) return(num(data[[variable]]) - mean(num(data[[variable]])))
  stats::residuals(stats::lm(stats::reformulate(rhs, response = variable), data = data))
}
