# Shared small-sample cluster-robust inference helpers for IV estimation and diagnostics.

clustered_coefficient_frame <- function(fit, vcov) {
  if (is.null(vcov) || !inherits(vcov, "clubSandwich")) return(data.frame())
  out <- tryCatch(
    clubSandwich::coef_test(
      fit,
      vcov = vcov,
      test = "Satterthwaite"
    ),
    error = function(e) NULL
  )
  if (is.null(out) || !nrow(out)) return(data.frame())

  result <- data.frame(
    Estimate = suppressWarnings(as.numeric(out$beta)),
    `Std. Error` = suppressWarnings(as.numeric(out$SE)),
    statistic = suppressWarnings(as.numeric(out$tstat)),
    `Pr(>|t|)` = suppressWarnings(as.numeric(out$p_Satt)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(result) <- as.character(out$Coef)
  attr(result, "df") <- stats::setNames(
    suppressWarnings(as.numeric(out$df_Satt)),
    as.character(out$Coef)
  )
  result
}

wald_test_from_vcov <- function(fit, terms, vcov) {
  coefficients <- tryCatch(stats::coef(fit), error = function(e) NULL)
  if (is.null(coefficients) || !length(terms) || any(!terms %in% names(coefficients))) {
    return(c(statistic = NA_real_, p.value = NA_real_, df = length(terms), df_denom = NA_real_))
  }
  if (is.null(vcov) || !inherits(vcov, "clubSandwich")) {
    return(c(statistic = NA_real_, p.value = NA_real_, df = length(terms), df_denom = NA_real_))
  }

  constraints <- clubSandwich::constrain_zero(terms, coefs = coefficients)
  out <- tryCatch(
    clubSandwich::Wald_test(
      fit,
      constraints = constraints,
      vcov = vcov,
      test = "HTZ",
      tidy = TRUE
    ),
    error = function(e) NULL
  )
  if (is.null(out) || !nrow(out)) {
    return(c(statistic = NA_real_, p.value = NA_real_, df = length(terms), df_denom = NA_real_))
  }
  c(
    statistic = suppressWarnings(as.numeric(out$Fstat[[1L]])),
    p.value = suppressWarnings(as.numeric(out$p_val[[1L]])),
    df = suppressWarnings(as.numeric(out$df_num[[1L]])),
    df_denom = suppressWarnings(as.numeric(out$df_denom[[1L]]))
  )
}

clustered_joint_wald_test <- function(fit, terms, cluster, inference = NULL) {
  if (is.null(inference)) {
    inference <- tryCatch(iv_clustered_inference(fit, cluster), error = function(e) NULL)
  }
  if (is.null(inference) || is.null(inference$vcov)) {
    return(c(statistic = NA_real_, p.value = NA_real_, df = length(terms), df_denom = NA_real_))
  }
  wald_test_from_vcov(fit, terms, inference$vcov)
}

many_cluster_lm_inference <- function(model, cluster) {
  cluster <- as.vector(cluster)
  n_clusters <- length(unique(cluster))
  if (
    length(cluster) != stats::nobs(model) ||
      anyNA(cluster) ||
      n_clusters < 2L
  ) {
    return(list(
      vcov = NULL, coefficients = data.frame(), df = NA_real_,
      status = "unavailable",
      reason = "Cluster vector is incomplete or not aligned to fitted observations."
    ))
  }

  vc <- tryCatch(
    sandwich::vcovCL(model, cluster = cluster, type = "HC1"),
    error = function(e) e
  )
  if (inherits(vc, "error")) {
    return(list(
      vcov = NULL, coefficients = data.frame(), df = NA_real_,
      status = "unavailable", reason = conditionMessage(vc)
    ))
  }

  df <- n_clusters - 1
  coefficients <- tryCatch(
    as.data.frame(lmtest::coeftest(model, vcov. = vc, df = df)),
    error = function(e) data.frame()
  )
  list(
    vcov = vc, coefficients = coefficients, df = df,
    status = "estimated", reason = NA_character_
  )
}

many_cluster_lm_joint_test <- function(model, terms, inference) {
  if (
    !length(terms) || is.null(inference$vcov) ||
      !is.finite(inference$df) || inference$df <= 0
  ) {
    return(c(statistic = NA_real_, p.value = NA_real_, df = length(terms)))
  }
  coefficients <- tryCatch(stats::coef(model), error = function(e) NULL)
  if (is.null(coefficients) || any(!terms %in% names(coefficients))) {
    return(c(statistic = NA_real_, p.value = NA_real_, df = length(terms)))
  }

  out <- tryCatch(
    car::linearHypothesis(
      model, terms, vcov. = inference$vcov, test = "F",
      error.df = inference$df, singular.ok = TRUE
    ),
    error = function(e) NULL
  )
  if (is.null(out) || nrow(out) < 2L) {
    return(c(statistic = NA_real_, p.value = NA_real_, df = length(terms)))
  }
  f_col <- grep("^F$", names(out), value = TRUE)
  p_col <- match("Pr(>F)", names(out))
  if (!length(f_col) || is.na(p_col)) {
    return(c(statistic = NA_real_, p.value = NA_real_, df = length(terms)))
  }
  c(
    statistic = suppressWarnings(as.numeric(out[[f_col[[1L]]]][[2L]])),
    p.value = suppressWarnings(as.numeric(out[[p_col]][[2L]])),
    df = length(terms)
  )
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

  clustered_requested <- !is.null(vcov) && inherits(vcov, "clubSandwich")
  clustered <- clustered_coefficient_frame(fit, vcov)
  if (nrow(clustered) && term %in% rownames(clustered)) {
    return(c(
      estimate = clustered[term, "Estimate"],
      std.error = clustered[term, "Std. Error"],
      statistic = clustered[term, "statistic"],
      p.value = clustered[term, "Pr(>|t|)"]
    ))
  }
  if (clustered_requested) {
    return(c(
      estimate = unname(coefficients[[term]]),
      std.error = NA_real_, statistic = NA_real_, p.value = NA_real_
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
