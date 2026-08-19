# Shared clustered Wald inference helpers for IV estimation and diagnostics.

clustered_joint_wald_test <- function(fit, terms, cluster) {
  inference <- tryCatch(iv_clustered_inference(fit, cluster), error = function(e) NULL)
  if (is.null(inference) || is.null(inference$vcov) || !requireNamespace("car", quietly = TRUE)) {
    return(c(statistic = NA_real_, p.value = NA_real_, df = length(terms)))
  }
  hypotheses <- paste0(terms, " = 0")
  test <- tryCatch(
    car::linearHypothesis(fit, hypotheses, vcov. = inference$vcov, test = "F"),
    error = function(e) NULL
  )
  if (is.null(test) || nrow(test) < 2L || !all(c("F", "Pr(>F)") %in% names(test))) {
    return(c(statistic = NA_real_, p.value = NA_real_, df = length(terms)))
  }
  c(
    statistic = suppressWarnings(as.numeric(test[["F"]][[2]])),
    p.value = suppressWarnings(as.numeric(test[["Pr(>F)"]][[2]])),
    df = length(terms)
  )
}


iv_nuisance_terms <- function(controls = character(), fixed_effect = "none") {
  unique(c(controls, iv_fixed_effect_terms(fixed_effect)))
}

residualize_iv_variable <- function(data, variable, controls = character(), fixed_effect = "none") {
  rhs <- iv_nuisance_terms(controls, fixed_effect)
  if (!length(rhs)) return(num(data[[variable]]) - mean(num(data[[variable]])))
  stats::residuals(stats::lm(stats::reformulate(rhs, response = variable), data = data))
}
