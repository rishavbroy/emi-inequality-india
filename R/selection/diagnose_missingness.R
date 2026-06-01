# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-missingness-logit-parallel

#' diagnose missingness
#'
#' @return A tibble, model object, list, or file path depending on context.
diagnose_missingness <- function(selection_data, cfg) {
  selection_data
}

#' check missing logit parallel
#'
#' @return A tibble, model object, list, or file path depending on context.
check_missing_logit_parallel <- function(df, miss_vars, covars, method_p = "BH") {
  rhs <- paste(covars, collapse = " + ")
  fit_one <- function(m) { f <- stats::as.formula(paste0("is.na(", m, ") ~ ", rhs)); fit <- stats::glm(f, data = df, family = stats::binomial); broom::tidy(fit) |> dplyr::mutate(missing_var = m, pseudoR2 = 1 - fit$deviance / fit$null.deviance, nobs = stats::nobs(fit)) }
  out <- dplyr::bind_rows(lapply(miss_vars, fit_one))
  out |> dplyr::group_by(missing_var) |> dplyr::mutate(p_adj = {adj <- rep(NA_real_, dplyr::n()); idx <- term != "(Intercept)"; adj[idx] <- p.adjust(p.value[idx], method = method_p); adj}) |> dplyr::ungroup()
}

#' summarize missingness by variable
#'
#' @return A tibble, model object, list, or file path depending on context.
summarize_missingness_by_variable <- function(df) {
  df
}

#' run missingness logits
#'
#' @return A tibble, model object, list, or file path depending on context.
run_missingness_logits <- function(df, miss_vars, covars) {
  check_missing_logit_parallel(df, miss_vars, covars)
}

#' adjust missingness pvalues bh
#'
#' @return A tibble, model object, list, or file path depending on context.
adjust_missingness_pvalues_bh <- function(df) {
  df
}

#' save missingness diagnostics
#'
#' @return A tibble, model object, list, or file path depending on context.
save_missingness_diagnostics <- function(diagnostics) {
  diagnostics
}

# sample-end: code-missingness-logit-parallel
