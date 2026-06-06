# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-missingness-logit-parallel

#' diagnose missingness
#'
#' @return Function-specific return value.
diagnose_missingness <- function(selection_data, cfg) {
  data.frame(
    missing_var = names(selection_data)[vapply(selection_data, function(x) any(is.na(x)), logical(1))],
    stringsAsFactors = FALSE
  )
}

#' check missing logit parallel
#'
#' @return Function-specific return value.
check_missing_logit_parallel <- function(df, miss_vars, covars, method_p = "BH") {
  rhs <- paste(covars, collapse = " + ")
  fit_one <- function(m) { f <- stats::as.formula(paste0("is.na(", m, ") ~ ", rhs)); fit <- stats::glm(f, data = df, family = stats::binomial); broom::tidy(fit) |> dplyr::mutate(missing_var = m, pseudoR2 = 1 - fit$deviance / fit$null.deviance, nobs = stats::nobs(fit)) }
  out <- dplyr::bind_rows(lapply(miss_vars, fit_one))
  out |> dplyr::group_by(missing_var) |> dplyr::mutate(p_adj = {adj <- rep(NA_real_, dplyr::n()); idx <- term != "(Intercept)"; adj[idx] <- p.adjust(p.value[idx], method = method_p); adj}) |> dplyr::ungroup()
}

#' summarize missingness by variable
#'
#' @return Function-specific return value.
summarize_missingness_by_variable <- function(df) {
  data.frame(
    missing_var = names(df),
    n_missing = vapply(df, function(x) sum(is.na(x)), integer(1)),
    stringsAsFactors = FALSE
  )
}

#' run missingness logits
#'
#' @return Function-specific return value.
run_missingness_logits <- function(df, miss_vars, covars) {
  check_missing_logit_parallel(df, miss_vars, covars)
}

#' adjust missingness pvalues bh
#'
#' @return Function-specific return value.
adjust_missingness_pvalues_bh <- function(df) {
  df
}

#' save missingness diagnostics
#'
#' @return Function-specific return value.
save_missingness_diagnostics <- function(diagnostics) {
  diagnostics
}

# sample-end: code-missingness-logit-parallel
