# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-iv-formula-estimation

#' make iv formula
#'
#' @return A tibble, model object, list, or file path depending on context.
make_iv_formula <- function(dep, endog, instruments, controls = NULL, fixed_effects = NULL) {
  stats::as.formula(paste(
    dep,
    "~",
    paste(c(endog, controls, fixed_effects), collapse = " + "),
    "|",
    paste(c(instruments, controls, fixed_effects), collapse = " + ")
  ))
}

#' build iv formulas
#'
#' @return A tibble, model object, list, or file path depending on context.
build_iv_formulas <- function(cfg) {
  controls <- c(
    "consumption_2007", "gini_consumption_2007",
    "pct_urban", "avg_hh_size", "dependency_ratio",
    "pct_fem_head",
    "pct_hindu", "pct_muslim",
    "pct_st", "pct_sc", "pct_obc",
    "pct_small_land", "pct_medium_land", "pct_large_land",
    "pct_head_lit_to_primary",
    "pct_head_secondary_plus"
  )
  list(
    baseline = make_iv_formula(
      "consumption_growth_pct",
      "emie_2007",
      "wavg_ling_degrees",
      controls
    ),
    fd_log = make_iv_formula(
      "log_consumption_difference",
      "emie_2007",
      "wavg_ling_degrees",
      controls
    )
  )
}

#' build baseline 2sls formula
#'
#' @return A tibble, model object, list, or file path depending on context.
build_baseline_2sls_formula <- function(...) {
  make_iv_formula(...)
}

#' build fd 2sls formula
#'
#' @return A tibble, model object, list, or file path depending on context.
build_fd_2sls_formula <- function(...) {
  stop("TODO: implement after FD redesign")
}

#' build state fe 2sls formula
#'
#' @return A tibble, model object, list, or file path depending on context.
build_state_fe_2sls_formula <- function(...) {
  stop("TODO: implement after state-FE redesign")
}
# sample-end: code-iv-formula-estimation
