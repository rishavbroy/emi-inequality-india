# IV formulas for the main analysis and appendix comparisons.

# sample-start: code-iv-formula-estimation

#' Make an IV formula
#'
make_iv_formula <- function(dep, endog, instruments, controls = NULL, fixed_effects = NULL) {
  stats::as.formula(paste(
    dep,
    "~",
    paste(c(endog, controls, fixed_effects), collapse = " + "),
    "|",
    paste(c(instruments, controls, fixed_effects), collapse = " + ")
  ))
}

baseline_iv_controls <- function() {
  main_census_2001_controls()
}

legacy_2007_iv_controls <- function() {
  c(
    "consumption_0708", "gini_cons_0708",
    "pct_urban", "avg_hh_size", "dependency_ratio",
    "pct_fem_head", "pct_hindu", "pct_muslim",
    "pct_st", "pct_sc", "pct_obc",
    "pct_small_land", "pct_medium_land", "pct_large_land",
    "pct_head_lit_to_primary", "pct_head_secondary_plus"
  )
}

iv_state_fixed_effects <- function() {
  "factor(state_2001_cluster)"
}

#' Build IV formulas
#'
build_iv_formulas <- function(cfg) {
  controls <- baseline_iv_controls()
  state_fe <- iv_state_fixed_effects()
  list(
    consumption = make_iv_formula(
      "real_log_consumption_change",
      "EMIE",
      "wavg_ling_degrees",
      controls,
      state_fe
    ),
    consumption_ancova = make_iv_formula(
      "log_real_consumption_1718",
      "EMIE",
      "wavg_ling_degrees",
      c("log_real_consumption_0708", controls),
      state_fe
    ),
    consumption_household_mean = make_iv_formula(
      "real_log_consumption_change_household_mean",
      "EMIE",
      "wavg_ling_degrees",
      controls,
      state_fe
    ),
    consumption_nominal = make_iv_formula(
      "log_consumption_difference",
      "EMIE",
      "wavg_ling_degrees",
      controls,
      state_fe
    ),
    consumption_legacy_controls = make_iv_formula(
      "real_log_consumption_change",
      "EMIE",
      "wavg_ling_degrees",
      legacy_2007_iv_controls(),
      state_fe
    )
  )
}

# sample-end: code-iv-formula-estimation
