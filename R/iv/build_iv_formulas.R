# IV formula definitions.

# sample-start: code-iv-formula-estimation

make_iv_formula <- function(dep, endog, instruments, controls = NULL, fixed_effects = NULL) {
  stats::as.formula(paste(
    dep, '~', paste(c(endog, controls, fixed_effects), collapse = ' + '), '|',
    paste(c(instruments, controls, fixed_effects), collapse = ' + ')
  ))
}

legacy_2007_iv_controls <- function() {
  c(
    'consumption_0708', 'gini_cons_0708', 'pct_urban', 'avg_hh_size',
    'dependency_ratio', 'pct_fem_head', 'pct_hindu', 'pct_muslim',
    'pct_st', 'pct_sc', 'pct_obc', 'pct_small_land', 'pct_medium_land',
    'pct_large_land', 'pct_head_lit_to_primary', 'pct_head_secondary_plus'
  )
}


preferred_iv_variables <- function() {
  list(
    treatment = "emi_exposure_all_children_0708",
    instrument = "ling_distance_nonzero_mean"
  )
}

# Historical specification retained only for the optional legacy-geography comparison.
build_legacy_iv_formulas <- function() {
  controls <- legacy_2007_iv_controls()
  list(
    consumption = make_iv_formula(
      "consumption_pct_change", "EMIE", "wavg_ling_degrees", controls
    ),
    gini = make_iv_formula(
      "gini_change", "EMIE", "wavg_ling_degrees", controls
    )
  )
}

# Preferred public formulas: all-child EMI exposure, the full-distribution
# nonzero linguistic-distance scalar, predetermined Census controls, and state FE.
build_revised_iv_formulas <- function() {
  spec <- preferred_iv_variables()
  controls <- census_2001_main_controls()
  state_fe <- 'factor(state_code_2001)'
  list(
    consumption = make_iv_formula(
      'real_log_consumption_change', spec$treatment, spec$instrument,
      controls = controls, fixed_effects = state_fe
    ),
    consumption_ancova = make_iv_formula(
      'log_real_consumption_1718', spec$treatment, spec$instrument,
      controls = c('log_real_consumption_0708', controls), fixed_effects = state_fe
    ),
    consumption_nominal = make_iv_formula(
      'log_consumption_difference', spec$treatment, spec$instrument,
      controls = controls, fixed_effects = state_fe
    ),
    consumption_legacy_controls = make_iv_formula(
      'log_consumption_difference', spec$treatment, spec$instrument,
      controls = legacy_2007_iv_controls(), fixed_effects = state_fe
    )
  )
}
# sample-end: code-iv-formula-estimation
