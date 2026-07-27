# IV formula definitions.

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

baseline_iv_controls <- function() legacy_2007_iv_controls()

# Current paper formulas remain available until the price and Census-control
# targets have passed their completeness checks.
build_iv_formulas <- function(cfg) {
  controls <- baseline_iv_controls()
  list(
    consumption = make_iv_formula('consumption_pct_change', 'EMIE', 'wavg_ling_degrees', controls),
    gini = make_iv_formula('gini_change', 'EMIE', 'wavg_ling_degrees', controls)
  )
}

# Formulas for the next paper revision. These are kept separate so that a missing
# price series or Census table cannot silently change the current estimates.
build_revised_iv_formulas <- function() {
  controls <- census_2001_main_controls()
  state_fe <- 'state_2001'
  list(
    consumption = make_iv_formula(
      'real_log_consumption_change', 'EMIE', 'wavg_ling_degrees',
      controls = controls, fixed_effects = state_fe
    ),
    consumption_ancova = make_iv_formula(
      'log_real_consumption_1718', 'EMIE', 'wavg_ling_degrees',
      controls = c('log_real_consumption_0708', controls), fixed_effects = state_fe
    ),
    consumption_nominal = make_iv_formula(
      'log_consumption_difference', 'EMIE', 'wavg_ling_degrees',
      controls = controls, fixed_effects = state_fe
    ),
    consumption_legacy_controls = make_iv_formula(
      'log_consumption_difference', 'EMIE', 'wavg_ling_degrees',
      controls = legacy_2007_iv_controls(), fixed_effects = state_fe
    )
  )
}
