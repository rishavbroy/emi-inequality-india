# Extended diagnostics for Census worker structure and historical IV validity.

census_worker_2001_balance_variables <- function() {
  c(
    "manufacturing_share_among_main_workers",
    "construction_share_among_main_workers",
    "trade_share_among_main_workers",
    "transport_communication_share_among_main_workers",
    "finance_realestate_business_share_among_main_workers",
    "public_social_other_services_share_among_main_workers",
    "manager_professional_technical_share",
    "clerical_service_sales_share",
    "craft_machine_operator_share",
    "elementary_occupation_share"
  )
}

prepare_census_worker_2001_validity_panel <- function(panel, industry_2001, occupation_2001) {
  keys <- c("state_code", "district_code")
  industry <- safe_df(industry_2001)
  occupation <- safe_df(occupation_2001)
  variables <- census_worker_2001_balance_variables()
  industry_vars <- intersect(variables, names(industry))
  occupation_vars <- intersect(variables, names(occupation))
  if (!length(industry_vars) || !length(occupation_vars)) {
    stop("Census 2001 worker validity inputs are missing industry or occupation measures.", call. = FALSE)
  }
  measures <- merge(
    industry[c(keys, industry_vars)],
    occupation[c(keys, occupation_vars)],
    by = keys, all = TRUE, sort = FALSE
  )
  prepare_census_2001_balance_panel(
    panel, measures, variables, "Census worker validity"
  )
}

summarise_census_worker_coverage <- function(
    industry_2001, occupation_2001, industry_2011, occupation_2011) {
  safe_bind_rows(list(
    data.frame(
      dataset = "industry_2001",
      n_districts = nrow(industry_2001),
      n_positive_denominators = sum(
        is.finite(num(industry_2001$main_workers_total)) &
          num(industry_2001$main_workers_total) > 0
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      dataset = "occupation_2001",
      n_districts = nrow(occupation_2001),
      n_positive_denominators = sum(
        is.finite(num(occupation_2001$workers_excl_cultivators_aglab_total)) &
          num(occupation_2001$workers_excl_cultivators_aglab_total) > 0
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      dataset = "industry_2011_harmonized",
      n_districts = nrow(industry_2011),
      n_positive_denominators = sum(
        is.finite(num(industry_2011$workers_total)) & num(industry_2011$workers_total) > 0
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      dataset = "occupation_2011_harmonized",
      n_districts = nrow(occupation_2011),
      n_positive_denominators = sum(
        is.finite(num(occupation_2011$workers_excl_cultivators_aglab_total)) &
          num(occupation_2011$workers_excl_cultivators_aglab_total) > 0
      ),
      stringsAsFactors = FALSE
    )
  ))
}

build_census_worker_diagnostics <- function(
    b04_2001_source, b25_2001_source, b26_2001_source,
    industry_2001, occupation_2001,
    b04_2011_source, b06_2011_source, b25a_2011_source, b25b_2011_source,
    industry_2011, occupation_2011, district_panel, control_registry = NULL) {
  validity_panel <- prepare_census_worker_2001_validity_panel(
    district_panel, industry_2001, occupation_2001
  )
  validity_specs <- candidate_iv_balance_specifications(
    control_registry = control_registry
  )
  balance <- add_iv_balance_holm(
    run_iv_balance_diagnostics(
      validity_panel,
      specifications = validity_specs,
      variables = census_worker_2001_balance_variables()
    )
  )
  joint_balance <- run_iv_joint_balance_diagnostics(
    validity_panel,
    specifications = validity_specs,
    variables = census_worker_2001_balance_variables()
  )
  list(
    industry_2001 = safe_df(industry_2001),
    occupation_2001 = safe_df(occupation_2001),
    industry_2011_harmonized = safe_df(industry_2011),
    occupation_2011_harmonized = safe_df(occupation_2011),
    coverage = summarise_census_worker_coverage(
      industry_2001, occupation_2001, industry_2011, occupation_2011
    ),
    b25_b26_2001_main_occupation_validation =
      validate_census_2001_b25_b26_main_occupation(b25_2001_source, b26_2001_source),
    b04_b25a_universe_validation =
      validate_census_2011_b04_b25a_universe(b04_2011_source, b25a_2011_source),
    b06_b25b_universe_validation =
      validate_census_2011_b06_b25b_universe(b06_2011_source, b25b_2011_source),
    worker_2001_balance = balance,
    worker_2001_joint_balance = joint_balance
  )
}

save_census_worker_diagnostics <- function(
    diagnostics, dir = "outputs/diagnostics/extended/census_workers") {
  filenames <- c(
    industry_2001 = "industry_2001.csv",
    occupation_2001 = "occupation_2001.csv",
    industry_2011_harmonized = "industry_2011_harmonized_2001.csv",
    occupation_2011_harmonized = "occupation_2011_harmonized_2001.csv",
    coverage = "coverage.csv",
    b25_b26_2001_main_occupation_validation = "b25_b26_2001_main_occupation_validation.csv",
    b04_b25a_universe_validation = "b04_b25a_universe_validation.csv",
    b06_b25b_universe_validation = "b06_b25b_universe_validation.csv",
    worker_2001_balance = "worker_2001_instrument_balance.csv",
    worker_2001_joint_balance = "worker_2001_instrument_balance_joint.csv"
  )
  write_diagnostic_bundle(diagnostics[names(filenames)], dir, filenames)
}
