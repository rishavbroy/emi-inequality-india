# Extended diagnostics and registered causal-mechanism inference for Economic Census measures.

economic_census_mechanism_registry <- function() {
  data.frame(
    outcome_id = c(
      "nonfarm_employment_growth",
      "firm_growth",
      "hired_employment_share_change",
      "private_employment_share_change",
      "services_employment_share_change",
      "manufacturing_employment_share_change"
    ),
    source_id = rep("change", 6L),
    variable = c(
      "log_nonfarm_employment_change_2013_2005",
      "log_firms_total_change_2013_2005",
      "hired_employment_share_change_2013_2005",
      "private_employment_share_change_2013_2005",
      "services_employment_share_change_2013_2005",
      "manufacturing_employment_share_change_2013_2005"
    ),
    construct_id = c(
      "log_nonfarm_employment_change_2013_2005",
      "log_firms_total_change_2013_2005",
      "hired_employment_share_change_2013_2005",
      "private_employment_share_change_2013_2005",
      "services_employment_share_change_2013_2005",
      "manufacturing_employment_share_change_2013_2005"
    ),
    label = c(
      "Log change in nonfarm employment",
      "Log change in establishments",
      "Change in hired-worker employment share",
      "Change in private-sector employment share",
      "Change in services employment share",
      "Change in manufacturing employment share"
    ),
    unit = c("log_change", "log_change", rep("share_change", 4L)),
    mechanism_family = c(
      "local_labor_demand", "establishment_growth", "employment_formality",
      "ownership_structure", "sectoral_shift", "sectoral_shift"
    ),
    tier = c("core", "core", "core", "core", "core", "secondary"),
    denominator = c(
      "log_nonfarm_employment", "log_establishments", rep("nonfarm_employment", 4L)
    ),
    stringsAsFactors = FALSE
  )
}

economic_census_mechanism_specifications <- function(
    outcome = "log_nonfarm_employment_change_2013_2005",
    treatment = preferred_iv_variables()$treatment,
    control_registry = NULL) {
  posttreatment_mechanism_specifications(
    outcome = outcome,
    treatment = treatment,
    sample_rule = "economic_census_change_mechanism_common_support",
    control_registry = control_registry
  )
}

# Predetermined local opportunity environment used only for bounded effect
# modification. NIC-2004 Division 72 is observed in the Fifth Economic Census
# establishment records; it is not promoted into the weak-IV outcome family.
economic_census_it_opportunity_construct_registry <- function() {
  data.frame(
    construct_id = "ec05_it_employment_share_nonfarm",
    variable = "it_employment_share_nonfarm",
    label = "EC05 computer-related employment share of nonfarm employment",
    unit = "share",
    denominator = "nonfarm_employment",
    stringsAsFactors = FALSE
  )
}

economic_census_it_opportunity_specifications <- function(consumption_registry) {
  welfare <- schooling_consumption_bridge_welfare_registry(consumption_registry)
  welfare <- welfare[
    welfare$welfare_specification_id == "long_2022__change", , drop = FALSE
  ]
  if (nrow(welfare) != 1L) {
    stop("EC05 IT-opportunity heterogeneity requires the registered 2022 change outcome.", call. = FALSE)
  }
  predictor_id <- c("linguistic_opportunity", "schooling_exposure")
  predictor <- c(
    preferred_iv_variables()$instrument,
    preferred_iv_variables()$treatment
  )
  predictor_role <- c("instrument", "treatment")
  predictor_scale <- c(1, 10)
  specification_id <- paste(predictor_id, "ec05_it_environment", sep = "__")
  data.frame(
    analysis_id = paste("economic_census_it_opportunity", specification_id, sep = "__"),
    specification_id = specification_id,
    welfare_specification_id = welfare$welfare_specification_id[[1L]],
    outcome_round = welfare$outcome_round[[1L]],
    outcome_construct_id = paste(
      "consumption", welfare$outcome_round[[1L]], welfare$outcome_id[[1L]], sep = "__"
    ),
    baseline_construct_id = paste(
      "consumption", welfare$baseline_round[[1L]], welfare$outcome_id[[1L]], sep = "__"
    ),
    estimand = c(
      "linguistic_opportunity_by_predetermined_it_environment",
      "descriptive_schooling_by_predetermined_it_environment"
    ),
    predictor_id = predictor_id,
    predictor = predictor,
    predictor_role = predictor_role,
    predictor_scale = predictor_scale,
    modifier_id = "ec05_it_employment_share",
    modifier = "it_employment_share_nonfarm",
    modifier_construct_id = "ec05_it_employment_share_nonfarm",
    adjustment_id = "state_main",
    stringsAsFactors = FALSE
  )
}

prepare_economic_census_it_opportunity_sample <- function(
    district_panel, ec05_it_baseline, consumption_registry, control_registry = NULL) {
  panel <- if (inherits(district_panel, "sf")) {
    sf::st_drop_geometry(district_panel)
  } else {
    safe_df(district_panel)
  }
  it <- safe_df(ec05_it_baseline)
  required_it <- c(
    "target_unit_2001", "source_available", "it_employment_share_nonfarm"
  )
  missing_it <- setdiff(required_it, names(it))
  if (length(missing_it)) {
    stop(
      "EC05 IT-opportunity baseline lacks fields: ",
      paste(missing_it, collapse = ", "), call. = FALSE
    )
  }
  if (anyDuplicated(it$target_unit_2001)) {
    stop("EC05 IT-opportunity baseline is not unique by Census-2001 target.", call. = FALSE)
  }
  it <- it[it$source_available %in% TRUE, c(
    "target_unit_2001", "it_employment_share_nonfarm"
  ), drop = FALSE]
  panel <- merge(panel, it, by = "target_unit_2001", all.x = TRUE, sort = FALSE)

  specs <- economic_census_it_opportunity_specifications(consumption_registry)
  welfare <- schooling_consumption_bridge_welfare_registry(consumption_registry)
  welfare <- welfare[
    welfare$welfare_specification_id == specs$welfare_specification_id[[1L]],
    , drop = FALSE
  ]
  adjustments <- schooling_consumption_bridge_adjustment_registry(control_registry)
  adjustment <- adjustments[adjustments$specification_id == "state_main", , drop = FALSE]
  if (nrow(adjustment) != 1L) {
    stop("EC05 IT-opportunity heterogeneity requires the registered state-main adjustment.", call. = FALSE)
  }
  sample_adjustment <- adjustment
  sample_adjustment$controls[[1L]] <- unique(c(
    adjustment$controls[[1L]], specs$modifier[[1L]]
  ))
  prepare_schooling_consumption_bridge_sample(
    panel, unique(specs$predictor), welfare, sample_adjustment
  )
}

fit_economic_census_it_opportunity_specification <- function(
    sample, specification, consumption_registry, control_registry = NULL) {
  spec <- safe_df(specification)
  if (nrow(spec) != 1L) {
    stop("EC05 IT-opportunity fit requires one specification row.", call. = FALSE)
  }
  welfare <- schooling_consumption_bridge_welfare_registry(consumption_registry)
  welfare <- welfare[
    welfare$welfare_specification_id == spec$welfare_specification_id[[1L]],
    , drop = FALSE
  ]
  adjustments <- schooling_consumption_bridge_adjustment_registry(control_registry)
  adjustment <- adjustments[
    adjustments$specification_id == spec$adjustment_id[[1L]], , drop = FALSE
  ]
  if (nrow(welfare) != 1L || nrow(adjustment) != 1L) {
    stop("EC05 IT-opportunity fit could not resolve its welfare or adjustment row.", call. = FALSE)
  }
  outcome <- consumption_iv_variable_name(
    welfare$welfare_specification_id[[1L]], "outcome"
  )
  x <- safe_df(sample)
  fitted <- fit_standardized_modifier_interaction(
    sample = x,
    outcome = outcome,
    predictor = spec$predictor[[1L]],
    modifier = spec$modifier[[1L]],
    controls = adjustment$controls[[1L]],
    fixed_effect = adjustment$fixed_effect[[1L]],
    cluster = x$state_code_2001,
    predictor_scale = spec$predictor_scale[[1L]]
  )
  fitted$analysis_id <- spec$analysis_id[[1L]]
  fitted$specification_id <- spec$specification_id[[1L]]
  fitted$predictor_id <- spec$predictor_id[[1L]]
  fitted$predictor <- spec$predictor[[1L]]
  fitted$predictor_role <- spec$predictor_role[[1L]]
  fitted$predictor_scale <- spec$predictor_scale[[1L]]
  fitted$modifier_id <- spec$modifier_id[[1L]]
  fitted$modifier <- spec$modifier[[1L]]
  fitted
}

diagnose_economic_census_it_opportunity <- function(
    district_panel, ec05_it_baseline, consumption_registry, control_registry = NULL) {
  specs <- economic_census_it_opportunity_specifications(consumption_registry)
  sample <- prepare_economic_census_it_opportunity_sample(
    district_panel, ec05_it_baseline, consumption_registry, control_registry
  )
  estimates <- safe_bind_rows(lapply(seq_len(nrow(specs)), function(i) {
    fit_economic_census_it_opportunity_specification(
      sample, specs[i, , drop = FALSE], consumption_registry, control_registry
    )
  }))
  if (length(unique(estimates$n)) != 1L) {
    stop("EC05 IT-opportunity family must use one common district sample.", call. = FALSE)
  }
  estimates$interaction_p_value_holm_family <- holm_adjust_finite(
    estimates$interaction_p_value_clustered
  )
  estimates <- estimates[
    match(specs$specification_id, estimates$specification_id), , drop = FALSE
  ]
  rownames(estimates) <- NULL
  list(specifications = specs, estimates = estimates)
}

prepare_economic_census_mechanism_panel <- function(
    district_panel,
    changes,
    registry = economic_census_mechanism_registry(),
    control_registry = NULL) {
  prepare_posttreatment_mechanism_panel(
    district_panel = district_panel,
    sources = list(change = safe_df(changes)),
    registry = registry,
    specifications = economic_census_mechanism_specifications(
      control_registry = control_registry
    ),
    label = "Economic Census"
  )
}

estimate_economic_census_mechanism_models <- function(
    mechanism_panel,
    registry = economic_census_mechanism_registry(),
    cfg = list(),
    ar_points = 401L,
    control_registry = NULL) {
  estimate_posttreatment_mechanism_models(
    mechanism_panel = mechanism_panel,
    registry = registry,
    specifications = economic_census_mechanism_specifications(
      control_registry = control_registry
    ),
    cfg = cfg,
    ar_points = ar_points,
    label = "Economic Census",
    analysis_namespace = "economic_census"
  )
}

build_economic_census_diagnostics <- function(
    ec05,
    ec05_it_baseline,
    ec13,
    changes,
    district_panel,
    consumption_registry,
    cfg = list(),
    control_registry = NULL) {
  registry <- economic_census_mechanism_registry()
  mechanism_panel <- prepare_economic_census_mechanism_panel(
    district_panel,
    changes,
    registry = registry,
    control_registry = control_registry
  )
  mechanism <- estimate_economic_census_mechanism_models(
    mechanism_panel,
    registry = registry,
    cfg = cfg,
    control_registry = control_registry
  )
  it_opportunity <- diagnose_economic_census_it_opportunity(
    district_panel, ec05_it_baseline, consumption_registry, control_registry
  )
  list(
    ec05_district_measures = safe_df(ec05),
    ec05_it_baseline = safe_df(ec05_it_baseline),
    ec05_it_opportunity_specifications = it_opportunity$specifications,
    ec05_it_opportunity_estimates = it_opportunity$estimates,
    ec13_district_measures = safe_df(ec13),
    ec05_ec13_changes = safe_df(changes),
    mechanism_registry = mechanism$registry,
    mechanism_sample_coverage = mechanism$sample_coverage,
    mechanism_sample_support = mechanism$sample_support,
    mechanism_first_stage = mechanism$first_stage,
    mechanism_reduced_form = mechanism$reduced_form,
    mechanism_weak_iv = mechanism$weak_iv,
    mechanism_anderson_rubin_grid = mechanism$anderson_rubin_grid
  )
}

save_economic_census_diagnostics <- function(
    diagnostics, dir = "outputs/diagnostics/extended/economic_census") {
  measurement <- diagnostics[c(
    "ec05_district_measures", "ec05_it_baseline",
    "ec05_it_opportunity_specifications", "ec05_it_opportunity_estimates",
    "ec13_district_measures", "ec05_ec13_changes"
  )]
  c(
    write_diagnostic_bundle(measurement, dir),
    save_posttreatment_mechanism_outputs(diagnostics, dir)
  )
}
