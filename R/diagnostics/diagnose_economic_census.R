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
    label = "Economic Census"
  )
}

build_economic_census_diagnostics <- function(
    ec05,
    ec13,
    changes,
    district_panel,
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
  list(
    ec05_district_measures = safe_df(ec05),
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
    diagnostics,
    dir = "outputs/diagnostics/extended/economic_census") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  files <- c(
    ec05_district_measures = file.path(dir, "ec05_district_measures.csv"),
    ec13_district_measures = file.path(dir, "ec13_district_measures.csv"),
    ec05_ec13_changes = file.path(dir, "ec05_ec13_changes.csv"),
    mechanism_registry = file.path(dir, "mechanism_registry.csv"),
    mechanism_sample_coverage = file.path(dir, "mechanism_sample_coverage.csv"),
    mechanism_sample_support = file.path(dir, "mechanism_sample_support.csv"),
    mechanism_first_stage = file.path(dir, "mechanism_first_stage.csv"),
    mechanism_reduced_form = file.path(dir, "mechanism_reduced_form.csv"),
    mechanism_weak_iv = file.path(dir, "mechanism_weak_iv.csv"),
    mechanism_anderson_rubin_grid = file.path(dir, "mechanism_anderson_rubin_grid.csv")
  )
  for (name in names(files)) {
    utils::write.csv(diagnostics[[name]], files[[name]], row.names = FALSE, na = "")
  }
  unname(files)
}
