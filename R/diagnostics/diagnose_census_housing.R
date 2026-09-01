# Extended diagnostics for longitudinal Census housing/living-standard measures.

census_housing_mechanism_registry <- function() {
  data.frame(
    outcome_id = c(
      "electricity_access_change",
      "no_lighting_change",
      "banking_access_change",
      "television_access_change",
      "telephone_access_change",
      "bicycle_ownership_change",
      "motorcycle_ownership_change",
      "car_ownership_change"
    ),
    source_id = rep("change", 8L),
    variable = c(
      "electricity_share_households_change_2011_2001",
      "no_lighting_share_households_change_2011_2001",
      "banking_share_households_change_2011_2001",
      "television_share_households_change_2011_2001",
      "telephone_share_households_change_2011_2001",
      "bicycle_share_households_change_2011_2001",
      "motorcycle_share_households_change_2011_2001",
      "car_share_households_change_2011_2001"
    ),
    mechanism_family = c(
      "basic_services", "basic_services", "financial_access",
      "communications_assets", "communications_assets",
      "transport_assets", "transport_assets", "transport_assets"
    ),
    tier = c(
      "core", "secondary", "core", "secondary",
      "core", "secondary", "core", "core"
    ),
    denominator = rep("households", 8L),
    stringsAsFactors = FALSE
  )
}

census_housing_mechanism_specifications <- function(
    outcome = "electricity_share_households_change_2011_2001",
    treatment = preferred_iv_variables()$treatment,
    control_registry = NULL) {
  census_mechanism_specifications(
    outcome = outcome,
    treatment = treatment,
    sample_rule = "housing_change_mechanism_common_support",
    control_registry = control_registry
  )
}

prepare_census_housing_mechanism_panel <- function(
    district_panel, housing_change, registry = census_housing_mechanism_registry(),
    control_registry = NULL) {
  prepare_census_mechanism_panel(
    district_panel = district_panel,
    sources = list(change = safe_df(housing_change)),
    registry = registry,
    specifications = census_housing_mechanism_specifications(
      control_registry = control_registry
    ),
    label = "Census housing"
  )
}

estimate_census_housing_mechanism_models <- function(
    mechanism_panel, registry = census_housing_mechanism_registry(),
    cfg = list(), ar_points = 401L, control_registry = NULL) {
  estimate_census_mechanism_models(
    mechanism_panel = mechanism_panel,
    registry = registry,
    specifications = census_housing_mechanism_specifications(
      control_registry = control_registry
    ),
    cfg = cfg,
    ar_points = ar_points,
    label = "Census housing"
  )
}

summarise_census_housing_coverage <- function(housing_2001, housing_2011, housing_change) {
  safe_bind_rows(list(
    data.frame(
      dataset = "housing_2001",
      n_districts = nrow(housing_2001),
      n_positive_denominators = sum(is.finite(num(housing_2001$households_total)) & num(housing_2001$households_total) > 0),
      stringsAsFactors = FALSE
    ),
    data.frame(
      dataset = "housing_2011_harmonized",
      n_districts = nrow(housing_2011),
      n_positive_denominators = sum(is.finite(num(housing_2011$households_total)) & num(housing_2011$households_total) > 0),
      stringsAsFactors = FALSE
    ),
    data.frame(
      dataset = "housing_change_2011_2001",
      n_districts = nrow(housing_change),
      n_positive_denominators = sum(is.finite(num(housing_change$households_total_2011)) & num(housing_change$households_total_2011) > 0),
      stringsAsFactors = FALSE
    )
  ))
}


summarise_census_housing_change_coverage <- function(housing_change) {
  x <- safe_df(housing_change)
  variables <- census_housing_common_share_columns()
  safe_bind_rows(lapply(variables, function(variable) {
    change <- paste0(variable, "_change_2011_2001")
    data.frame(
      variable = variable,
      n_harmonized_parents = nrow(x),
      n_finite_baseline = sum(is.finite(num(x[[paste0(variable, "_2001")]]))),
      n_finite_followup = sum(is.finite(num(x[[paste0(variable, "_2011")]]))),
      n_finite_change = sum(is.finite(num(x[[change]]))),
      stringsAsFactors = FALSE
    )
  }))
}

build_census_housing_diagnostics <- function(
    h05_2001, h08_2001, h09_2001, h10_2001, h11_2001, h12_2001, h13_2001, housing_2001,
    hl04_2011, hl06_2011, hl07_2011, hl08_2011, hl09_2011, hl10_2011,
    hl11_2011, hl12_2011, hl13_2011, housing_2011, housing_change,
    district_panel, cfg = list(), control_registry = NULL) {
  mechanism_registry <- census_housing_mechanism_registry()
  mechanism_panel <- prepare_census_housing_mechanism_panel(
    district_panel, housing_change, mechanism_registry,
    control_registry = control_registry
  )
  mechanism <- estimate_census_housing_mechanism_models(
    mechanism_panel, mechanism_registry, cfg = cfg,
    control_registry = control_registry
  )
  list(
    housing_2001 = safe_df(housing_2001),
    housing_2011_harmonized = safe_df(housing_2011),
    housing_change_2011_2001 = safe_df(housing_change),
    coverage = summarise_census_housing_coverage(housing_2001, housing_2011, housing_change),
    change_coverage = summarise_census_housing_change_coverage(housing_change),
    source_validation_2001 = validate_census_housing_sources(
      h09_2001, h12_2001, h13_2001, 2001L, h05_2001, h08_2001,
      h10_2001, NULL, h11_2001
    ),
    source_validation_2011 = validate_census_housing_sources(
      hl07_2011, hl11_2011, hl12_2011, 2011L, hl04_2011, hl06_2011,
      hl08_2011, hl09_2011, hl10_2011, hl13_2011
    ),
    mechanism_registry = mechanism$registry,
    mechanism_sample_coverage = mechanism$sample_coverage,
    mechanism_sample_support = mechanism$sample_support,
    mechanism_first_stage = mechanism$first_stage,
    mechanism_reduced_form = mechanism$reduced_form,
    mechanism_weak_iv = mechanism$weak_iv,
    mechanism_anderson_rubin_grid = mechanism$anderson_rubin_grid
  )
}

save_census_housing_diagnostics <- function(
    diagnostics, dir = "outputs/diagnostics/extended/census_housing") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  files <- c(
    housing_2001 = file.path(dir, "housing_2001.csv"),
    housing_2011_harmonized = file.path(dir, "housing_2011_harmonized_2001.csv"),
    housing_change_2011_2001 = file.path(dir, "housing_change_2011_2001.csv"),
    coverage = file.path(dir, "coverage.csv"),
    change_coverage = file.path(dir, "change_coverage.csv"),
    source_validation_2001 = file.path(dir, "source_validation_2001.csv"),
    source_validation_2011 = file.path(dir, "source_validation_2011.csv"),
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
