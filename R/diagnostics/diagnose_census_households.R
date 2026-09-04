# Extended diagnostics for longitudinal Census household human-capital and worker-intensity measures.

summarise_census_household_change_coverage <- function(household_change) {
  x <- safe_df(household_change)
  safe_bind_rows(lapply(census_household_longitudinal_share_columns(), function(variable) {
    data.frame(
      variable = variable,
      n_harmonized_parents = nrow(x),
      n_finite_baseline = sum(is.finite(num(x[[paste0(variable, "_2001")]]))),
      n_finite_followup = sum(is.finite(num(x[[paste0(variable, "_2011")]]))),
      n_finite_change = sum(is.finite(num(x[[paste0(variable, "_change_2011_2001")]]))),
      stringsAsFactors = FALSE
    )
  }))
}

build_census_household_diagnostics <- function(
    hh09, hh13, hh15, hh15a, household_2001,
    hh08, hh10, hh11, household_2011, household_change) {
  list(
    household_2001 = safe_df(household_2001),
    household_2011_harmonized_2001 = safe_df(household_2011),
    household_change_2011_2001 = safe_df(household_change),
    change_coverage = summarise_census_household_change_coverage(household_change),
    source_validation_2001 = validate_census_2001_household_sources(hh09, hh13, hh15, hh15a),
    source_validation_2011 = validate_census_2011_household_sources(hh08, hh10, hh11)
  )
}

save_census_household_diagnostics <- function(
    diagnostics, dir = "outputs/diagnostics/extended/census_households") {
  write_diagnostic_bundle(
    diagnostics[c(
      "household_2001", "household_2011_harmonized_2001", "household_change_2011_2001",
      "change_coverage", "source_validation_2001", "source_validation_2011"
    )],
    dir
  )
}
