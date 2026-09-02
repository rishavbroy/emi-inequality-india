# Extended diagnostics for Census 2011 household human-capital and worker-intensity measures.

build_census_household_diagnostics <- function(hh08, hh10, hh11, household_2011) {
  list(
    household_2011_harmonized_2001 = safe_df(household_2011),
    source_validation_2011 = validate_census_2011_household_sources(hh08, hh10, hh11)
  )
}

save_census_household_diagnostics <- function(
    diagnostics, dir = "outputs/diagnostics/extended/census_households") {
  write_diagnostic_bundle(
    list(
      household_2011_harmonized_2001 = safe_df(diagnostics$household_2011_harmonized_2001),
      source_validation_2011 = safe_df(diagnostics$source_validation_2011)
    ),
    dir
  )
}
