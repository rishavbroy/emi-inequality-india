# Extended diagnostics for Census 2011 household human-capital and worker-intensity measures.

build_census_household_diagnostics <- function(hh08, hh10, hh11, household_2011) {
  list(
    household_2011_harmonized_2001 = safe_df(household_2011),
    source_validation_2011 = validate_census_2011_household_sources(hh08, hh10, hh11)
  )
}

save_census_household_diagnostics <- function(diagnostics, dir = "outputs/diagnostics/extended/census_households") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    household_2011_harmonized_2001 = file.path(dir, "household_2011_harmonized_2001.csv"),
    source_validation_2011 = file.path(dir, "source_validation_2011.csv")
  )
  for (name in names(paths)) utils::write.csv(safe_df(diagnostics[[name]]), paths[[name]], row.names = FALSE, na = "")
  unname(paths)
}
