# Extended diagnostics for Census migration source measures.

build_census_migration_diagnostics <- function(
    d02_2001, d02_2011_source, d03_2011_source, d02_2011, d03_2011) {
  list(
    d02_2001 = safe_df(d02_2001),
    d02_2011_harmonized = safe_df(d02_2011),
    d03_2011_harmonized = safe_df(d03_2011),
    coverage = summarise_census_migration_coverage(d02_2001, d02_2011, d03_2011),
    d02_d03_2011_total_validation = validate_census_2011_migration_totals(
      d02_2011_source, d03_2011_source
    )
  )
}

save_census_migration_diagnostics <- function(
    diagnostics, dir = "outputs/diagnostics/extended/census_migration") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  files <- c(
    d02_2001 = file.path(dir, "d02_2001.csv"),
    d02_2011_harmonized = file.path(dir, "d02_2011_harmonized_2001.csv"),
    d03_2011_harmonized = file.path(dir, "d03_2011_harmonized_2001.csv"),
    coverage = file.path(dir, "coverage.csv"),
    d02_d03_2011_total_validation = file.path(dir, "d02_d03_2011_total_validation.csv")
  )
  for (name in names(files)) {
    utils::write.csv(diagnostics[[name]], files[[name]], row.names = FALSE, na = "")
  }
  unname(files)
}
