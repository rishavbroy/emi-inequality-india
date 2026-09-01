# Source-first diagnostics for SHRUG Economic Census district products.

save_economic_census_2005_diagnostics <- function(
    measures,
    path = "outputs/diagnostics/extended/economic_census/ec05_district_measures.csv") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(safe_df(measures), path, row.names = FALSE, na = "")
  path
}

save_economic_census_2013_diagnostics <- function(
    measures,
    path = "outputs/diagnostics/extended/economic_census/ec13_district_measures.csv") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(safe_df(measures), path, row.names = FALSE, na = "")
  path
}

save_economic_census_change_diagnostics <- function(
    changes,
    path = "outputs/diagnostics/extended/economic_census/ec05_ec13_changes.csv") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(safe_df(changes), path, row.names = FALSE, na = "")
  path
}
