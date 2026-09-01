# Source-first diagnostics for the 2005 SHRUG Economic Census district product.

save_economic_census_2005_diagnostics <- function(
    measures,
    path = "outputs/diagnostics/extended/economic_census/ec05_district_measures.csv") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(safe_df(measures), path, row.names = FALSE, na = "")
  path
}
