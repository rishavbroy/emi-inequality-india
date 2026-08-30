# Extended diagnostics for longitudinal Census housing/living-standard measures.

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
    h09_2001, h12_2001, h13_2001, housing_2001,
    hl07_2011, hl11_2011, hl12_2011, housing_2011, housing_change) {
  list(
    housing_2001 = safe_df(housing_2001),
    housing_2011_harmonized = safe_df(housing_2011),
    housing_change_2011_2001 = safe_df(housing_change),
    coverage = summarise_census_housing_coverage(housing_2001, housing_2011, housing_change),
    change_coverage = summarise_census_housing_change_coverage(housing_change),
    source_validation_2001 = validate_census_housing_sources(h09_2001, h12_2001, h13_2001, 2001L),
    source_validation_2011 = validate_census_housing_sources(hl07_2011, hl11_2011, hl12_2011, 2011L)
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
    source_validation_2011 = file.path(dir, "source_validation_2011.csv")
  )
  for (name in names(files)) {
    utils::write.csv(diagnostics[[name]], files[[name]], row.names = FALSE, na = "")
  }
  unname(files)
}
