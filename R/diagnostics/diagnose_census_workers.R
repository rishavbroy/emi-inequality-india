# Extended diagnostics for Census 2011 local industrial and occupational structure.

summarise_census_worker_coverage <- function(industry, occupation) {
  safe_bind_rows(list(
    data.frame(
      dataset = "industry_2011_harmonized",
      n_districts = nrow(industry),
      n_positive_denominators = sum(is.finite(num(industry$workers_total)) & num(industry$workers_total) > 0),
      stringsAsFactors = FALSE
    ),
    data.frame(
      dataset = "occupation_2011_harmonized",
      n_districts = nrow(occupation),
      n_positive_denominators = sum(
        is.finite(num(occupation$workers_excl_cultivators_aglab_total)) &
          num(occupation$workers_excl_cultivators_aglab_total) > 0
      ),
      stringsAsFactors = FALSE
    )
  ))
}

build_census_worker_diagnostics <- function(
    b04_source, b06_source, b25a_source, b25b_source,
    industry_2011, occupation_2011) {
  list(
    industry_2011_harmonized = safe_df(industry_2011),
    occupation_2011_harmonized = safe_df(occupation_2011),
    coverage = summarise_census_worker_coverage(industry_2011, occupation_2011),
    b04_b25a_universe_validation = validate_census_2011_b04_b25a_universe(b04_source, b25a_source),
    b06_b25b_universe_validation = validate_census_2011_b06_b25b_universe(b06_source, b25b_source)
  )
}

save_census_worker_diagnostics <- function(
    diagnostics, dir = "outputs/diagnostics/extended/census_workers") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  files <- c(
    industry_2011_harmonized = file.path(dir, "industry_2011_harmonized_2001.csv"),
    occupation_2011_harmonized = file.path(dir, "occupation_2011_harmonized_2001.csv"),
    coverage = file.path(dir, "coverage.csv"),
    b04_b25a_universe_validation = file.path(dir, "b04_b25a_universe_validation.csv"),
    b06_b25b_universe_validation = file.path(dir, "b06_b25b_universe_validation.csv")
  )
  for (name in names(files)) {
    utils::write.csv(diagnostics[[name]], files[[name]], row.names = FALSE, na = "")
  }
  unname(files)
}
