# Historical linguistic-instrument geography diagnostics.

historical_linguistic_shrug_source_ids <- function() {
  c(
    "shrug_pc91r", "shrug_pc91u", "shrug_pc91dist",
    "shrug_pc01r", "shrug_pc01u", "shrug_pc01dist"
  )
}

require_historical_linguistic_shrug_sources <- function(raw_sources) {
  missing <- setdiff(historical_linguistic_shrug_source_ids(), names(raw_sources))
  if (length(missing)) {
    stop(
      "Historical linguistic geography is missing SHRUG sources: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

historical_1991_district_geography_summary <- function(bridge, min_population_coverage = 0.99) {
  out <- summarize_shrug_source_district_mapping(
    bridge,
    source_year = 1991L,
    target_year = 2001L,
    min_population_coverage = min_population_coverage
  )
  if (!nrow(out)) return(out)
  out$mapping_class[out$mapping_class == "splits_across_target_districts"] <-
    "splits_across_2001_districts"
  names(out)[names(out) == "population_total"] <- "population_1991_total"
  names(out)[names(out) == "population_deterministic"] <- "population_1991_deterministic"
  names(out)[names(out) == "n_target_districts"] <- "n_target_2001_districts"
  names(out)[names(out) == "exact_one_to_one"] <- "exact_language_persistence"
  names(out)[names(out) == "preferred_single_target"] <- "preferred_language_persistence"
  out
}

historical_linguistic_geography_sensitivity <- function(
    source_districts, thresholds = c(0.95, 0.98, 0.99, 0.995, 0.999, 1)) {
  x <- safe_df(source_districts)
  required <- c("population_coverage", "n_target_2001_districts")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Historical linguistic geography sensitivity lacks fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  safe_bind_rows(lapply(thresholds, function(threshold) {
    eligible <- x$n_target_2001_districts == 1L &
      is.finite(x$population_coverage) & x$population_coverage >= threshold
    data.frame(
      population_coverage_threshold = threshold,
      eligible_districts = sum(eligible),
      eligible_population_1991 = sum(num(x$population_1991_total[eligible]), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
}

build_historical_linguistic_geography_1991_2001 <- function(raw_sources) {
  require_historical_linguistic_shrug_sources(raw_sources)
  bridge <- build_shrug_district_bridge_1991_2001(
    raw_sources$shrug_pc91r, raw_sources$shrug_pc91u,
    raw_sources$shrug_pc01r, raw_sources$shrug_pc01u,
    raw_sources$shrug_pc91dist, raw_sources$shrug_pc01dist
  )
  source_districts <- historical_1991_district_geography_summary(bridge)
  list(
    bridge = bridge,
    transition = build_district_transition_1991_2001(bridge),
    source_districts = source_districts,
    coverage_sensitivity = historical_linguistic_geography_sensitivity(source_districts),
    bridge_summary = summarize_shrid_bridge(bridge)
  )
}

save_historical_linguistic_geography_1991_2001 <- function(
    x, directory = "outputs/diagnostics/extended/instrument_relevance") {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    source_districts = file.path(directory, "historical_linguistic_geography_1991_2001.csv"),
    coverage_sensitivity = file.path(directory, "historical_linguistic_geography_coverage_sensitivity.csv"),
    transition = file.path(directory, "historical_linguistic_transition_1991_2001.csv"),
    bridge_summary = file.path(directory, "historical_linguistic_shrid_bridge_1991_2001.csv")
  )
  write_diagnostic_csv(x$source_districts, paths[["source_districts"]])
  write_diagnostic_csv(x$coverage_sensitivity, paths[["coverage_sensitivity"]])
  write_diagnostic_csv(x$transition, paths[["transition"]])
  write_diagnostic_csv(x$bridge_summary, paths[["bridge_summary"]])
  unname(paths)
}
