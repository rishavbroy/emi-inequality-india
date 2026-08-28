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
  bridge <- safe_df(bridge)
  if (!is.numeric(min_population_coverage) || length(min_population_coverage) != 1L ||
      !is.finite(min_population_coverage) || min_population_coverage <= 0 || min_population_coverage > 1) {
    stop("Historical linguistic geography population coverage must be in (0, 1].", call. = FALSE)
  }
  required <- c(
    "shrid2", "state_code_1991", "district_code_1991",
    "state_code_2001", "district_code_2001", "deterministic",
    "population"
  )
  missing <- setdiff(required, names(bridge))
  if (length(missing)) {
    stop("1991 SHRUG bridge lacks geography-summary fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  source <- bridge[
    !is.na(bridge$state_code_1991) & !is.na(bridge$district_code_1991),
    , drop = FALSE
  ]
  if (!nrow(source)) return(data.frame())

  key <- interaction(source$state_code_1991, source$district_code_1991, drop = TRUE)
  out <- safe_bind_rows(lapply(split(seq_len(nrow(source)), key), function(i) {
    part <- source[i, , drop = FALSE]
    deterministic <- part$deterministic %in% TRUE &
      !is.na(part$state_code_2001) & !is.na(part$district_code_2001)
    targets <- unique(paste(
      part$state_code_2001[deterministic],
      part$district_code_2001[deterministic],
      sep = "__"
    ))
    n_total <- length(unique(part$shrid2))
    n_mapped <- length(unique(part$shrid2[deterministic]))
    pop_total <- sum_finite_or_na(part$population)
    pop_mapped <- sum_finite_or_na(part$population[deterministic])
    shrid_coverage <- if (n_total > 0L) n_mapped / n_total else NA_real_
    population_coverage <- if (is.finite(pop_total) && pop_total > 0) pop_mapped / pop_total else NA_real_
    complete <- is.finite(shrid_coverage) && abs(shrid_coverage - 1) <= 1e-12
    one_target <- length(targets) == 1L
    high_population_coverage <- is.finite(population_coverage) &&
      population_coverage >= min_population_coverage
    mapping_class <- if (!length(targets)) {
      "no_deterministic_target"
    } else if (length(targets) > 1L) {
      "splits_across_2001_districts"
    } else if (complete) {
      "deterministic_one_to_one"
    } else if (high_population_coverage) {
      "high_population_coverage_single_target"
    } else {
      "incomplete_population_coverage_single_target"
    }
    data.frame(
      state_code_1991 = part$state_code_1991[[1L]],
      district_code_1991 = part$district_code_1991[[1L]],
      n_shrid_total = n_total,
      n_shrid_deterministic = n_mapped,
      shrid_coverage = shrid_coverage,
      population_1991_total = pop_total,
      population_1991_deterministic = pop_mapped,
      population_coverage = population_coverage,
      n_target_2001_districts = length(targets),
      mapping_class = mapping_class,
      exact_language_persistence = identical(mapping_class, "deterministic_one_to_one"),
      preferred_language_persistence = one_target && high_population_coverage,
      preferred_population_coverage_threshold = min_population_coverage,
      stringsAsFactors = FALSE
    )
  }))
  out[order(out$state_code_1991, out$district_code_1991), , drop = FALSE]
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
