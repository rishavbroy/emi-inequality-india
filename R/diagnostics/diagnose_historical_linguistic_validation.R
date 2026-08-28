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

historical_1991_district_geography_summary <- function(bridge) {
  bridge <- safe_df(bridge)
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
    mapping_class <- if (!length(targets)) {
      "no_deterministic_target"
    } else if (!complete) {
      "incomplete_shrid_coverage"
    } else if (length(targets) == 1L) {
      "deterministic_one_to_one"
    } else {
      "splits_across_2001_districts"
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
      preferred_language_persistence = identical(mapping_class, "deterministic_one_to_one"),
      stringsAsFactors = FALSE
    )
  }))
  out[order(out$state_code_1991, out$district_code_1991), , drop = FALSE]
}

build_historical_linguistic_geography_1991_2001 <- function(raw_sources) {
  require_historical_linguistic_shrug_sources(raw_sources)
  bridge <- build_shrug_district_bridge_1991_2001(
    raw_sources$shrug_pc91r, raw_sources$shrug_pc91u,
    raw_sources$shrug_pc01r, raw_sources$shrug_pc01u,
    raw_sources$shrug_pc91dist, raw_sources$shrug_pc01dist
  )
  list(
    bridge = bridge,
    transition = build_district_transition_1991_2001(bridge),
    source_districts = historical_1991_district_geography_summary(bridge),
    bridge_summary = summarize_shrid_bridge(bridge)
  )
}

save_historical_linguistic_geography_1991_2001 <- function(
    x, directory = "outputs/diagnostics/extended/instrument_relevance") {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    source_districts = file.path(directory, "historical_linguistic_geography_1991_2001.csv"),
    transition = file.path(directory, "historical_linguistic_transition_1991_2001.csv"),
    bridge_summary = file.path(directory, "historical_linguistic_shrid_bridge_1991_2001.csv")
  )
  write_diagnostic_csv(x$source_districts, paths[["source_districts"]])
  write_diagnostic_csv(x$transition, paths[["transition"]])
  write_diagnostic_csv(x$bridge_summary, paths[["bridge_summary"]])
  unname(paths)
}
