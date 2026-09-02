# Primary-source validation of concept-matched Census-1991 historical controls.
# The official ORGI tables validate source counts before any 1991->2001
# geography allocation. They do not redefine the causal-control family.

census_1991_primary_validation_registry <- function() {
  data.frame(
    measure_id = c(
      "population_c02t", "urban_population_c02u", "secondary_plus_c02t",
      "main_workers_b01s", "dependent_population_c06t",
      "working_age_population_c06t", "muslim_population_c09t",
      "religion_sum_c09t", "population_c09t"
    ),
    source_id = c(
      "C02T", "C02U", "C02T", "B01S", "C06T", "C06T", "C09T", "C09T", "C09T"
    ),
    official_field = c(
      "population_c02t_1991_count", "urban_population_c02u_1991_count",
      "secondary_plus_c02t_1991_count", "main_workers_b01s_1991_count",
      "dependent_population_c06t_1991_count", "working_age_population_c06t_1991_count",
      "muslim_population_c09t_1991_count", "religion_population_sum_c09t_1991_count",
      "population_c09t_1991_count"
    ),
    reference_field = c(
      "population_1991_count", "urban_population_1991_count",
      "matriculate_plus_1991_count", "main_workers_1991_count",
      "dependent_population_1991_count", "working_age_population_1991_count",
      "muslim_population_1991_count", "population_1991_count", "population_1991_count"
    ),
    exact_required = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    scientific_role = c(
      "population anchor", "urbanization", "human capital", "worker status",
      "dependency numerator", "dependency denominator", "religion",
      "religion-category population accounting", "published C-09 population diagnostic"
    ),
    stringsAsFactors = FALSE
  )
}

census_1991_primary_validation_reference <- function(vanneman_counts) {
  x <- validate_census_1991_district_keys(
    safe_df(vanneman_counts), "Vanneman 1991 primary-validation reference"
  )
  required <- c(
    "population_1991_count", "rural_population_1991_count",
    "matriculate_plus_1991_count", "main_workers_1991_count",
    "dependent_population_1991_count", "working_age_population_1991_count",
    "muslim_population_1991_count"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Vanneman primary-validation reference lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  x$urban_population_1991_count <-
    num(x$population_1991_count) - num(x$rural_population_1991_count)
  x
}

census_1991_primary_validation_sources <- function(b01s, c02t, c02u, c06t, c09t) {
  list(
    B01S = validate_census_1991_district_keys(b01s, "Census 1991 B-01(S)"),
    C02T = validate_census_1991_district_keys(c02t, "Census 1991 C-02 total"),
    C02U = validate_census_1991_district_keys(c02u, "Census 1991 C-02 urban"),
    C06T = validate_census_1991_district_keys(c06t, "Census 1991 C-06"),
    C09T = validate_census_1991_district_keys(c09t, "Census 1991 C-09")
  )
}

census_1991_primary_validation_coverage <- function(sources, reference) {
  reference_keys <- census_1991_district_key(reference)
  safe_bind_rows(lapply(names(sources), function(source_id) {
    source <- sources[[source_id]]
    source_keys <- census_1991_district_key(source)
    data.frame(
      source_id = source_id,
      official_districts = length(source_keys),
      vanneman_districts = length(reference_keys),
      overlapping_districts = length(intersect(source_keys, reference_keys)),
      official_only_districts = length(setdiff(source_keys, reference_keys)),
      vanneman_only_districts = length(setdiff(reference_keys, source_keys)),
      stringsAsFactors = FALSE
    )
  }))
}

compare_census_1991_primary_measure <- function(source, reference, spec) {
  keys <- census_1991_keys()
  official_field <- spec$official_field[[1L]]
  reference_field <- spec$reference_field[[1L]]
  if (!official_field %in% names(source)) {
    stop("Census 1991 validation source lacks `", official_field, "`.", call. = FALSE)
  }
  if (!reference_field %in% names(reference)) {
    stop("Census 1991 validation reference lacks `", reference_field, "`.", call. = FALSE)
  }
  official <- source[c(keys, official_field)]
  names(official)[[3L]] <- "official_value"
  reference_part <- reference[c(keys, reference_field)]
  names(reference_part)[[3L]] <- "reference_value"
  joined <- merge(reference_part, official, by = keys, all.x = TRUE, sort = FALSE)

  # C-02U omits districts with no urban population. Treat an absent published
  # urban row as a structural zero only when the independent Vanneman count is zero.
  structural_zero <- spec$measure_id[[1L]] == "urban_population_c02u" &
    !is.finite(num(joined$official_value)) & num(joined$reference_value) == 0
  joined$official_value[structural_zero] <- 0
  joined$official_status <- ifelse(
    structural_zero, "structural_zero_no_urban_row",
    ifelse(is.finite(num(joined$official_value)), "observed", "missing")
  )
  joined$measure_id <- spec$measure_id[[1L]]
  joined$source_id <- spec$source_id[[1L]]
  joined$exact_required <- spec$exact_required[[1L]]
  joined$official_value <- num(joined$official_value)
  joined$reference_value <- num(joined$reference_value)
  joined$absolute_difference <- abs(joined$official_value - joined$reference_value)
  joined$exact_match <- is.finite(joined$absolute_difference) & joined$absolute_difference == 0
  joined[c(
    "measure_id", "source_id", keys, "official_status", "official_value",
    "reference_value", "absolute_difference", "exact_match", "exact_required"
  )]
}

summarize_census_1991_primary_comparison <- function(comparisons, registry) {
  safe_bind_rows(lapply(seq_len(nrow(registry)), function(i) {
    spec <- registry[i, , drop = FALSE]
    x <- comparisons[comparisons$measure_id == spec$measure_id[[1L]], , drop = FALSE]
    complete <- is.finite(x$official_value) & is.finite(x$reference_value)
    correlation <- if (sum(complete) >= 2L && stats::sd(x$official_value[complete]) > 0 &&
                       stats::sd(x$reference_value[complete]) > 0) {
      stats::cor(x$official_value[complete], x$reference_value[complete])
    } else {
      NA_real_
    }
    differences <- x$absolute_difference[complete]
    data.frame(
      measure_id = spec$measure_id[[1L]],
      source_id = spec$source_id[[1L]],
      scientific_role = spec$scientific_role[[1L]],
      exact_required = spec$exact_required[[1L]],
      reference_districts = nrow(x),
      compared_districts = sum(complete),
      exact_matches = sum(x$exact_match %in% TRUE, na.rm = TRUE),
      exact_match_share = if (sum(complete)) mean(x$exact_match[complete]) else NA_real_,
      correlation = correlation,
      median_abs_difference = if (length(differences)) stats::median(differences) else NA_real_,
      max_abs_difference = if (length(differences)) max(differences) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}

validate_census_1991_primary_comparison <- function(summary) {
  x <- safe_df(summary)
  required <- x$exact_required %in% TRUE
  failed <- required & (
    x$compared_districts != x$reference_districts |
      x$exact_matches != x$reference_districts
  )
  if (any(failed)) {
    stop(
      "Official Census 1991 primary validation failed exact source contracts: ",
      paste(x$measure_id[failed], collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

build_census_1991_primary_validation <- function(
    b01s, c02t, c02u, c06t, c09t, vanneman_counts) {
  registry <- census_1991_primary_validation_registry()
  sources <- census_1991_primary_validation_sources(b01s, c02t, c02u, c06t, c09t)
  reference <- census_1991_primary_validation_reference(vanneman_counts)
  comparisons <- safe_bind_rows(lapply(seq_len(nrow(registry)), function(i) {
    spec <- registry[i, , drop = FALSE]
    compare_census_1991_primary_measure(sources[[spec$source_id[[1L]]]], reference, spec)
  }))
  summary <- summarize_census_1991_primary_comparison(comparisons, registry)
  validate_census_1991_primary_comparison(summary)
  discrepancies <- comparisons[
    is.finite(comparisons$absolute_difference) & comparisons$absolute_difference != 0,
    , drop = FALSE
  ]
  list(
    registry = registry,
    source_coverage = census_1991_primary_validation_coverage(sources, reference),
    comparison_summary = summary,
    discrepancies = discrepancies
  )
}

save_census_1991_primary_validation <- function(
    x, root = "outputs/diagnostics/extended/instrument_relevance") {
  write_diagnostic_bundle(
    x,
    root,
    filenames = c(
      registry = "census_1991_primary_validation_registry.csv",
      source_coverage = "census_1991_primary_validation_coverage.csv",
      comparison_summary = "census_1991_primary_validation_summary.csv",
      discrepancies = "census_1991_primary_validation_discrepancies.csv"
    )
  )
}
