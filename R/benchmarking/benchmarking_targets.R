# Benchmarking target adapters.
#
# Long-running tuning and runtime comparisons live behind opt-in benchmark
# targets so ordinary public builds stay fast while the research workflow remains
# reproducible and visible.

with_diagnostic_enabled <- function(cfg, name) {
  if (is.null(cfg$run_diagnostics)) cfg$run_diagnostics <- list()
  cfg$run_diagnostics[[name]] <- TRUE
  cfg
}

run_ame_methods_benchmark <- function(selection_model, selection_data, cfg) {
  save_ame_benchmark(
    diagnose_ame_benchmark(
      selection_model,
      selection_data,
      with_diagnostic_enabled(cfg, "ame_benchmark")
    )
  )
}


run_fuzzy_matching_benchmark <- function(district_tracker = data.frame(), district_join_map = data.frame(), cfg = list()) {
  pairs <- fuzzy_candidate_pairs(district_tracker, district_join_map)
  save_fuzzy_matching_benchmark(
    summarize_threshold_sensitivity(
      pairs = pairs,
      methods = district_fuzzy_match_methods()
    ),
    pairs = pairs
  )
}

run_spatial_iv_benchmark <- function(district_panel, spatial_weights, cfg) {
  save_spatial_iv_benchmark(
    estimate_spatial_iv_experimental(district_panel, spatial_weights, cfg)
  )
}

run_spatial_weights_benchmark <- function(district_panel, cfg) {
  save_spatial_weights_benchmark(compare_rook_queen_contiguity(district_panel))
}

save_fuzzy_matching_benchmark <- function(x, pairs = data.frame(), dir = "outputs/benchmarking/fuzzy_matching") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  output_manifest(c(
    threshold_sensitivity = write_diagnostic_csv(x, file.path(dir, "fuzzy_matching_threshold_sensitivity.csv")),
    candidate_pairs = write_diagnostic_csv(pairs, file.path(dir, "fuzzy_matching_candidate_pairs.csv")),
    candidate_pair_coverage = write_diagnostic_csv(summarize_fuzzy_candidate_pair_coverage(pairs), file.path(dir, "fuzzy_matching_candidate_pair_coverage.csv")),
    legacy_tuning_reference = write_diagnostic_csv(fuzzy_tuning_reference(), file.path(dir, "fuzzy_matching_legacy_tuning_reference.csv"))
  ))
}

save_spatial_iv_benchmark <- function(x, dir = "outputs/benchmarking/spatial_iv") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (is.list(x) && !is.data.frame(x)) {
    paths <- c(
      status = write_diagnostic_csv(x$status %||% data.frame(), file.path(dir, "spatial_iv_status.csv")),
      augmented_panel_summary = write_diagnostic_csv(x$augmented_panel_summary %||% data.frame(), file.path(dir, "spatial_iv_augmented_panel_summary.csv")),
      model_status = write_diagnostic_csv(x$model_status %||% data.frame(), file.path(dir, "spatial_iv_model_status.csv")),
      coefficient_summary = write_diagnostic_csv(x$coefficient_summary %||% data.frame(), file.path(dir, "spatial_iv_coefficient_summary.csv")),
      clustered_coefficient_summary = write_diagnostic_csv(x$clustered_coefficient_summary %||% data.frame(), file.path(dir, "spatial_iv_clustered_coefficient_summary.csv")),
      diagnostics_summary = write_diagnostic_csv(x$diagnostics_summary %||% data.frame(), file.path(dir, "spatial_iv_diagnostics_summary.csv")),
      failure_summary = write_diagnostic_csv(x$failure_summary %||% data.frame(), file.path(dir, "spatial_iv_failure_summary.csv"))
    )
  } else {
    paths <- c(status = write_diagnostic_csv(as.data.frame(x), file.path(dir, "spatial_iv_status.csv")))
  }
  output_manifest(paths)
}

save_spatial_weights_benchmark <- function(x, dir = "outputs/benchmarking/spatial_weights") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  output_manifest(c(rook_queen = write_diagnostic_csv(x, file.path(dir, "spatial_weights_rook_queen_benchmark.csv"))))
}

benchmark_consumption_distribution_domains <- function(
    lineaged_households, registry, max_districts = 8L) {
  rules <- consumption_welfare_registry_partition(registry, "distributional")
  rules <- rules[plain_chr(rules$estimand) == "survey_gini", , drop = FALSE]
  if (!nrow(rules)) {
    return(data.frame(
      mode = "unavailable", elapsed_seconds = NA_real_, n_districts = 0L,
      max_abs_estimate_diff = NA_real_, max_abs_se_diff = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  x <- consumption_design_rows(lineaged_households)
  districts <- head(sort(unique(plain_chr(x$target_unit_2001))), as.integer(max_districts))
  x <- x[plain_chr(x$target_unit_2001) %in% districts, , drop = FALSE]
  direct_households <- lineaged_households[
    plain_chr(lineaged_households$target_unit_2001) %in% districts,
    , drop = FALSE
  ]

  run_mode <- function(cores) {
    old <- Sys.getenv("EMI_CONSUMPTION_DOMAIN_CORES", unset = NA_character_)
    Sys.setenv(EMI_CONSUMPTION_DOMAIN_CORES = as.character(cores))
    on.exit({
      if (is.na(old)) Sys.unsetenv("EMI_CONSUMPTION_DOMAIN_CORES") else
        Sys.setenv(EMI_CONSUMPTION_DOMAIN_CORES = old)
    }, add = TRUE)
    result <- NULL
    elapsed <- system.time({
      result <- estimate_consumption_district_welfare_distributional(
        direct_households, rules
      )
    })[["elapsed"]]
    list(result = result, elapsed = unname(elapsed))
  }

  serial <- run_mode(1L)
  configured <- run_mode(consumption_domain_cores())
  left <- serial$result[order(serial$result$district_2001), , drop = FALSE]
  right <- configured$result[order(configured$result$district_2001), , drop = FALSE]
  if (!identical(plain_chr(left$district_2001), plain_chr(right$district_2001))) {
    stop("Consumption distribution benchmark returned different district domains.", call. = FALSE)
  }
  max_estimate_diff <- max(abs(num(left$estimate) - num(right$estimate)), na.rm = TRUE)
  max_se_diff <- max(abs(num(left$std_error) - num(right$std_error)), na.rm = TRUE)
  if (!is.finite(max_estimate_diff)) max_estimate_diff <- NA_real_
  if (!is.finite(max_se_diff)) max_se_diff <- NA_real_

  data.frame(
    mode = c("serial", "configured"),
    elapsed_seconds = c(serial$elapsed, configured$elapsed),
    n_districts = length(districts),
    requested_cores = c(1L, consumption_domain_cores()),
    max_abs_estimate_diff = c(0, max_estimate_diff),
    max_abs_se_diff = c(0, max_se_diff),
    stringsAsFactors = FALSE
  )
}

run_consumption_distribution_benchmark <- function(
    lineaged_households, registry,
    path = "outputs/benchmarking/consumption_distribution_domains.csv") {
  write_diagnostic_csv(
    benchmark_consumption_distribution_domains(lineaged_households, registry),
    path
  )
}
