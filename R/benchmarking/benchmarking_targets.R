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
  pairs <- legacy_fuzzy_candidate_pairs(district_tracker, district_join_map)
  save_fuzzy_matching_benchmark(
    summarize_threshold_sensitivity(
      pairs = pairs,
      methods = legacy_fuzzy_match_methods()
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
  legacy_output_manifest(c(
    threshold_sensitivity = write_diagnostic_csv(x, file.path(dir, "fuzzy_matching_threshold_sensitivity.csv")),
    candidate_pairs = write_diagnostic_csv(pairs, file.path(dir, "fuzzy_matching_candidate_pairs.csv"))
  ))
}

save_spatial_iv_benchmark <- function(x, dir = "outputs/benchmarking/spatial_iv") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (is.list(x) && !is.data.frame(x)) {
    paths <- c(
      status = write_diagnostic_csv(x$status %||% data.frame(), file.path(dir, "spatial_iv_status.csv")),
      augmented_panel_summary = write_diagnostic_csv(x$augmented_panel_summary %||% data.frame(), file.path(dir, "spatial_iv_augmented_panel_summary.csv")),
      model_status = write_diagnostic_csv(x$model_status %||% data.frame(), file.path(dir, "spatial_iv_model_status.csv")),
      failure_summary = write_diagnostic_csv(x$failure_summary %||% data.frame(), file.path(dir, "spatial_iv_failure_summary.csv"))
    )
  } else {
    paths <- c(status = write_diagnostic_csv(as.data.frame(x), file.path(dir, "spatial_iv_status.csv")))
  }
  legacy_output_manifest(paths)
}

save_spatial_weights_benchmark <- function(x, dir = "outputs/benchmarking/spatial_weights") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  legacy_output_manifest(c(rook_queen = write_diagnostic_csv(x, file.path(dir, "spatial_weights_rook_queen_benchmark.csv"))))
}
