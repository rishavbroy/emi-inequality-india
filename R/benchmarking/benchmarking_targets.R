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
  diagnose_ame_benchmark(
    selection_model,
    selection_data,
    with_diagnostic_enabled(cfg, "ame_benchmark")
  )
}
