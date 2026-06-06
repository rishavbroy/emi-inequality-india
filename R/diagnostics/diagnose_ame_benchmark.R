# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' diagnose ame benchmark
#'
#' @return Function-specific return value.
diagnose_ame_benchmark <- function(selection_model, selection_data, cfg) {
  if (!diagnostic_enabled(cfg, "ame_benchmark")) return(tibble::tibble(status = "skipped"))
  data.frame(method = "autodiff_or_fallback", n = nrow(as.data.frame(selection_data)))
}

#' benchmark ame methods
#'
#' @return Function-specific return value.
benchmark_ame_methods <- function(...) {
  tibble::tibble()
}

#' benchmark parallelization options
#'
#' @return Function-specific return value.
benchmark_parallelization_options <- function(...) {
  tibble::tibble()
}

#' save ame benchmark
#'
#' @return Function-specific return value.
save_ame_benchmark <- function(benchmark) {
  benchmark
}
