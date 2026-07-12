# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

#' diagnose ame benchmark
#'
#' Port the legacy Chunk 10 AME timing/tuning code into opt-in benchmarks.  The
#' legacy final method used `avg_slopes(..., vcov = TRUE, type = "response")`
#' over all complete cases with `marginaleffects_parallel = FALSE`, and recorded
#' exploratory timings for subsampling, forward differences, split slopes vs.
#' comparisons, and failed future-based parallelization.  This function records
#' those choices and, when packages/model support it, runs small reproducible
#' timing checks behind the benchmark target.
diagnose_ame_benchmark <- function(selection_model, selection_data, cfg) {
  if (!diagnostic_enabled(cfg, "ame_benchmark")) return(tibble::tibble(status = "skipped"))
  notes <- legacy_ame_benchmark_notes()
  methods <- benchmark_ame_methods(selection_model, selection_data, cfg)
  parallel <- benchmark_parallelization_options(selection_model, selection_data, cfg)
  out <- list(methods = methods, parallel = parallel, notes = notes)
  class(out) <- c("emi_ame_benchmark", class(out))
  out
}

legacy_ame_benchmark_notes <- function() {
  data.frame(
    topic = c(
      "final_method",
      "sample_seed",
      "full_data_legacy_timing",
      "subsample_20000_legacy_timing",
      "subsample_2000_legacy_timing",
      "forward_difference_result",
      "split_slopes_comparisons_result",
      "parallelization_failure"
    ),
    legacy_choice = c(
      "avg_slopes(model_probit_selection, newdata = sel_data, wts = 'weight', vcov = TRUE, type = 'response'); marginaleffects_parallel = FALSE",
      "set.seed(999)",
      "legacy notes: full data first run around 6-11 minutes, later around 4.85 minutes; raw derivation once took ~40 minutes",
      "legacy notes: around 36-39 seconds",
      "legacy notes: around 4-5 seconds",
      "fdforward did not materially help; centered differences retained for accuracy on continuous variables",
      "splitting numeric slopes and factor comparisons did not help; use avg_slopes()",
      "future/marginaleffects parallelization exceeded available memory because exported globals were tens of GiB"
    ),
    stringsAsFactors = FALSE
  )
}

ame_env_flag_enabled <- function(name, default = FALSE) {
  value <- tolower(trimws(Sys.getenv(name, if (isTRUE(default)) "true" else "false")))
  !value %in% c("0", "false", "no", "off", "")
}

ame_benchmark_sample_sizes <- function(n_observations, cfg = list()) {
  configured <- cfg$diagnostics$ame_benchmark_sample_sizes %||% cfg$ame_benchmark_sample_sizes %||% NULL
  env <- Sys.getenv("EMI_AME_BENCHMARK_SAMPLE_SIZES", unset = "")
  if (nzchar(env)) configured <- strsplit(env, ",", fixed = TRUE)[[1]]
  if (is.null(configured)) configured <- c(200L, 2000L, 20000L)
  configured <- trimws(as.character(configured))
  include_full <- ame_env_flag_enabled("EMI_AME_BENCHMARK_INCLUDE_FULL", default = FALSE) || any(tolower(configured) %in% c("full", "all"))
  numeric_sizes <- suppressWarnings(as.integer(configured[!tolower(configured) %in% c("full", "all")]))
  numeric_sizes <- numeric_sizes[is.finite(numeric_sizes) & numeric_sizes > 0L]
  out <- unique(c(numeric_sizes, if (isTRUE(include_full)) as.integer(n_observations) else integer()))
  sort(out)
}

benchmark_ame_methods <- function(selection_model, selection_data, cfg, sample_sizes = NULL) {
  if (!requireNamespace("marginaleffects", quietly = TRUE)) {
    return(data.frame(method = "avg_slopes", sample_size = NA_integer_, elapsed_seconds = NA_real_, status = "skipped", reason = "Package marginaleffects not installed.", stringsAsFactors = FALSE))
  }
  if (is.list(selection_model) && !inherits(selection_model, "glm")) {
    return(data.frame(method = "avg_slopes", sample_size = NA_integer_, elapsed_seconds = NA_real_, status = "skipped", reason = selection_model$reason %||% "Selection model is not fitted.", stringsAsFactors = FALSE))
  }
  amed <- if (exists("ame_model_data_and_weights", mode = "function")) {
    ame_model_data_and_weights(selection_model)
  } else {
    list(data = as.data.frame(stats::model.frame(selection_model)), wts = FALSE)
  }
  data <- as.data.frame(amed$data, stringsAsFactors = FALSE)
  if (!nrow(data)) return(data.frame(method = "avg_slopes", sample_size = NA_integer_, elapsed_seconds = NA_real_, status = "skipped", reason = "No model-frame rows.", stringsAsFactors = FALSE))
  response_name <- as.character(stats::formula(selection_model)[[2]])
  weight_name <- if (is.character(amed$wts) && length(amed$wts) == 1L) amed$wts else "weight"
  numeric_variables <- setdiff(names(data)[vapply(data, is.numeric, logical(1))], c(response_name, weight_name))
  n_observations <- nrow(data)
  n_numeric_variables <- length(numeric_variables)
  centered_predict_calls <- n_numeric_variables * n_observations * 2L
  if (is.null(sample_sizes)) sample_sizes <- ame_benchmark_sample_sizes(nrow(data), cfg)
  sample_sizes <- sample_sizes[sample_sizes <= nrow(data)]
  if (!length(sample_sizes)) sample_sizes <- min(200L, nrow(data))
  old_parallel <- getOption("marginaleffects_parallel")
  options(marginaleffects_parallel = FALSE)
  on.exit(options(marginaleffects_parallel = old_parallel), add = TRUE)

  base_args <- list(model = selection_model, vcov = TRUE, type = "response", wts = amed$wts)

  safe_bind_rows(lapply(sample_sizes, function(n) {
    set.seed(999)
    rows <- if (n < nrow(data)) sample(seq_len(nrow(data)), n) else seq_len(nrow(data))
    newdata_sub <- data[rows, , drop = FALSE]
    run_one <- function(label, args = list()) {
      elapsed <- system.time({
        attempt <- tryCatch({
          do.call(marginaleffects::avg_slopes, c(base_args, list(newdata = newdata_sub), args))
          list(status = "estimated_legacy_vcov", reason = NA_character_, fallback = NA_character_)
        }, error = function(e) {
          # Recent marginaleffects versions can fail for the exact legacy call
          # (wts = "weight", vcov = TRUE) on sub-sampled survey-style newdata.
          # Preserve that failure, then try a clearly labeled current-version
          # timing path without uncertainty and without the legacy weight-string
          # dispatch.  This prevents failed rows from being mistaken for a
          # successful legacy timing while still yielding a useful current timing
          # when the package supports derivative-only estimation.
          legacy_error <- conditionMessage(e)
          fallback <- tryCatch({
            fallback_args <- c(list(model = selection_model, vcov = FALSE, type = "response", newdata = newdata_sub, wts = amed$wts), args)
            do.call(marginaleffects::avg_slopes, fallback_args)
            TRUE
          }, error = function(e2) conditionMessage(e2))
          if (isTRUE(fallback)) {
            list(status = "estimated_current_derivative_only_after_legacy_failure", reason = legacy_error, fallback = "vcov=FALSE with explicit sampled weights")
          } else {
            list(status = "current_version_incompatible", reason = legacy_error, fallback = paste("vcov=FALSE fallback failed:", fallback))
          }
        })
      })[["elapsed"]]
      data.frame(
        method = label,
        sample_size = n,
        n_observations = n_observations,
        n_numeric_variables = n_numeric_variables,
        centered_predict_calls_full_data = centered_predict_calls,
        elapsed_seconds = unname(elapsed),
        status = attempt$status,
        reason = attempt$reason,
        fallback = attempt$fallback,
        stringsAsFactors = FALSE
      )
    }
    safe_bind_rows(list(
      run_one("avg_slopes_centered_default"),
      run_one("avg_slopes_fdforward", list(numderiv = "fdforward"))
    ))
  }))
}

benchmark_parallelization_options <- function(selection_model, selection_data, cfg) {
  data.frame(
    method = c("marginaleffects_parallel_false", "future_parallel_attempt"),
    status = c("legacy_final_choice", "documented_not_run_by_default"),
    reason = c(
      "Legacy final draft set options(marginaleffects_parallel = FALSE).",
      "Legacy commented attempt failed because exported globals exceeded available RAM; benchmark target records this rather than forcing an unsafe run."
    ),
    stringsAsFactors = FALSE
  )
}

save_ame_benchmark <- function(benchmark, dir = "outputs/benchmarking/ame") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!inherits(benchmark, "emi_ame_benchmark")) benchmark <- list(methods = as.data.frame(benchmark), parallel = data.frame(), notes = data.frame())
  paths <- c(
    methods = write_diagnostic_csv(benchmark$methods %||% data.frame(), file.path(dir, "ame_methods_benchmark.csv")),
    parallel = write_diagnostic_csv(benchmark$parallel %||% data.frame(), file.path(dir, "ame_parallelization_notes.csv")),
    notes = write_diagnostic_csv(benchmark$notes %||% data.frame(), file.path(dir, "ame_legacy_benchmark_notes.csv"))
  )
  legacy_output_manifest(paths)
}
