# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-spatial-autocorrelation

#' diagnose spatial autocorrelation
#'
#' Reproduce the legacy Rmd's Moran's I diagnostics for the 2019-20 district
#' geometry path.  The legacy workflow used rook contiguity, row-standardized
#' weights, `zero.policy = TRUE`, IV-model residuals, first-stage residuals,
#' the main treatment/instrument variables, and the consumption/Gini outcomes.
#'
#' @return A data frame with Moran diagnostics.
diagnose_spatial_autocorrelation <- function(district_panel, iv_models, spatial_weights, cfg) {
  if (isFALSE(cfg$run_diagnostics$spatial_autocorrelation %||% TRUE)) {
    return(spatial_autocorrelation_status_row("skipped", "Spatial autocorrelation diagnostics are disabled in config."))
  }
  if (!inherits(district_panel, "sf")) {
    return(spatial_autocorrelation_status_row("out_of_active_pipeline", "Requires sf geometry."))
  }
  need_pkg("spdep", "Moran's I diagnostics")
  need_pkg("sf", "Moran's I diagnostics")

  model <- spatial_iv_model(iv_models, "consumption")
  rows <- spatial_model_rows(model, district_panel)
  if (!length(rows)) {
    return(spatial_autocorrelation_status_row("out_of_active_pipeline", "Could not recover the IV model analysis rows for spatial diagnostics."))
  }
  weights <- build_spatial_weights_for_rows(district_panel, rows, queen = FALSE)
  if (!identical(weights$status, "constructed")) {
    return(spatial_autocorrelation_status_row(weights$status %||% "out_of_active_pipeline", weights$reason %||% "Could not build rook-contiguity weights."))
  }

  rows <- weights$row_index
  panel <- as.data.frame(sf::st_drop_geometry(district_panel[rows, , drop = FALSE]))
  out <- list()

  out <- c(out, list(spatial_moran_test_from_model_residuals(
    model = spatial_iv_model(iv_models, "consumption"),
    district_panel = district_panel,
    weights = weights,
    legacy_name = "m_cons_resid",
    estimand = "consumption_iv_residual",
    variable = "resid_cons",
    source = "second_stage_residual"
  )))

  out <- c(out, list(spatial_moran_test_from_model_residuals(
    model = spatial_iv_model(iv_models, "gini"),
    district_panel = district_panel,
    weights = weights,
    legacy_name = "m_gini_resid",
    estimand = "gini_iv_residual",
    variable = "resid_gini",
    source = "second_stage_residual"
  )))

  out <- c(out, list(spatial_moran_test_from_first_stage_residuals(
    model = spatial_iv_model(iv_models, "consumption"),
    district_panel = district_panel,
    weights = weights,
    legacy_name = "m_fscons_resid",
    estimand = "consumption_first_stage_residual",
    variable = "resid_fscons",
    source = "first_stage_residual"
  )))

  out <- c(out, list(spatial_moran_test_from_first_stage_residuals(
    model = spatial_iv_model(iv_models, "gini"),
    district_panel = district_panel,
    weights = weights,
    legacy_name = "m_fsgini_resid",
    estimand = "gini_first_stage_residual",
    variable = "resid_fsgini",
    source = "first_stage_residual"
  )))

  out <- c(out, list(compute_moran_tests(panel$EMIE, weights, legacy_name = "m_EMIE", estimand = "emie", variable = "EMIE", source = "treatment")))
  out <- c(out, list(compute_moran_tests(panel$wavg_ling_degrees, weights, legacy_name = "m_wavg_ling_degrees", estimand = "linguistic_distance", variable = "wavg_ling_degrees", source = "instrument")))
  out <- c(out, list(compute_moran_tests(panel$consumption_pct_change, weights, legacy_name = "m_cons", estimand = "consumption_growth", variable = "consumption_pct_change", source = "outcome")))
  out <- c(out, list(compute_moran_tests(panel$gini_change, weights, legacy_name = "m_gini", estimand = "gini_change", variable = "gini_change", source = "outcome")))

  out <- safe_bind_rows(out)
  out$legacy_note <- spatial_legacy_note(out$legacy_name)
  out
}

spatial_autocorrelation_status_row <- function(status, reason) {
  data.frame(
    legacy_name = NA_character_,
    estimand = NA_character_,
    variable = NA_character_,
    source = NA_character_,
    test = "moran",
    status = status,
    statistic = NA_real_,
    estimate = NA_real_,
    expected = NA_real_,
    variance = NA_real_,
    p.value = NA_real_,
    method = NA_character_,
    alternative = NA_character_,
    n = NA_integer_,
    contiguity = NA_character_,
    weights_style = NA_character_,
    matrix_style = NA_character_,
    zero_policy = NA,
    n_spatial_rows = NA_integer_,
    n_islands = NA_integer_,
    mean_neighbors = NA_real_,
    warnings = NA_character_,
    reason = reason,
    stringsAsFactors = FALSE
  )
}

spatial_iv_model <- function(iv_models, name) {
  if (is.list(iv_models) && !is.null(iv_models[[name]])) return(iv_models[[name]])
  if (is.list(iv_models) && length(iv_models)) return(iv_models[[1]])
  iv_models
}

spatial_model_rows <- function(model, district_panel) {
  rows <- tryCatch(as.integer(rownames(stats::model.frame(model))), error = function(e) integer())
  rows <- rows[is.finite(rows) & rows >= 1L & rows <= nrow(district_panel)]
  unique(rows)
}

spatial_moran_test_from_model_residuals <- function(model, district_panel, weights, legacy_name, estimand, variable, source) {
  if (!inherits(model, "ivreg")) {
    return(spatial_autocorrelation_status_row("out_of_active_pipeline", paste0("No active IV model for ", estimand, ".")))
  }
  rows <- spatial_model_rows(model, district_panel)
  if (!identical(as.integer(rows), as.integer(weights$row_index))) {
    weights <- build_spatial_weights_for_rows(district_panel, rows, queen = FALSE)
  }
  x <- tryCatch(stats::residuals(model), error = function(e) NA_real_)
  compute_moran_tests(x, weights, legacy_name, estimand, variable, source)
}

spatial_moran_test_from_first_stage_residuals <- function(model, district_panel, weights, legacy_name, estimand, variable, source) {
  fit <- spatial_first_stage_model(model, district_panel)
  if (is.null(fit)) {
    return(spatial_autocorrelation_status_row("out_of_active_pipeline", paste0("Could not estimate first-stage model for ", estimand, ".")))
  }
  rows <- spatial_model_rows(fit, district_panel)
  if (!identical(as.integer(rows), as.integer(weights$row_index))) {
    weights <- build_spatial_weights_for_rows(district_panel, rows, queen = FALSE)
  }
  x <- tryCatch(stats::residuals(fit), error = function(e) NA_real_)
  compute_moran_tests(x, weights, legacy_name, estimand, variable, source)
}

spatial_first_stage_model <- function(model, district_panel) {
  if (!inherits(model, "ivreg")) return(NULL)
  iv_terms <- parse_iv_formula_terms(model)
  if (is.null(iv_terms) || !length(iv_terms$regressors) || !length(iv_terms$instruments)) return(NULL)
  endogenous <- setdiff(iv_terms$regressors, iv_terms$instruments)
  if (!length(endogenous)) endogenous <- iv_terms$regressors[[1]]
  if (!length(endogenous) || is.na(endogenous[[1]]) || !nzchar(endogenous[[1]])) return(NULL)
  first_stage_formula <- stats::as.formula(paste(endogenous[[1]], "~", paste(iv_terms$instruments, collapse = " + ")))
  data <- add_legacy_iv_aliases(as.data.frame(district_panel))
  missing <- setdiff(all.vars(first_stage_formula), names(data))
  if (length(missing)) return(NULL)
  tryCatch(stats::lm(first_stage_formula, data = data), error = function(e) NULL)
}

#' compute moran tests
#'
#' @return A data frame containing the statistic, Moran's I estimate, expected
#' value, variance, and p-value.  `moran.test()` is the asymptotic-normal test
#' used by the legacy Rmd.
compute_moran_tests <- function(x, spatial_weights, legacy_name = NA_character_, estimand = NA_character_, variable = NA_character_, source = NA_character_) {
  if (!inherits(spatial_weights, "emi_spatial_weights") || !identical(spatial_weights$status, "constructed")) {
    return(spatial_autocorrelation_status_row("out_of_active_pipeline", "Spatial weights were not constructed."))
  }
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != length(spatial_weights$row_index)) {
    return(spatial_autocorrelation_status_row("out_of_active_pipeline", paste0("Length mismatch for ", variable, ": ", length(x), " values for ", length(spatial_weights$row_index), " spatial rows.")))
  }
  if (any(!is.finite(x))) {
    return(spatial_autocorrelation_status_row("out_of_active_pipeline", paste0("Non-finite values prevent Moran's I for ", variable, ".")))
  }
  test <- tryCatch(spdep::moran.test(x, spatial_weights$listw, zero.policy = TRUE), error = function(e) e)
  if (inherits(test, "error")) {
    return(spatial_autocorrelation_status_row("out_of_active_pipeline", conditionMessage(test)))
  }
  estimate <- suppressWarnings(as.numeric(test$estimate))
  estimate_value <- function(i) if (length(estimate) >= i && is.finite(estimate[[i]])) estimate[[i]] else NA_real_
  data.frame(
    legacy_name = legacy_name,
    estimand = estimand,
    variable = variable,
    source = source,
    test = "moran",
    status = "estimated",
    statistic = suppressWarnings(as.numeric(test$statistic[[1]])),
    estimate = estimate_value(1L),
    expected = estimate_value(2L),
    variance = estimate_value(3L),
    p.value = suppressWarnings(as.numeric(test$p.value)),
    method = test$method %||% "Moran I test under randomisation",
    alternative = test$alternative %||% NA_character_,
    n = length(x),
    contiguity = spatial_weights$contiguity %||% NA_character_,
    weights_style = spatial_weights$style %||% NA_character_,
    matrix_style = spatial_weights$matrix_style %||% NA_character_,
    zero_policy = spatial_weights$zero_policy %||% NA,
    n_spatial_rows = spatial_weights$n %||% length(x),
    n_islands = spatial_weights$n_islands %||% NA_integer_,
    mean_neighbors = spatial_weights$mean_neighbors %||% NA_real_,
    warnings = paste(spatial_weights$warnings %||% character(), collapse = "; "),
    reason = NA_character_,
    stringsAsFactors = FALSE
  )
}

#' compute monte carlo moran tests
#'
#' The legacy Rmd kept this as commented-out sensitivity code with
#' `set.seed(999)` and `nsim = 9999`, noting that `moran.test()` assumes
#' asymptotic normality.  It remains opt-in because it is much slower than the
#' asymptotic tests used in the paper text.
compute_monte_carlo_moran_tests <- function(x, spatial_weights, nsim = 9999L, seed = 999L, legacy_name = NA_character_, estimand = NA_character_, variable = NA_character_, source = NA_character_) {
  if (!inherits(spatial_weights, "emi_spatial_weights") || !identical(spatial_weights$status, "constructed")) {
    return(spatial_autocorrelation_status_row("out_of_active_pipeline", "Spatial weights were not constructed."))
  }
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != length(spatial_weights$row_index) || any(!is.finite(x))) {
    return(spatial_autocorrelation_status_row("out_of_active_pipeline", paste0("Cannot run Monte Carlo Moran test for ", variable, ".")))
  }
  set.seed(seed)
  test <- tryCatch(spdep::moran.mc(x, spatial_weights$listw, nsim = nsim, zero.policy = TRUE), error = function(e) e)
  if (inherits(test, "error")) {
    return(spatial_autocorrelation_status_row("out_of_active_pipeline", conditionMessage(test)))
  }
  data.frame(
    legacy_name = paste0(legacy_name, "_mc"),
    estimand = estimand,
    variable = variable,
    source = source,
    test = "moran_mc",
    status = "estimated",
    statistic = suppressWarnings(as.numeric(test$statistic[[1]])),
    estimate = NA_real_,
    expected = NA_real_,
    variance = NA_real_,
    p.value = suppressWarnings(as.numeric(test$p.value)),
    method = test$method %||% "Monte-Carlo simulation of Moran I",
    alternative = test$alternative %||% NA_character_,
    n = length(x),
    contiguity = spatial_weights$contiguity %||% NA_character_,
    weights_style = spatial_weights$style %||% NA_character_,
    matrix_style = spatial_weights$matrix_style %||% NA_character_,
    zero_policy = spatial_weights$zero_policy %||% NA,
    n_spatial_rows = spatial_weights$n %||% length(x),
    n_islands = spatial_weights$n_islands %||% NA_integer_,
    mean_neighbors = spatial_weights$mean_neighbors %||% NA_real_,
    warnings = paste(spatial_weights$warnings %||% character(), collapse = "; "),
    reason = NA_character_,
    stringsAsFactors = FALSE
  )
}


spatial_moran_mc_nsim <- function(cfg = list()) {
  configured <- cfg$diagnostics$spatial_moran_mc_nsim %||% cfg$spatial_moran_mc_nsim %||% NULL
  env <- Sys.getenv("EMI_SPATIAL_MORAN_MC_NSIM", unset = "")
  if (nzchar(env)) configured <- env
  nsim <- suppressWarnings(as.integer(configured %||% 9999L))
  if (!is.finite(nsim) || nsim < 1L) nsim <- 9999L
  nsim
}

spatial_moran_mc_seed <- function(cfg = list()) {
  configured <- cfg$diagnostics$spatial_moran_mc_seed %||% cfg$spatial_moran_mc_seed %||% NULL
  env <- Sys.getenv("EMI_SPATIAL_MORAN_MC_SEED", unset = "")
  if (nzchar(env)) configured <- env
  seed <- suppressWarnings(as.integer(configured %||% 999L))
  if (!is.finite(seed)) seed <- 999L
  seed
}

#' diagnose Monte Carlo Moran benchmark
#'
#' Expensive legacy sensitivity path for Moran's I p-values.  This is not part
#' of the public default diagnostics because `moran.mc(..., nsim = 9999)` is much
#' slower than the asymptotic `moran.test()` path used in the paper values.
#' `make benchmarking-full` runs this target deliberately.
diagnose_spatial_moran_mc <- function(district_panel, iv_models, spatial_weights, cfg) {
  nsim <- spatial_moran_mc_nsim(cfg)
  seed <- spatial_moran_mc_seed(cfg)
  if (!inherits(spatial_weights, "emi_spatial_weights") || !identical(spatial_weights$status, "constructed")) {
    out <- spatial_autocorrelation_status_row("out_of_active_pipeline", "Spatial weights were not constructed for Monte Carlo Moran benchmark.")
    out$nsim <- nsim
    out$seed <- seed
    return(out)
  }
  out <- list()

  add_model_residual <- function(model, legacy_name, estimand, variable, source) {
    if (!inherits(model, "ivreg")) {
      return(spatial_autocorrelation_status_row("out_of_active_pipeline", paste0("No active IV model for ", estimand, ".")))
    }
    rows <- spatial_model_rows(model, district_panel)
    weights <- if (identical(as.integer(rows), as.integer(spatial_weights$row_index))) spatial_weights else build_spatial_weights_for_rows(district_panel, rows, queen = FALSE)
    x <- tryCatch(stats::residuals(model), error = function(e) NA_real_)
    compute_monte_carlo_moran_tests(x, weights, nsim = nsim, seed = seed, legacy_name = legacy_name, estimand = estimand, variable = variable, source = source)
  }

  out <- c(out, list(add_model_residual(spatial_iv_model(iv_models, "consumption"), "m_cons_resid", "consumption_iv_residual", "resid_cons", "second_stage_residual")))
  out <- c(out, list(add_model_residual(spatial_iv_model(iv_models, "gini"), "m_gini_resid", "gini_iv_residual", "resid_gini", "second_stage_residual")))

  panel <- as.data.frame(district_panel)[spatial_weights$row_index, , drop = FALSE]
  for (spec in list(
    list(col = "EMIE", legacy_name = "m_EMIE", estimand = "emie", source = "treatment"),
    list(col = "wavg_ling_degrees", legacy_name = "m_wavg_ling_degrees", estimand = "linguistic_distance", source = "instrument"),
    list(col = "consumption_pct_change", legacy_name = "m_cons", estimand = "consumption_growth", source = "outcome"),
    list(col = "gini_change", legacy_name = "m_gini", estimand = "gini_change", source = "outcome")
  )) {
    if (spec$col %in% names(panel)) {
      out <- c(out, list(compute_monte_carlo_moran_tests(panel[[spec$col]], spatial_weights, nsim = nsim, seed = seed, legacy_name = spec$legacy_name, estimand = spec$estimand, variable = spec$col, source = spec$source)))
    }
  }
  out <- safe_bind_rows(out)
  out$nsim <- nsim
  out$seed <- seed
  out$legacy_note <- spatial_legacy_note(sub("_mc$", "", out$legacy_name))
  out
}

spatial_legacy_note <- function(legacy_name) {
  notes <- c(
    m_cons_resid = "Final-paper residual p-value: residuals(model_consumption_iv). Legacy comments reported a pre-control value of 2.779572e-23.",
    m_gini_resid = "Legacy residual diagnostic: residuals(model_gini_iv). Legacy comments reported a pre-control value of 2.033012e-40.",
    m_fscons_resid = "Legacy first-stage residual diagnostic: residuals(first_stage_consumption). Legacy comments reported a pre-control value of 1.189148e-105.",
    m_fsgini_resid = "Legacy first-stage residual diagnostic: residuals(first_stage_gini). Legacy comments noted the same pre-control value as first-stage consumption.",
    m_EMIE = "Legacy comments reported p = 8.990354e-180 for EMIE.",
    m_wavg_ling_degrees = "Legacy comments reported p = 1.721903e-254 for weighted average linguistic distance.",
    m_cons = "Final-paper outcome p-value: consumption_pct_change. Legacy comments reported p = 1.608813e-26.",
    m_gini = "Legacy outcome diagnostic: gini_change. Legacy comments reported p = 8.51626e-22."
  )
  unname(notes[legacy_name]) %||% NA_character_
}

spatial_moran_mc_reference <- function() {
  data.frame(
    scaffold = "moran.mc(resid_cons, listw_2020, nsim = 9999)",
    status = "documented_not_run_by_default",
    reason = "Legacy Chunk 29 kept this as a Monte Carlo robustness scaffold. The default public diagnostic keeps the asymptotic moran.test() path used by report_values; run `make benchmarking-full` or the public audit flag `--with-benchmarking-full` before treating Monte Carlo p-values as refreshed current results.",
    stringsAsFactors = FALSE
  )
}


#' save spatial autocorrelation diagnostics
#'
#' @return The diagnostics, invisibly writing public CSV artifacts for review.
save_spatial_autocorrelation_diagnostics <- function(diagnostics, dir = "outputs/diagnostics/public") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(as.data.frame(diagnostics), file.path(dir, "spatial_moran_tests.csv"), row.names = FALSE)
  utils::write.csv(spatial_moran_mc_reference(), file.path(dir, "spatial_moran_mc_reference.csv"), row.names = FALSE)
  diagnostics
}

# sample-end: code-spatial-autocorrelation
