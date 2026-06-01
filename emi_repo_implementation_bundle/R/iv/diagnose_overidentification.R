# Overidentification diagnostics for IV specifications.
# Current baseline is exactly identified: one endogenous variable (EMIE) and one excluded
# instrument (linguistic distance). These diagnostics therefore return an explicit
# "not applicable" result unless future specifications add more excluded instruments
# than endogenous regressors.

#' Diagnose overidentifying restrictions when structurally applicable
#'
#' @param iv_models A fitted model or list of fitted models.
#' @param model_specs A model-spec list or list of model-spec lists. Each spec should
#'   contain `endogenous_vars` and `excluded_instruments`.
#' @param cfg Project config list.
#' @return Tibble describing Sargan/GMM overidentification diagnostics or why skipped.
diagnose_overidentification <- function(iv_models, model_specs, cfg = list()) {
  if (is.null(model_specs)) {
    return(tibble::tibble(test = "overidentification", status = "not_applicable", reason = "No model_specs supplied."))
  }
  if (!is.list(model_specs) || !is.null(model_specs$endogenous_vars)) {
    model_specs <- list(model_specs)
  }
  purrr::map_dfr(seq_along(model_specs), function(i) {
    spec <- model_specs[[i]]
    model <- if (is.list(iv_models) && length(iv_models) >= i) iv_models[[i]] else iv_models
    mode <- cfg$overidentification$run %||% "auto"
    if (identical(mode, "never")) {
      return(tibble::tibble(model = i, test = "overidentification", status = "skipped", reason = "Disabled by config."))
    }
    if (!is_overidentified(spec)) {
      if (identical(mode, "force")) stop("Overidentification test forced but model is exactly identified or underidentified.", call. = FALSE)
      return(tibble::tibble(model = i, test = "overidentification", status = "not_applicable", reason = "Excluded instruments do not outnumber endogenous variables."))
    }
    dplyr::bind_rows(
      run_sargan_if_applicable(model, spec, cfg, model_id = i),
      run_gmm_overid_if_applicable(model, spec, cfg, model_id = i)
    )
  })
}

#' Test whether a model is overidentified
#'
#' @param model_spec List with `endogenous_vars` and `excluded_instruments`.
#' @return TRUE iff excluded instruments outnumber endogenous variables.
is_overidentified <- function(model_spec) {
  n_endog <- length(model_spec$endogenous_vars %||% character())
  n_inst <- length(model_spec$excluded_instruments %||% character())
  n_inst > n_endog && n_endog > 0L
}

#' Run Sargan-style diagnostics when enabled and possible
#'
#' @return Tibble with diagnostic result or skip reason.
run_sargan_if_applicable <- function(model, model_spec, cfg = list(), model_id = NA_integer_) {
  enabled <- isTRUE(cfg$overidentification$sargan %||% FALSE)
  if (!enabled) {
    return(tibble::tibble(model = model_id, test = "sargan", status = "skipped", reason = "Disabled by config; robust GMM-style tests are preferred for heteroskedastic/clustered settings."))
  }
  if (!is_overidentified(model_spec)) {
    return(tibble::tibble(model = model_id, test = "sargan", status = "not_applicable", reason = "Model is not overidentified."))
  }
  # Placeholder for future exact implementation once the final IV estimation object is fixed.
  tibble::tibble(model = model_id, test = "sargan", status = "todo", reason = "Implement after final IV object/class is stable.")
}

#' Run robust GMM overidentification diagnostics when enabled and possible
#'
#' @return Tibble with diagnostic result or skip reason.
run_gmm_overid_if_applicable <- function(model, model_spec, cfg = list(), model_id = NA_integer_) {
  enabled <- isTRUE(cfg$overidentification$robust_gmm %||% TRUE)
  if (!enabled) {
    return(tibble::tibble(model = model_id, test = "gmm_overid", status = "skipped", reason = "Disabled by config."))
  }
  if (!is_overidentified(model_spec)) {
    return(tibble::tibble(model = model_id, test = "gmm_overid", status = "not_applicable", reason = "Model is not overidentified."))
  }
  # Placeholder for future exact implementation once the final IV/GMM estimation routine is chosen.
  tibble::tibble(model = model_id, test = "gmm_overid", status = "todo", reason = "Implement after multiple-instrument specifications are finalized.")
}

`%||%` <- function(x, y) if (is.null(x)) y else x
