# Overidentification diagnostics for IV specifications.
# The current baseline is exactly identified: one endogenous variable (EMIE) and
# one excluded instrument (linguistic distance). The diagnostic therefore reports
# exact-identification status from the active IV formulas instead of exposing
# unimplemented Sargan/GMM placeholders.

#' Diagnose whether active IV specifications are overidentified
#'
#' @param iv_models A fitted model or named list of fitted models. Retained for
#'   target compatibility; identification status is inferred from `model_specs`.
#' @param model_specs IV formulas from `build_iv_formulas()` or explicit spec
#'   lists with `endogenous_vars` and `excluded_instruments`.
#' @param cfg Project config list.
#' @return Tibble with one row per model/specification.
diagnose_overidentification <- function(iv_models, model_specs, cfg = list()) {
  specs <- normalize_overidentification_specs(model_specs)
  if (!length(specs)) {
    return(tibble::tibble(
      model = NA_character_,
      test = "overidentification",
      status = "not_applicable",
      reason = "No IV model specifications supplied."
    ))
  }

  mode <- cfg$overidentification$run %||% "auto"
  purrr::map_dfr(names(specs), function(model_name) {
    spec <- specs[[model_name]]
    n_endog <- length(spec$endogenous_vars %||% character())
    n_inst <- length(spec$excluded_instruments %||% character())

    if (identical(mode, "never")) {
      return(tibble::tibble(
        model = model_name,
        test = "overidentification",
        status = "skipped",
        n_endogenous = n_endog,
        n_excluded_instruments = n_inst,
        reason = "Disabled by config."
      ))
    }

    if (!is_overidentified(spec)) {
      return(tibble::tibble(
        model = model_name,
        test = "overidentification",
        status = "not_applicable",
        n_endogenous = n_endog,
        n_excluded_instruments = n_inst,
        reason = "Excluded instruments do not outnumber endogenous variables."
      ))
    }

    if (identical(mode, "force")) {
      stop("Overidentification diagnostics were forced, but no overidentification test is implemented for the active IV estimator.", call. = FALSE)
    }
    tibble::tibble(
      model = model_name,
      test = "overidentification",
      status = "requires_overidentified_estimator",
      n_endogenous = n_endog,
      n_excluded_instruments = n_inst,
      reason = "Specification is overidentified, but the current project has not selected a robust overidentification-test estimator."
    )
  })
}

normalize_overidentification_specs <- function(model_specs) {
  if (is.null(model_specs) || inherits(model_specs, "data.frame")) return(list())
  if (inherits(model_specs, "formula")) return(list(model = iv_formula_spec(model_specs)))
  if (is.list(model_specs) && !is.null(model_specs$endogenous_vars)) return(list(model = model_specs))
  if (!is.list(model_specs)) return(list())

  out <- lapply(model_specs, function(spec) {
    if (inherits(spec, "formula")) return(iv_formula_spec(spec))
    if (is.list(spec) && !is.null(spec$endogenous_vars)) return(spec)
    NULL
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) return(list())
  if (is.null(names(out)) || any(!nzchar(names(out)))) names(out) <- paste0("model_", seq_along(out))
  out
}

iv_formula_spec <- function(formula) {
  rhs <- deparse(formula[[3]], width.cutoff = 500L)
  rhs <- paste(rhs, collapse = " ")
  sides <- strsplit(rhs, "|", fixed = TRUE)[[1]]
  if (length(sides) != 2L) {
    return(list(endogenous_vars = character(), excluded_instruments = character()))
  }
  regressors <- formula_terms(sides[[1]])
  instruments <- formula_terms(sides[[2]])
  list(
    endogenous_vars = setdiff(regressors, instruments),
    excluded_instruments = setdiff(instruments, regressors)
  )
}

formula_terms <- function(rhs) {
  rhs <- trimws(rhs)
  if (!nzchar(rhs)) return(character())
  attr(stats::terms(stats::as.formula(paste("~", rhs))), "term.labels")
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
