# Overidentifying-restrictions diagnostics for IV specifications.
# Exactly identified specifications are reported as not applicable. Overidentified
# ivreg specifications use the package's standard Sargan diagnostic; this is a
# conventional homoskedastic diagnostic and is not a weak-IV validity test.

ivreg_sargan_diagnostic <- function(model) {
  if (!inherits(model, "ivreg")) {
    return(data.frame(
      test = "sargan",
      status = "not_estimated",
      statistic = NA_real_,
      df = NA_real_,
      p.value = NA_real_,
      reason = "No fitted ivreg model was supplied.",
      stringsAsFactors = FALSE
    ))
  }
  diagnostics <- summary(model, diagnostics = TRUE)$diagnostics
  row <- grep("^Sargan$", rownames(diagnostics), ignore.case = TRUE)
  if (!length(row)) {
    return(data.frame(
      test = "sargan",
      status = "not_applicable",
      statistic = NA_real_,
      df = NA_real_,
      p.value = NA_real_,
      reason = "The fitted model is not overidentified.",
      stringsAsFactors = FALSE
    ))
  }
  row <- row[[1]]
  data.frame(
    test = "sargan",
    status = if (is.finite(diagnostics[row, "statistic"])) "estimated" else "not_applicable",
    statistic = unname(diagnostics[row, "statistic"]),
    df = unname(diagnostics[row, "df1"]),
    p.value = unname(diagnostics[row, "p-value"]),
    reason = if (is.finite(diagnostics[row, "statistic"])) NA_character_ else "The fitted model is not overidentified.",
    stringsAsFactors = FALSE
  )
}

estimate_overidentification_spec <- function(data, specification) {
  if (specification$n_excluded_instruments[[1]] <= specification$n_endogenous[[1]]) {
    return(data.frame(
      specification_id = specification$specification_id,
      test = "sargan",
      status = "not_applicable",
      n_endogenous = specification$n_endogenous[[1]],
      n_excluded_instruments = specification$n_excluded_instruments[[1]],
      statistic = NA_real_,
      df = NA_real_,
      p.value = NA_real_,
      reason = "Excluded instruments do not outnumber endogenous variables.",
      stringsAsFactors = FALSE
    ))
  }
  needed <- iv_specification_variables(specification)
  missing <- setdiff(needed, names(data))
  if (length(missing)) {
    return(data.frame(
      specification_id = specification$specification_id,
      test = "sargan",
      status = "not_estimated",
      n_endogenous = specification$n_endogenous[[1]],
      n_excluded_instruments = specification$n_excluded_instruments[[1]],
      statistic = NA_real_,
      df = NA_real_,
      p.value = NA_real_,
      reason = paste0("Missing columns: ", paste(missing, collapse = ", ")),
      stringsAsFactors = FALSE
    ))
  }
  x <- data[stats::complete.cases(data[needed]), , drop = FALSE]
  fit <- ivreg::ivreg(iv_specification_formula(specification), data = x)
  result <- ivreg_sargan_diagnostic(fit)
  cbind(
    data.frame(
      specification_id = specification$specification_id,
      n_endogenous = specification$n_endogenous[[1]],
      n_excluded_instruments = specification$n_excluded_instruments[[1]],
      stringsAsFactors = FALSE
    ),
    result
  )
}

run_iv_overidentification_diagnostics <- function(
  panel,
  specifications = iv_diagnostic_specification_registry()
) {
  data <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else as.data.frame(panel, stringsAsFactors = FALSE)
  applicable <- iv_diagnostic_applicability(specifications)
  ids <- applicable$specification_id[
    applicable$diagnostic_id == "overidentification" & applicable$will_run
  ]
  specs <- specifications[specifications$specification_id %in% ids, , drop = FALSE]
  safe_bind_rows(lapply(seq_len(nrow(specs)), function(i) {
    estimate_overidentification_spec(data, specs[i, , drop = FALSE])
  }))
}

#' Diagnose whether active IV specifications are overidentified
#'
#' @param iv_models A fitted model or named list of fitted models. Retained for
#'   target compatibility; identification status is inferred from `model_specs`.
#' @param model_specs IV formulas or explicit spec
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

    model <- if (is.list(iv_models) && !inherits(iv_models, "ivreg")) {
      iv_models[[model_name]]
    } else {
      iv_models
    }
    result <- ivreg_sargan_diagnostic(model)
    if (identical(mode, "force") && !identical(result$status[[1]], "estimated")) {
      stop("Overidentification diagnostics were forced, but the Sargan diagnostic could not be estimated.", call. = FALSE)
    }
    tibble::tibble(
      model = model_name,
      test = result$test,
      status = result$status,
      n_endogenous = n_endog,
      n_excluded_instruments = n_inst,
      statistic = result$statistic,
      df = result$df,
      p.value = result$p.value,
      reason = result$reason
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
