# Model coefficient and first-stage helpers for public report values.

report_coefficient_frame <- function(model, data = NULL) {
  clustered_required <- identical(attr(model, "cluster_inference_status"), "estimated")
  coefs <- clustered_model_coefficients(model, data)
  if (!nrow(coefs) && !clustered_required) coefs <- plain_model_coefficients(model)
  if (!nrow(coefs)) return(data.frame())

  out <- data.frame(
    Estimate = coefs$Estimate,
    estimate = coefs$Estimate,
    p.value = coefs$`Pr(>|t|)`,
    `Pr(>|t|)` = coefs$`Pr(>|t|)`,
    statistic = coefs$statistic,
    std.error = coefs$`Std. Error`,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(out) <- rownames(coefs)
  out
}

coefficient_value <- function(
  model, terms, column = c("Estimate", "estimate"), digits = NULL,
  data = NULL
) {
  out <- tryCatch({
    coefs <- report_coefficient_frame(model, data)
    term <- first_matching_term(terms, rownames(coefs))
    if (is.na(term)) return(NA_real_)
    hit <- intersect(column, names(coefs))
    if (!length(hit)) return(NA_real_)
    suppressWarnings(as.numeric(coefs[term, hit[[1]]]))
  }, error = function(e) NA_real_)
  if (!is.null(digits) && is.finite(out)) out <- round(out, digits)
  out
}

p_value <- function(model, terms, digits = NULL, data = NULL) {
  out <- tryCatch({
    coefs <- report_coefficient_frame(model, data)
    term <- first_matching_term(terms, rownames(coefs))
    if (is.na(term)) return(NA_real_)
    hit <- intersect(c("Pr(>|t|)", "Pr(>|z|)", "p.value", "p_value"), names(coefs))
    if (!length(hit)) return(NA_real_)
    suppressWarnings(as.numeric(coefs[term, hit[[1]]]))
  }, error = function(e) NA_real_)
  if (!is.null(digits) && is.finite(out)) out <- signif(out, digits)
  out
}

condition_number_value <- function(model) {
  out <- tryCatch(
    standardized_design_condition_number(iv_structural_model_matrix(model)),
    error = function(e) NA_real_
  )
  if (is.finite(out)) format(out, scientific = FALSE, digits = 7) else NA_character_
}

normalize_report_term <- function(x) {
  x <- tolower(as.character(x))
  gsub("[^a-z0-9]+", "", x)
}

first_matching_term <- function(terms, available_terms) {
  if (!length(terms) || !length(available_terms)) return(NA_character_)
  term_norm <- normalize_report_term(terms)
  available_norm <- normalize_report_term(available_terms)

  exact <- match(term_norm, available_norm)
  exact <- exact[!is.na(exact)]
  if (length(exact)) return(available_terms[[exact[[1]]]])

  fuzzy <- which(vapply(available_norm, function(x) any(nzchar(term_norm) & grepl(paste(term_norm, collapse = "|"), x)), logical(1)))
  if (length(fuzzy)) return(available_terms[[fuzzy[[1]]]])

  NA_character_
}

format_report_number <- function(out, column = "estimate", digits = NULL) {
  if (!is.null(digits) && is.finite(out)) {
    if (identical(column, "p.value")) out <- signif(out, digits) else out <- round(out, digits)
  }
  out
}

first_stage_value <- function(first_stage_tests, terms, column = "estimate", digits = NULL) {
  x <- as_plain_data_frame(first_stage_tests)
  if (!nrow(x) || !"term" %in% names(x) || !column %in% names(x)) return(NA_real_)
  if ("model" %in% names(x) && any(x$model %in% c("consumption", "baseline"))) {
    x <- x[x$model %in% c("consumption", "baseline"), , drop = FALSE]
  }
  if (any(grepl("^[0-9]+$", as.character(x$term)))) return(NA_real_)
  if ("status" %in% names(x)) x <- x[x$status == "estimated", , drop = FALSE]
  term <- first_matching_term(terms, x$term)
  if (is.na(term)) return(NA_real_)
  out <- suppressWarnings(as.numeric(x[x$term == term, column][[1]]))
  format_report_number(out, column, digits)
}

first_iv_model <- function(iv_models) {
  if (is.list(iv_models) && length(iv_models)) return(iv_models[[1]])
  iv_models
}

first_stage_model_from_iv <- function(iv_models, district_panel = NULL) {
  model <- first_iv_model(iv_models)
  if (!inherits(model, "ivreg")) return(NULL)

  data <- as_plain_data_frame(district_panel)
  first_stage <- if (nrow(data)) iv_first_stage_fit(model, data) else NULL
  if (is.null(first_stage)) {
    fitted_data <- tryCatch(
      as_plain_data_frame(stats::model.frame(model)),
      error = function(e) data.frame()
    )
    if (nrow(fitted_data)) first_stage <- iv_first_stage_fit(model, fitted_data)
  }
  if (is.null(first_stage)) NULL else first_stage$model
}

first_stage_model_value <- function(iv_models, district_panel, terms, column = "estimate", digits = NULL) {
  fit <- first_stage_model_from_iv(iv_models, district_panel)
  if (is.null(fit)) return(NA_real_)
  if (identical(column, "p.value")) {
    return(p_value(fit, terms, digits = digits))
  }
  coefficient_value(fit, terms, column = c("Estimate", "estimate"), digits = digits)
}

first_stage_report_value <- function(first_stage_tests, iv_models, district_panel, terms, column = "estimate", digits = NULL) {
  out <- first_stage_value(first_stage_tests, terms, column, digits)
  if (is.finite(out)) return(out)
  first_stage_model_value(iv_models, district_panel, terms, column, digits)
}
