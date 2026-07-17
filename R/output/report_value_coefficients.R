# Model coefficient and first-stage helpers for public report values.

report_coefficient_frame <- function(model) {
  estimates <- tryCatch(stats::coef(model), error = function(e) NULL)
  if (is.null(estimates) || !length(estimates)) return(data.frame())

  terms <- names(estimates)
  if (is.null(terms) || !length(terms)) terms <- paste0("term_", seq_along(estimates))
  estimates <- suppressWarnings(as.numeric(estimates))

  vc <- tryCatch(stats::vcov(model), error = function(e) NULL)
  se <- rep(NA_real_, length(estimates))
  if (!is.null(vc) && length(dim(vc)) == 2L && all(dim(vc) >= length(estimates))) {
    diag_vc <- suppressWarnings(as.numeric(diag(vc)))
    vc_terms <- rownames(vc)
    if (!is.null(vc_terms) && length(vc_terms)) {
      matched <- match(terms, vc_terms)
      ok <- !is.na(matched) & matched <= length(diag_vc)
      se[ok] <- sqrt(pmax(diag_vc[matched[ok]], 0))
    } else {
      se <- sqrt(pmax(diag_vc[seq_along(estimates)], 0))
    }
  }

  statistic <- estimates / se
  statistic[!is.finite(statistic)] <- NA_real_
  df_resid <- tryCatch(stats::df.residual(model), error = function(e) NA_real_)
  p_value <- if (is.finite(df_resid) && df_resid > 0) {
    2 * stats::pt(abs(statistic), df = df_resid, lower.tail = FALSE)
  } else {
    2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
  }
  p_value[!is.finite(p_value)] <- NA_real_

  out <- data.frame(
    Estimate = estimates,
    estimate = estimates,
    p.value = p_value,
    `Pr(>|t|)` = p_value,
    statistic = statistic,
    std.error = se,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(out) <- terms
  out
}

coefficient_value <- function(model, terms, column = c("Estimate", "estimate"), digits = NULL) {
  out <- tryCatch({
    coefs <- report_coefficient_frame(model)
    term <- first_matching_term(terms, rownames(coefs))
    if (is.na(term)) return(NA_real_)
    hit <- intersect(column, names(coefs))
    if (!length(hit)) return(NA_real_)
    suppressWarnings(as.numeric(coefs[term, hit[[1]]]))
  }, error = function(e) NA_real_)
  if (!is.null(digits) && is.finite(out)) out <- round(out, digits)
  out
}

p_value <- function(model, terms, digits = NULL) {
  out <- tryCatch({
    coefs <- report_coefficient_frame(model)
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
  out <- tryCatch({
    X <- stats::model.matrix(model)
    kappa(X, exact = TRUE)
  }, error = function(e) NA_real_)
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

first_stage_formula_from_iv_model <- function(model) {
  f <- tryCatch(stats::formula(model), error = function(e) NULL)
  if (is.null(f) || length(f) < 3L || !is.call(f[[3]]) || !identical(f[[3]][[1]], as.name("|"))) {
    return(NULL)
  }
  regressors <- all.vars(f[[3]][[2]])
  instruments <- all.vars(f[[3]][[3]])
  endogenous <- setdiff(regressors, instruments)
  if (!length(endogenous)) endogenous <- regressors[[1]]
  if (!length(endogenous) || is.na(endogenous[[1]]) || !nzchar(endogenous[[1]]) || !length(instruments)) {
    return(NULL)
  }
  stats::as.formula(paste(endogenous[[1]], "~", paste(instruments, collapse = " + ")))
}

first_stage_model_from_iv <- function(iv_models, district_panel = NULL) {
  model <- first_iv_model(iv_models)
  if (!inherits(model, "ivreg")) return(NULL)
  fs_formula <- first_stage_formula_from_iv_model(model)
  if (is.null(fs_formula)) return(NULL)

  data <- as_plain_data_frame(district_panel)
  if (!nrow(data) || any(!all.vars(fs_formula) %in% names(data))) {
    data <- tryCatch(as_plain_data_frame(stats::model.frame(model)), error = function(e) data.frame())
  }
  if (!nrow(data) || any(!all.vars(fs_formula) %in% names(data))) return(NULL)

  tryCatch(stats::lm(fs_formula, data = data), error = function(e) NULL)
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
