# Build named values for paper/report.qmd from targets outputs.

inline_expression_key <- function(expr) {
  paste0("inline_", substr(digest::digest(expr, algo = "xxhash32"), 1L, 8L))
}

report_value_status <- function(reason, status = "unavailable_in_draft", value = NA, display = "—") {
  list(status = status, reason = reason, value = value, display = display)
}

is_report_value_status <- function(value) {
  is.list(value) && !is.null(value$status) && !is.null(value$reason)
}

value_or_status <- function(value, reason) {
  if (length(value) == 0L || all(is.na(value))) report_value_status(reason) else value
}

as_plain_data_frame <- function(x) {
  tryCatch(as.data.frame(x), error = function(e) data.frame())
}

first_available_number <- function(x, candidates) {
  if (is.null(x)) return(NA_real_)
  x <- as_plain_data_frame(x)
  hit <- intersect(candidates, names(x))
  if (!length(hit) || !nrow(x)) return(NA_real_)
  suppressWarnings(as.numeric(x[[hit[[1]]]][[1]]))
}

first_available_text <- function(x, candidates, default = "unavailable in current draft pipeline") {
  if (is.null(x)) return(default)
  x <- as_plain_data_frame(x)
  hit <- intersect(candidates, names(x))
  if (!length(hit) || !nrow(x)) return(default)
  out <- as.character(x[[hit[[1]]]][[1]])
  if (!length(out) || is.na(out) || !nzchar(out)) default else out
}

coefficient_value <- function(model, terms, column = c("Estimate", "estimate"), digits = NULL) {
  out <- tryCatch({
    sm <- summary(model)
    coefs <- as.data.frame(sm$coefficients)
    term <- intersect(terms, rownames(coefs))
    if (!length(term)) return(NA_real_)
    hit <- intersect(column, names(coefs))
    if (!length(hit)) return(NA_real_)
    suppressWarnings(as.numeric(coefs[term[[1]], hit[[1]]]))
  }, error = function(e) NA_real_)
  if (!is.null(digits) && is.finite(out)) out <- round(out, digits)
  out
}

p_value <- function(model, terms, digits = NULL) {
  out <- tryCatch({
    sm <- summary(model)
    coefs <- as.data.frame(sm$coefficients)
    term <- intersect(terms, rownames(coefs))
    if (!length(term)) return(NA_real_)
    hit <- intersect(c("Pr(>|t|)", "Pr(>|z|)", "p.value", "p_value"), names(coefs))
    if (!length(hit)) return(NA_real_)
    suppressWarnings(as.numeric(coefs[term[[1]], hit[[1]]]))
  }, error = function(e) NA_real_)
  if (!is.null(digits) && is.finite(out)) out <- signif(out, digits)
  out
}

lookup_ame <- function(ame_results, term_pattern, value_col = "estimate", multiply = 1, digits = NULL, contrast_pattern = NULL) {
  x <- as_plain_data_frame(ame_results)
  if (!nrow(x) || !"term" %in% names(x)) return(NA_real_)
  keep <- grepl(term_pattern, x$term)
  if (!is.null(contrast_pattern) && "contrast" %in% names(x)) keep <- keep & grepl(contrast_pattern, x$contrast)
  x <- x[keep, , drop = FALSE]
  if (!nrow(x) || !value_col %in% names(x)) return(NA_real_)
  out <- suppressWarnings(as.numeric(x[[value_col]][[1]])) * multiply
  if (!is.null(digits) && is.finite(out)) out <- round(out, digits)
  out
}

lookup_ame_s_value <- function(ame_results, term_pattern, contrast_pattern = NULL, digits = NULL) {
  out <- lookup_ame(ame_results, term_pattern, value_col = "s.value", contrast_pattern = contrast_pattern)
  if (is.finite(out)) {
    if (!is.null(digits)) out <- signif(out, digits)
    return(out)
  }
  p <- lookup_ame(ame_results, term_pattern, value_col = "p.value", contrast_pattern = contrast_pattern)
  if (!is.finite(p) || p <= 0) return(NA_real_)
  out <- -log2(p)
  if (!is.null(digits)) out <- signif(out, digits)
  out
}

selection_summary_value <- function(selection_data, relation_to_head, sex, summary = c("mean_age", "share_children")) {
  summary <- match.arg(summary)
  x <- as_plain_data_frame(selection_data)
  relation_col <- first_col(x, c("RELATION_TO_HEAD", "relation_to_head"))
  sex_col <- first_col(x, c("SEX", "sex"))
  age_col <- first_col(x, c("AGE", "age"))
  if (is.null(relation_col) || is.null(sex_col) || is.null(age_col)) return(NA_real_)

  sex_raw <- as.character(x[[sex_col]])
  sex_num <- suppressWarnings(as.numeric(sex_raw))
  sex_keep <- sex_num == sex | (sex == 1 & tolower(sex_raw) == "male") | (sex == 2 & tolower(sex_raw) == "female")
  keep <- suppressWarnings(as.numeric(as.character(x[[relation_col]])) == relation_to_head) &
    sex_keep
  ages <- suppressWarnings(as.numeric(as.character(x[[age_col]][keep])))
  ages <- ages[is.finite(ages)]
  if (!length(ages)) return(NA_real_)
  if (identical(summary, "mean_age")) return(round(mean(ages), 1))
  round(mean(ages <= 17), 3)
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

spatial_p_value <- function(diag, pattern = NULL) {
  x <- as_plain_data_frame(diag)
  if (!nrow(x)) return(NA_real_)
  if (!is.null(pattern)) {
    text_cols <- intersect(c("estimand", "model", "target", "outcome", "name", "test"), names(x))
    if (length(text_cols)) {
      keep <- Reduce(`|`, lapply(text_cols, function(col) grepl(pattern, x[[col]], ignore.case = TRUE)))
      if (any(keep)) x <- x[keep, , drop = FALSE]
    }
  }
  out <- first_available_number(x, c("p.value", "p_value", "p", "pval"))
  if (is.finite(out)) signif(out, 3) else NA_real_
}

add_inline_value <- function(values, expr, value) {
  values[[inline_expression_key(expr)]] <- value
  values[[expr]] <- value
  values
}

add_report_value <- function(values, name, expr, value, reason) {
  values[[name]] <- value_or_status(value, reason)
  add_inline_value(values, expr, values[[name]])
}

#' Build report values from current targets
#'
#' @return Named list of values used by paper/report.qmd.
build_report_values <- function(ame_results, first_stage_tests, iv_models, selection_data, district_panel, diag_spatial_autocorrelation = NULL, cfg = list()) {
  values <- list()

  model <- if (is.list(iv_models) && length(iv_models)) iv_models[[1]] else iv_models
  unavailable_ame <- "Full AME result is not available in the current draft pipeline."
  unavailable_iv <- "The requested IV coefficient is not available from the active model specification."
  unavailable_first_stage <- "The requested first-stage coefficient is not available from the active model specification."
  unavailable_selection <- "The requested relationship/age summary is not available from the active selection data."

  values <- add_report_value(values, "head_male_age", "edu0708b4 %>% filter(RELATION_TO_HEAD==1 & SEX==1) %>% summarise(round(mean(AGE),1)) %>% .[[1]]", selection_summary_value(selection_data, 1, 1), unavailable_selection)
  values <- add_report_value(values, "married_child_male_age", "edu0708b4 %>% filter(RELATION_TO_HEAD==3 & SEX==1) %>% summarise(round(mean(AGE),1)) %>% .[[1]]", selection_summary_value(selection_data, 3, 1), unavailable_selection)
  values <- add_report_value(values, "parent_in_law_male_age", "edu0708b4 %>% filter(RELATION_TO_HEAD==7 & SEX==1) %>% summarise(round(mean(AGE),1)) %>% .[[1]]", selection_summary_value(selection_data, 7, 1), unavailable_selection)
  values <- add_report_value(values, "spouse_male_age", "edu0708b4 %>% filter(RELATION_TO_HEAD==2 & SEX==1) %>% summarise(round(mean(AGE),1)) %>% .[[1]]", selection_summary_value(selection_data, 2, 1), unavailable_selection)
  values <- add_report_value(values, "spouse_male_child_share", "edu0708b4 %>% filter(RELATION_TO_HEAD==2 & SEX==1) %>% summarise(round(mean(AGE<=17),3)) %>% .[[1]]", selection_summary_value(selection_data, 2, 1, "share_children"), unavailable_selection)
  values <- add_report_value(values, "unmarried_child_male_age", "edu0708b4 %>% filter(RELATION_TO_HEAD==5 & SEX==1) %>% summarise(round(mean(AGE),1)) %>% .[[1]]", selection_summary_value(selection_data, 5, 1), unavailable_selection)

  values <- add_report_value(values, "ame_age", "mfx_df %>% filter(term==\"AGE\") %>% pull(estimate) %>% round(3)", lookup_ame(ame_results, "AGE", digits = 3), unavailable_ame)
  values <- add_report_value(values, "ame_age_abs_pct", "mfx_df %>% filter(term==\"AGE\") %>% pull(estimate) %>% round(3) %>% abs()*100", abs(lookup_ame(ame_results, "AGE", multiply = 100, digits = 3)), unavailable_ame)
  values <- add_report_value(values, "ame_muslim", "mfx_df %>% filter(term==\"RELIGION\" & grepl(\"Muslim\",contrast)) %>% pull(estimate) %>% round(3)", lookup_ame(ame_results, "RELIGION", contrast_pattern = "Muslim", digits = 3), unavailable_ame)
  values <- add_report_value(values, "ame_age_pct", "mfx_df %>% filter(term==\"AGE\") %>% pull(estimate) %>% round(3)*100", lookup_ame(ame_results, "AGE", multiply = 100, digits = 3), unavailable_ame)
  values <- add_report_value(values, "ame_sex_pct", "mfx_df %>% filter(term==\"SEX\") %>% pull(estimate) %>% round(3)*100", lookup_ame(ame_results, "SEX", multiply = 100, digits = 3), unavailable_ame)
  values <- add_report_value(values, "ame_hh_size_pct", "mfx_df %>% filter(term==\"HH_SIZE\") %>% pull(estimate) %>% round(3)*100", lookup_ame(ame_results, "HH_SIZE", multiply = 100, digits = 3), unavailable_ame)
  values <- add_report_value(values, "ame_muslim_pct", "mfx_df %>% filter(term==\"RELIGION\" & grepl(\"Muslim\", contrast)) %>% pull(estimate) %>% round(3)*100", lookup_ame(ame_results, "RELIGION", contrast_pattern = "Muslim", multiply = 100, digits = 3), unavailable_ame)
  values <- add_report_value(values, "ame_st_pct", "mfx_df %>% filter(term==\"SOCIAL_GROUP\" & grepl(\"Tribe\", contrast)) %>% pull(estimate) %>% round(3)*100", lookup_ame(ame_results, "SOCIAL_GROUP", contrast_pattern = "Tribe", multiply = 100, digits = 3), unavailable_ame)
  values <- add_report_value(values, "ame_textbooks_s", "mfx_df %>% filter(grepl(\"dmean_num\", term) & grepl(\"RECD_TXT_BOOKS\", term)) %>% pull(s.value) %>% signif(3)", lookup_ame_s_value(ame_results, "dmean_num.*RECD_TXT_BOOKS|RECD_TXT_BOOKS.*dmean_num", digits = 3), unavailable_ame)

  values <- add_report_value(values, "kappa", "kappa %>% format(scientific = FALSE, digits = 7)", condition_number_value(model), "The model design matrix condition number is not available from the active model specification.")

  values$partial_f <- value_or_status(first_available_number(first_stage_tests, c("partial_f", "statistic", "f_stat", "F")), unavailable_first_stage)
  values$partial_p <- value_or_status(first_available_number(first_stage_tests, c("partial_p", "p.value", "p_value", "p")), unavailable_first_stage)
  values <- add_inline_value(values, "partial_f %>% round(digits = 2)", if (is_report_value_status(values$partial_f)) values$partial_f else round(values$partial_f, 2))
  values <- add_inline_value(values, "partial_p %>% signif(digits = 2)", if (is_report_value_status(values$partial_p)) values$partial_p else signif(values$partial_p, 2))

  values <- add_report_value(values, "first_stage_linguistic_distance_estimate", "summary(first_stage_consumption, vcov = vcov_first_stage_consumption)$coefficients[\"wavg_ling_degrees\",1] %>% round(2)", first_stage_report_value(first_stage_tests, iv_models, district_panel, c("wavg_ling_degrees", "linguistic_distance", "ling_degrees"), "estimate", 2), unavailable_first_stage)
  values$first_stage_linguistic_distance_p <- value_or_status(first_stage_report_value(first_stage_tests, iv_models, district_panel, c("wavg_ling_degrees", "linguistic_distance", "ling_degrees"), "p.value", 3), unavailable_first_stage)
  values <- add_report_value(values, "first_stage_gini_estimate", "summary(first_stage_consumption, vcov = vcov_first_stage_consumption)$coefficients[\"gini_cons_0708\",1] %>% round(2)", first_stage_report_value(first_stage_tests, iv_models, district_panel, c("gini_cons_0708", "gini_consumption_2007"), "estimate", 2), unavailable_first_stage)
  values$first_stage_gini_p <- value_or_status(first_stage_report_value(first_stage_tests, iv_models, district_panel, c("gini_cons_0708", "gini_consumption_2007"), "p.value", 3), unavailable_first_stage)

  iv_terms <- list(
    iv_emie = c("EMIE", "emie_2007"),
    iv_pct_urban = "pct_urban",
    iv_pct_head_secondary_plus = "pct_head_secondary_plus",
    iv_pct_muslim = "pct_muslim",
    iv_pct_st = "pct_st",
    iv_pct_obc = "pct_obc",
    iv_pct_medium_land = "pct_medium_land",
    iv_pct_large_land = "pct_large_land",
    iv_gini_cons_0708 = c("gini_cons_0708", "gini_consumption_2007"),
    iv_pct_fem_head = "pct_fem_head"
  )
  for (name in names(iv_terms)) {
    values[[paste0(name, "_estimate")]] <- value_or_status(coefficient_value(model, iv_terms[[name]], digits = 3), unavailable_iv)
    values[[paste0(name, "_p")]] <- value_or_status(p_value(model, iv_terms[[name]], digits = 3), unavailable_iv)
  }

  values <- add_inline_value(values, "summary(model_consumption_iv, vcov = vcov_model_consumption_iv)$coefficients[\"EMIE\",4] %>% round(digits = 2)", values$iv_emie_p)
  values <- add_inline_value(values, "summary(model_consumption_iv, vcov = vcov_model_consumption_iv)$coefficients[\"EMIE\",1] %>% round(digits = 2)", value_or_status(coefficient_value(model, iv_terms$iv_emie, digits = 2), unavailable_iv))
  values <- add_inline_value(values, "summary(model_consumption_iv, vcov = vcov_model_consumption_iv)$coefficients[\"pct_urban\",1] %>% round(digits = 2)", value_or_status(coefficient_value(model, iv_terms$iv_pct_urban, digits = 2), unavailable_iv))
  values <- add_inline_value(values, "summary(model_consumption_iv, vcov = vcov_model_consumption_iv)$coefficients[\"pct_urban\",4] %>% round(digits = 3)", values$iv_pct_urban_p)
  values <- add_inline_value(values, "summary(model_consumption_iv, vcov = vcov_model_consumption_iv)$coefficients[\"pct_head_secondary_plus\",1] %>% round(digits = 2)", value_or_status(coefficient_value(model, iv_terms$iv_pct_head_secondary_plus, digits = 2), unavailable_iv))
  values <- add_inline_value(values, "summary(model_consumption_iv, vcov = vcov_model_consumption_iv)$coefficients[\"pct_head_secondary_plus\",4] %>% round(digits = 3)", values$iv_pct_head_secondary_plus_p)
  values <- add_inline_value(values, "summary(model_consumption_iv, vcov = vcov_model_consumption_iv)$coefficients[\"pct_muslim\",\"Estimate\"] %>% round(digits=3)", values$iv_pct_muslim_estimate)
  values <- add_inline_value(values, "summary(model_consumption_iv, vcov = vcov_model_consumption_iv)$coefficients[\"pct_st\",\"Estimate\"] %>% round(digits=3)", values$iv_pct_st_estimate)
  values <- add_inline_value(values, "summary(model_consumption_iv, vcov = vcov_model_consumption_iv)$coefficients[\"pct_obc\",\"Estimate\"] %>% round(digits=3)", values$iv_pct_obc_estimate)
  values <- add_inline_value(values, "summary(model_consumption_iv, vcov = vcov_model_consumption_iv)$coefficients[\"pct_medium_land\",4] %>% round(digits = 3)", values$iv_pct_medium_land_p)
  values <- add_inline_value(values, "summary(model_consumption_iv, vcov = vcov_model_consumption_iv)$coefficients[\"pct_large_land\",4] %>% round(digits = 3)", values$iv_pct_large_land_p)
  values <- add_inline_value(values, "summary(model_consumption_iv, vcov = vcov_model_consumption_iv)$coefficients[\"gini_cons_0708\",4] %>% round(digits = 3)", values$iv_gini_cons_0708_p)
  values <- add_inline_value(values, "summary(model_consumption_iv, vcov = vcov_model_consumption_iv)$coefficients[\"pct_fem_head\",4] %>% round(digits = 3)", values$iv_pct_fem_head_p)

  values
}
