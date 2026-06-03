# Build named values for paper/report.qmd from targets outputs.

inline_expression_key <- function(expr) {
  paste0("inline_", substr(digest::digest(expr, algo = "xxhash32"), 1L, 8L))
}

first_available_number <- function(x, candidates) {
  if (is.null(x)) return(NA_real_)
  x <- tryCatch(as.data.frame(x), error = function(e) data.frame())
  hit <- intersect(candidates, names(x))
  if (!length(hit) || !nrow(x)) return(NA_real_)
  suppressWarnings(as.numeric(x[[hit[[1]]]][[1]]))
}

coefficient_value <- function(model, term, column = c("Estimate", "estimate"), digits = NULL) {
  out <- tryCatch({
    sm <- summary(model)
    coefs <- as.data.frame(sm$coefficients)
    if (!term %in% rownames(coefs)) return(NA_real_)
    hit <- intersect(column, names(coefs))
    if (!length(hit)) return(NA_real_)
    suppressWarnings(as.numeric(coefs[term, hit[[1]]]))
  }, error = function(e) NA_real_)
  if (!is.null(digits) && is.finite(out)) out <- round(out, digits)
  out
}

p_value <- function(model, term, digits = NULL) {
  out <- tryCatch({
    sm <- summary(model)
    coefs <- as.data.frame(sm$coefficients)
    if (!term %in% rownames(coefs)) return(NA_real_)
    hit <- intersect(c("Pr(>|t|)", "Pr(>|z|)", "p.value", "p_value"), names(coefs))
    if (!length(hit)) return(NA_real_)
    suppressWarnings(as.numeric(coefs[term, hit[[1]]]))
  }, error = function(e) NA_real_)
  if (!is.null(digits) && is.finite(out)) out <- signif(out, digits)
  out
}

lookup_ame <- function(ame_results, term_pattern, value_col = "estimate", multiply = 1, digits = NULL, contrast_pattern = NULL) {
  x <- tryCatch(as.data.frame(ame_results), error = function(e) data.frame())
  if (!nrow(x) || !"term" %in% names(x)) return(NA_real_)
  keep <- grepl(term_pattern, x$term)
  if (!is.null(contrast_pattern) && "contrast" %in% names(x)) keep <- keep & grepl(contrast_pattern, x$contrast)
  x <- x[keep, , drop = FALSE]
  if (!nrow(x) || !value_col %in% names(x)) return(NA_real_)
  out <- suppressWarnings(as.numeric(x[[value_col]][[1]])) * multiply
  if (!is.null(digits) && is.finite(out)) out <- round(out, digits)
  out
}

add_inline_value <- function(values, expr, value) {
  values[[inline_expression_key(expr)]] <- value
  values
}

#' Build report values from current targets
#'
#' @return Named list of values used by paper/report.qmd.
build_report_values <- function(ame_results, first_stage_tests, iv_models, selection_data, district_panel, cfg = list()) {
  values <- list()

  values$partial_f <- first_available_number(first_stage_tests, c("partial_f", "statistic", "f_stat", "F"))
  values$partial_p <- first_available_number(first_stage_tests, c("partial_p", "p.value", "p_value", "p"))

  model <- if (is.list(iv_models) && length(iv_models)) iv_models[[1]] else iv_models
  values$iv_emie_estimate <- coefficient_value(model, "EMIE", c("Estimate", "estimate"), digits = 2)
  values$iv_emie_p <- p_value(model, "EMIE", digits = 2)
  values$iv_pct_urban_estimate <- coefficient_value(model, "pct_urban", c("Estimate", "estimate"), digits = 2)
  values$iv_pct_urban_p <- p_value(model, "pct_urban", digits = 3)
  values$iv_pct_head_secondary_plus_estimate <- coefficient_value(model, "pct_head_secondary_plus", c("Estimate", "estimate"), digits = 2)
  values$iv_pct_head_secondary_plus_p <- p_value(model, "pct_head_secondary_plus", digits = 3)

  values$ame_age <- lookup_ame(ame_results, "AGE", digits = 3)
  values$ame_age_pct <- lookup_ame(ame_results, "AGE", multiply = 100, digits = 3)
  values$ame_sex_pct <- lookup_ame(ame_results, "SEX", multiply = 100, digits = 3)
  values$ame_hh_size_pct <- lookup_ame(ame_results, "HH_SIZE", multiply = 100, digits = 3)
  values$ame_muslim_pct <- lookup_ame(ame_results, "RELIGION", contrast_pattern = "Muslim", multiply = 100, digits = 3)
  values$ame_st_pct <- lookup_ame(ame_results, "SOCIAL_GROUP", contrast_pattern = "Tribe", multiply = 100, digits = 3)

  values <- add_inline_value(values, "partial_f %>% round(digits = 2)", if (is.finite(values$partial_f)) round(values$partial_f, 2) else NA_real_)
  values <- add_inline_value(values, "partial_p %>% signif(digits = 2)", if (is.finite(values$partial_p)) signif(values$partial_p, 2) else NA_real_)
  values <- add_inline_value(values, "mfx_df %>% filter(term==\"AGE\") %>% pull(estimate) %>% round(3)", values$ame_age)
  values <- add_inline_value(values, "mfx_df %>% filter(term==\"AGE\") %>% pull(estimate) %>% round(3) %>% abs()*100", abs(values$ame_age_pct))
  values <- add_inline_value(values, "mfx_df %>% filter(term==\"AGE\") %>% pull(estimate) %>% round(3)*100", values$ame_age_pct)
  values <- add_inline_value(values, "mfx_df %>% filter(term==\"SEX\") %>% pull(estimate) %>% round(3)*100", values$ame_sex_pct)
  values <- add_inline_value(values, "mfx_df %>% filter(term==\"HH_SIZE\") %>% pull(estimate) %>% round(3)*100", values$ame_hh_size_pct)
  values <- add_inline_value(values, "mfx_df %>% filter(term==\"RELIGION\" & grepl(\"Muslim\",contrast)) %>% pull(estimate) %>% round(3)", values$ame_muslim_pct / 100)
  values <- add_inline_value(values, "mfx_df %>% filter(term==\"RELIGION\" & grepl(\"Muslim\", contrast)) %>% pull(estimate) %>% round(3)*100", values$ame_muslim_pct)
  values <- add_inline_value(values, "mfx_df %>% filter(term==\"SOCIAL_GROUP\" & grepl(\"Tribe\", contrast)) %>% pull(estimate) %>% round(3)*100", values$ame_st_pct)

  values <- add_inline_value(values, "summary(model_consumption_iv, vcov = vcov_model_consumption_iv)$coefficients[\"EMIE\",4] %>% round(digits = 2)", values$iv_emie_p)
  values <- add_inline_value(values, "summary(model_consumption_iv, vcov = vcov_model_consumption_iv)$coefficients[\"EMIE\",1] %>% round(digits = 2)", values$iv_emie_estimate)
  values <- add_inline_value(values, "summary(model_consumption_iv, vcov = vcov_model_consumption_iv)$coefficients[\"pct_urban\",1] %>% round(digits = 2)", values$iv_pct_urban_estimate)
  values <- add_inline_value(values, "summary(model_consumption_iv, vcov = vcov_model_consumption_iv)$coefficients[\"pct_urban\",4] %>% round(digits = 3)", values$iv_pct_urban_p)
  values <- add_inline_value(values, "summary(model_consumption_iv, vcov = vcov_model_consumption_iv)$coefficients[\"pct_head_secondary_plus\",1] %>% round(digits = 2)", values$iv_pct_head_secondary_plus_estimate)
  values <- add_inline_value(values, "summary(model_consumption_iv, vcov = vcov_model_consumption_iv)$coefficients[\"pct_head_secondary_plus\",4] %>% round(digits = 3)", values$iv_pct_head_secondary_plus_p)

  values
}
