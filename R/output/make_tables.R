# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-table-generation

is_final_mode <- function(cfg) {
  identical(cfg$mode, "final")
}

fail_final_if_status <- function(x, cfg, label) {
  df <- as.data.frame(x)
  if (is_final_mode(cfg) && "status" %in% names(df) && any(!is.na(df$status) & df$status != "mapped")) {
    stop("Final table generation requires completed model output for ", label, ".", call. = FALSE)
  }
  invisible(TRUE)
}

numeric_summary <- function(df, variables = NULL) {
  df <- as.data.frame(df)
  if (is.null(variables)) variables <- names(df)
  variables <- intersect(variables, names(df))
  out <- lapply(variables, function(v) {
    x <- suppressWarnings(as.numeric(as.character(df[[v]])))
    x <- x[is.finite(x)]
    if (!length(x)) return(NULL)
    data.frame(
      variable = v,
      min = min(x),
      q1 = unname(stats::quantile(x, 0.25)),
      median = stats::median(x),
      q3 = unname(stats::quantile(x, 0.75)),
      max = max(x),
      mean = mean(x),
      sd = stats::sd(x),
      n = length(x),
      stringsAsFactors = FALSE
    )
  })
  out <- safe_bind_rows(out)
  if (!nrow(out)) data.frame(variable = character(), n = integer()) else out
}

categorical_summary <- function(df, variables) {
  df <- as.data.frame(df)
  variables <- intersect(variables, names(df))
  safe_bind_rows(lapply(variables, function(v) {
    x <- df[[v]]
    tab <- sort(table(x, useNA = "ifany"), decreasing = TRUE)
    if (!length(tab)) return(NULL)
    data.frame(
      variable = v,
      category = names(tab),
      n = as.integer(tab),
      share = round(as.numeric(tab) / sum(tab), 4),
      stringsAsFactors = FALSE
    )
  }))
}

tidy_iv_models <- function(iv_models) {
  if (!is.list(iv_models)) iv_models <- list(model = iv_models)
  safe_bind_rows(lapply(names(iv_models), function(name) {
    model <- iv_models[[name]]
    if (is.list(model) && !is.null(model$status)) {
      return(data.frame(model = name, term = NA_character_, estimate = NA_real_, std.error = NA_real_, statistic = NA_real_, p.value = NA_real_, status = model$status, reason = model$reason %||% NA_character_))
    }
    coefs <- tryCatch(as.data.frame(summary(model)$coefficients), error = function(e) data.frame())
    if (!nrow(coefs)) {
      return(data.frame(model = name, term = NA_character_, estimate = NA_real_, std.error = NA_real_, statistic = NA_real_, p.value = NA_real_, status = "unavailable", reason = "Model coefficients are unavailable."))
    }
    data.frame(
      model = name,
      term = rownames(coefs),
      estimate = coefs[[intersect(c("Estimate", "estimate"), names(coefs))[[1]]]],
      std.error = coefs[[intersect(c("Std. Error", "std.error"), names(coefs))[[1]]]],
      statistic = coefs[[intersect(c("t value", "z value", "statistic"), names(coefs))[[1]]]],
      p.value = coefs[[intersect(c("Pr(>|t|)", "Pr(>|z|)", "p.value"), names(coefs))[[1]]]],
      status = "estimated",
      reason = NA_character_,
      stringsAsFactors = FALSE
    )
  }))
}

#' make tables
#'
#' @return A named list of data frames consumed by save_tables().
make_tables <- function(selection_data, ame_results, district_panel, iv_models, first_stage_tests, cfg) {
  fail_final_if_status(ame_results, cfg, "average marginal effects")
  fail_final_if_status(first_stage_tests, cfg, "first-stage regression")

  cons_iv <- tidy_iv_models(iv_models)
  fail_final_if_status(cons_iv, cfg, "second-stage IV regression")

  list(
    selection_n = data.frame(n = nrow(as.data.frame(selection_data))),
    sum_tbl_probit_quant = make_selection_summary_numeric_table(selection_data),
    sum_tbl_probit_cat = make_selection_summary_categorical_table(selection_data),
    probit_mfx = make_probit_ame_table(ame_results),
    sum_tbl_iv = make_iv_summary_table(district_panel),
    fs_cons = make_first_stage_table(first_stage_tests),
    cons_iv = make_second_stage_table(iv_models),
    ame_results = as.data.frame(ame_results),
    first_stage = as.data.frame(first_stage_tests)
  )
}

#' make selection summary numeric table
#'
#' @return A data frame of numeric summary statistics.
make_selection_summary_numeric_table <- function(selection_data) {
  numeric_summary(selection_data, c(
    "AGE", "HH_SIZE", "DIST_FROM_NEAREST_PRIMARY_CLASS", "DIST_FROM_UPPER_PRIMARY_CLASS",
    "DIST_FROM_SEC_CLASS", "IS_EDU_FREE", "RECD_TXT_BOOKS", "TOTAL", "GRAND_TOTAL",
    "weight", "enrolled"
  ))
}

#' make selection summary categorical table
#'
#' @return A data frame of categorical counts and shares.
make_selection_summary_categorical_table <- function(selection_data) {
  categorical_summary(selection_data, c("SEX", "RELIGION", "SOCIAL_GROUP", "RELATION_TO_HEAD", "SECTOR", "TYPE_OF_INSTT"))
}

#' make probit ame table
#'
#' @return A data frame of average marginal effects or explicit status rows.
make_probit_ame_table <- function(ame_results) {
  as.data.frame(ame_results)
}

#' make iv summary table
#'
#' @return A data frame of district-panel summary statistics.
make_iv_summary_table <- function(district_panel) {
  numeric_summary(district_panel)
}

#' make first stage table
#'
#' @return A data frame of first-stage estimates or explicit status rows.
make_first_stage_table <- function(first_stage_tests) {
  as.data.frame(first_stage_tests)
}

#' make second stage table
#'
#' @return A data frame of second-stage estimates or explicit status rows.
make_second_stage_table <- function(iv_models) {
  tidy_iv_models(iv_models)
}

#' make diagnostic tables
#'
#' @return A named list of diagnostic tables.
make_diagnostic_tables <- function(...) {
  list()
}
# sample-end: code-table-generation
