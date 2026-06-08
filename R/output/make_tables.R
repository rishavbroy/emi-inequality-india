# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-table-generation

is_final_mode <- function(cfg) identical(cfg$mode, "final")

fail_final_if_status <- function(x, cfg, label) {
  df <- as.data.frame(x)
  ok_status <- c("mapped", "estimated")
  if (is_final_mode(cfg) && "status" %in% names(df) && any(!is.na(df$status) & !df$status %in% ok_status)) {
    stop("Final table generation requires completed model output for ", label, ".", call. = FALSE)
  }
  invisible(TRUE)
}

legacy_numeric_stats <- function(df, meta, cost_vars = character()) {
  df <- as.data.frame(df)
  rows <- lapply(seq_len(nrow(meta)), function(i) {
    v <- meta$var[[i]]
    if (!v %in% names(df)) return(NULL)
    x <- suppressWarnings(as.numeric(as.character(df[[v]])))
    ok <- is.finite(x)
    if (!any(ok)) return(NULL)
    stats <- c(
      Min = min(x[ok], na.rm = TRUE),
      `1Q` = unname(stats::quantile(x[ok], .25, na.rm = TRUE)),
      Med = stats::median(x[ok], na.rm = TRUE),
      `3Q` = unname(stats::quantile(x[ok], .75, na.rm = TRUE)),
      Max = max(x[ok], na.rm = TRUE),
      Mean = mean(x[ok], na.rm = TRUE),
      SD = stats::sd(x[ok], na.rm = TRUE)
    )
    z <- data.frame(var = v, label = meta$label[[i]], N = sum(ok), t(stats), check.names = FALSE, stringsAsFactors = FALSE)
    z
  })
  out <- safe_bind_rows(rows)
  if (!nrow(out)) return(data.frame(var = character(), label = character(), N = integer()))
  for (nm in intersect(c("Min", "1Q", "Med", "3Q", "Max", "Mean", "SD"), names(out))) {
    x <- suppressWarnings(as.numeric(out[[nm]]))
    out[[nm]] <- ifelse(out$var %in% cost_vars, formatC(x, format = "f", big.mark = ",", digits = 2), sprintf("%.2f", x))
  }
  out
}

legacy_categorical_stats <- function(df, meta) {
  df <- as.data.frame(df)
  rows <- lapply(seq_len(nrow(meta)), function(i) {
    v <- meta$var[[i]]
    if (!v %in% names(df)) return(NULL)
    x <- df[[v]]
    x <- x[!is.na(x)]
    if (!length(x)) return(NULL)
    freq <- table(x)
    data.frame(
      var = v,
      label = meta$label[[i]],
      N = length(x),
      Values = paste(names(freq), collapse = ", "),
      Mode = names(freq)[which.max(freq)],
      `% Mode` = round(max(freq) / sum(freq) * 100, 1),
      `Least Freq.` = names(freq)[which.min(freq)],
      `% Least Freq.` = round(min(freq) / sum(freq) * 100, 1),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  out <- safe_bind_rows(rows)
  if (!nrow(out)) data.frame(var = character(), label = character(), N = integer()) else out
}

numeric_summary <- function(df, variables = NULL) {
  df <- as.data.frame(df)
  if (is.null(variables)) variables <- names(df)
  variables <- intersect(variables, names(df))
  out <- lapply(variables, function(v) {
    x <- suppressWarnings(as.numeric(as.character(df[[v]])))
    x <- x[is.finite(x)]
    if (!length(x)) return(NULL)
    data.frame(variable = v, min = min(x), q1 = unname(stats::quantile(x, 0.25)), median = stats::median(x), q3 = unname(stats::quantile(x, 0.75)), max = max(x), mean = mean(x), sd = stats::sd(x), n = length(x), stringsAsFactors = FALSE)
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
    data.frame(variable = v, category = names(tab), n = as.integer(tab), share = round(as.numeric(tab) / sum(tab), 4), stringsAsFactors = FALSE)
  }))
}

tidy_iv_models <- function(iv_models) {
  if (!is.list(iv_models)) iv_models <- list(model = iv_models)
  safe_bind_rows(lapply(names(iv_models), function(name) {
    model <- iv_models[[name]]
    if (is.list(model) && !is.null(model$status)) {
      return(data.frame(model = name, term = NA_character_, estimate = NA_real_, std.error = NA_real_, statistic = NA_real_, p.value = NA_real_, status = model$status, reason = model$reason %||% NA_character_))
    }
    coefs <- tryCatch({
      vc <- clustered_model_vcov(model)
      if (is.null(vc)) as.data.frame(summary(model)$coefficients) else as.data.frame(lmtest::coeftest(model, vcov. = vc))
    }, error = function(e) data.frame())
    if (!nrow(coefs)) return(data.frame(model = name, term = NA_character_, estimate = NA_real_, std.error = NA_real_, statistic = NA_real_, p.value = NA_real_, status = "unavailable", reason = "Model coefficients are unavailable."))
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

clustered_model_vcov <- function(model) {
  cluster <- attr(model, "cluster_state")
  cluster <- stats::na.omit(cluster)
  if (!is.null(cluster) && length(unique(cluster)) > 1L && requireNamespace("sandwich", quietly = TRUE)) {
    force(cluster)
    return(function(x) sandwich::vcovCL(x, cluster = cluster))
  }
  NULL
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

make_selection_summary_numeric_table <- function(selection_data) {
  meta <- data.frame(
    var = c("AGE", "HH_SIZE", "ENROLLMENT_COST", "dmean_num_IS_EDU_FREE", "dmean_num_TUTION_FEE_WAIVED", "dmean_num_RECD_SCHOLARSHIP_STIPEND", "dmean_num_RECD_TXT_BOOKS", "dmean_num_RECD_STATIONERY", "dmean_num_MID_DAY_MEAL_ETC_RECD"),
    label = c("Age", "Household size", "Enrollment cost (Rs.)", "Educ. free available? (Yes = 1)", "Tuition waived?", "Scholarship/Stipend?", "Textbooks received?", "Stationery received?", "Mid-day meal or more received?"),
    stringsAsFactors = FALSE
  )
  legacy_numeric_stats(selection_data, meta, cost_vars = "ENROLLMENT_COST")
}

make_selection_summary_categorical_table <- function(selection_data) {
  meta <- data.frame(
    var = c("SEX", "RELIGION", "SOCIAL_GROUP", "SECTOR", "DIST_FROM_NEAREST_PRIMARY_CLASS", "father_educ"),
    label = c("Sex", "Religion", "Social group", "Urban", "Distance of nearest primary class", "Father's education"),
    stringsAsFactors = FALSE
  )
  legacy_categorical_stats(selection_data, meta)
}

make_probit_ame_table <- function(ame_results) {
  out <- as.data.frame(ame_results, stringsAsFactors = FALSE)
  if (!"Term" %in% names(out)) out$Term <- out$term
  out$stars <- ifelse(is.finite(out$p.value) & out$p.value < 0.001, "***", ifelse(is.finite(out$p.value) & out$p.value < 0.01, "**", ifelse(is.finite(out$p.value) & out$p.value < 0.05, "*", "")))
  data.frame(
    Term = out$Term,
    Estimate = ifelse(is.finite(out$estimate), paste0(sprintf("%.3f", out$estimate), out$stars), NA_character_),
    `Std. Error` = ifelse(is.finite(out$std.error), sprintf("(%.3f)", out$std.error), NA_character_),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

make_iv_summary_table <- function(district_panel) {
  meta <- data.frame(
    var = c("EMIE", "wavg_ling_degrees", "npeople_0708", "consumption_0708", "gini_cons_0708", "pct_urban", "avg_hh_size", "dependency_ratio", "pct_fem_head", "pct_hindu", "pct_muslim", "pct_other_religion", "pct_st", "pct_sc", "pct_obc", "pct_small_land", "pct_medium_land", "pct_large_land", "pct_head_illiterate", "pct_head_lit_to_primary", "pct_head_secondary_plus", "pct_pucca", "npeople_1718", "consumption_1718", "gini_cons_1718", "consumption_pct_change", "gini_change"),
    label = c("EMIE", "Ling. Distance", "Population", "Consumption", "Gini of Consumption", "Pct. Urban", "Avg. HH Size", "Dependency Ratio × 100", "Pct. Female Head", "Pct. Hindu", "Pct. Muslim", "Pct. Other", "Pct. ST", "Pct. SC", "Pct. OBC", "Pct. Small Land-Owner", "Pct. Med. Land-Owner", "Pct. Large Land-Owner", "Pct. Head Educ., Illiterate", "Pct. Head Educ., Lit.-Primary", "Pct. Head Educ., Secondary+", "Pct. Pucca", "Population", "Consumption", "Gini of Consumption", "$%\\Delta\\text{Consumption}$", "$\\Delta\\text{Gini}^{\\text{Consumption}}$"),
    stringsAsFactors = FALSE
  )
  legacy_numeric_stats(district_panel, meta, cost_vars = c("npeople_0708", "npeople_1718"))
}

make_first_stage_table <- function(first_stage_tests) {
  fs <- as.data.frame(first_stage_tests, stringsAsFactors = FALSE)
  if ("model" %in% names(fs) && any(fs$model %in% c("consumption", "baseline"))) fs <- fs[fs$model %in% c("consumption", "baseline"), , drop = FALSE]
  fs <- fs[fs$status == "estimated" & fs$term %in% c("wavg_ling_degrees", "(Intercept)"), , drop = FALSE]
  if (!nrow(fs)) return(as.data.frame(first_stage_tests))
  fs$stars <- ifelse(is.finite(fs$p.value) & fs$p.value < 0.001, "***", ifelse(is.finite(fs$p.value) & fs$p.value < 0.01, "**", ifelse(is.finite(fs$p.value) & fs$p.value < 0.05, "*", "")))
  out <- data.frame(
    Term = ifelse(fs$term == "wavg_ling_degrees", "Linguistic distance", fs$term),
    Estimate = paste0(sprintf("%.2f", fs$estimate), fs$stars),
    `Std. Error` = sprintf("(%.2f)", fs$std.error),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  stat <- fs[fs$term == "wavg_ling_degrees", , drop = FALSE]
  if (nrow(stat)) out <- rbind(out, data.frame(Term = "First-stage F", Estimate = sprintf("%.2f", stat$partial_f[[1]]), `Std. Error` = "", check.names = FALSE))
  out
}

make_second_stage_table <- function(iv_models) {
  out <- tidy_iv_models(iv_models)
  out <- out[out$model %in% c("consumption", "baseline"), , drop = FALSE]
  out$stars <- ifelse(is.finite(out$p.value) & out$p.value < 0.001, "***", ifelse(is.finite(out$p.value) & out$p.value < 0.01, "**", ifelse(is.finite(out$p.value) & out$p.value < 0.05, "*", "")))
  data.frame(
    Term = legacy_iv_term_label(out$term),
    Estimate = ifelse(is.finite(out$estimate), paste0(sprintf("%.3f", out$estimate), out$stars), NA_character_),
    `Std. Error` = ifelse(is.finite(out$std.error), sprintf("(%.3f)", out$std.error), NA_character_),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

legacy_iv_term_label <- function(term) {
  labels <- c(
    "(Intercept)" = "Constant",
    EMIE = "EMIE",
    emie_2007 = "EMIE",
    wavg_ling_degrees = "Linguistic distance",
    consumption_0708 = "Consumption, 2007-08",
    gini_cons_0708 = "Gini consumption, 2007-08",
    pct_urban = "Pct. urban",
    avg_hh_size = "Avg. HH size",
    dependency_ratio = "Dependency ratio",
    pct_fem_head = "Pct. female head",
    pct_hindu = "Pct. Hindu",
    pct_muslim = "Pct. Muslim",
    pct_st = "Pct. ST",
    pct_sc = "Pct. SC",
    pct_obc = "Pct. OBC",
    pct_small_land = "Pct. small land",
    pct_medium_land = "Pct. medium land",
    pct_large_land = "Pct. large land",
    pct_head_lit_to_primary = "Pct. head literate-primary",
    pct_head_secondary_plus = "Pct. head secondary+"
  )
  out <- unname(labels[term])
  out[is.na(out)] <- term[is.na(out)]
  out
}

make_diagnostic_tables <- function(...) list()
# sample-end: code-table-generation
