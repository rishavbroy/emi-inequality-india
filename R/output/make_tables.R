# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-table-generation

is_final_mode <- function(cfg) identical(cfg$mode, "final")

table_status_failures <- function(x, cfg, label) {
  df <- as.data.frame(x)
  ok_status <- c("mapped", "estimated")
  if (!is_final_mode(cfg) || !"status" %in% names(df)) return(character())
  bad <- !is.na(df$status) & !df$status %in% ok_status
  if (!any(bad)) return(character())
  reasons <- character()
  if ("reason" %in% names(df)) reasons <- unique(stats::na.omit(as.character(df$reason[bad])))
  suffix <- if (length(reasons)) paste0(" Reasons: ", paste(reasons, collapse = "; ")) else ""
  paste0("Final table generation requires completed model output for ", label, ".", suffix)
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

table_status_row <- function(model, status = "unavailable", reason = NA_character_) {
  data.frame(
    model = model,
    term = NA_character_,
    estimate = NA_real_,
    std.error = NA_real_,
    statistic = NA_real_,
    p.value = NA_real_,
    status = status,
    reason = reason,
    stringsAsFactors = FALSE
  )
}

table_first_existing_column <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit)) hit[[1]] else NA_character_
}

table_column_or_na <- function(df, column) {
  if (length(column) != 1L || is.na(column) || !column %in% names(df)) {
    return(rep(NA_real_, nrow(df)))
  }
  suppressWarnings(as.numeric(df[[column]]))
}

coefficient_frame <- function(model, vcov_matrix = NULL) {
  estimates <- tryCatch(stats::coef(model), error = function(e) NULL)
  if (is.null(estimates) || !length(estimates)) return(data.frame())

  terms <- names(estimates)
  if (is.null(terms) || !length(terms)) terms <- paste0("term_", seq_along(estimates))
  estimates <- suppressWarnings(as.numeric(estimates))

  vc <- vcov_matrix
  if (is.null(vc)) vc <- tryCatch(stats::vcov(model), error = function(e) NULL)
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
    `Std. Error` = se,
    statistic = statistic,
    `Pr(>|t|)` = p_value,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(out) <- terms
  out
}

plain_model_coefficients <- function(model) {
  coefficient_frame(model)
}

clustered_model_coefficients <- function(model) {
  tryCatch({
    vc_fun <- clustered_model_vcov(model)
    if (is.null(vc_fun)) return(data.frame())
    vc <- vc_fun(model)
    coefficient_frame(model, vc)
  }, error = function(e) data.frame())
}

filter_table_model <- function(df, candidates) {
  if (!"model" %in% names(df)) return(df)
  out <- df[df$model %in% candidates, , drop = FALSE]
  if (nrow(out)) out else df[0L, , drop = FALSE]
}

is_model_status_payload <- function(x) {
  is.list(x) &&
    !inherits(x, c("lm", "ivreg")) &&
    !is.null(x$status) &&
    length(x$status) == 1L &&
    (is.character(x$status) || is.factor(x$status))
}

model_status_reason <- function(x) {
  reason <- x$reason
  if (is.null(reason) || !length(reason) || all(is.na(reason))) NA_character_ else as.character(reason[[1]])
}

tidy_iv_models <- function(iv_models) {
  if (!is.list(iv_models) || inherits(iv_models, c("lm", "ivreg"))) iv_models <- list(model = iv_models)
  safe_bind_rows(lapply(names(iv_models), function(name) {
    model <- iv_models[[name]]
    if (is_model_status_payload(model)) {
      return(table_status_row(name, as.character(model$status[[1]]), model_status_reason(model)))
    }
    coefs <- clustered_model_coefficients(model)
    if (!nrow(coefs)) coefs <- plain_model_coefficients(model)
    if (!nrow(coefs)) {
      return(table_status_row(name, "unavailable", "Model coefficients are unavailable."))
    }

    estimate_col <- table_first_existing_column(coefs, c("Estimate", "estimate"))
    se_col <- table_first_existing_column(coefs, c("Std. Error", "std.error", "Std.Error"))
    statistic_col <- table_first_existing_column(coefs, c("t value", "z value", "t", "z", "statistic"))
    p_col <- table_first_existing_column(coefs, c("Pr(>|t|)", "Pr(>|z|)", "p.value", "p", "P>|t|", "P>|z|"))

    if (is.na(estimate_col)) {
      return(table_status_row(name, "unavailable", paste("Model coefficient table lacks an estimate column. Columns:", paste(names(coefs), collapse = ", "))))
    }

    data.frame(
      model = name,
      term = rownames(coefs),
      estimate = table_column_or_na(coefs, estimate_col),
      std.error = table_column_or_na(coefs, se_col),
      statistic = table_column_or_na(coefs, statistic_col),
      p.value = table_column_or_na(coefs, p_col),
      status = "estimated",
      reason = NA_character_,
      stringsAsFactors = FALSE
    )
  }))
}

clustered_model_vcov <- function(model) {
  cluster <- attr(model, "cluster_state")
  if (is.null(cluster)) return(NULL)
  cluster <- as.vector(cluster)
  if (length(cluster) != stats::nobs(model)) return(NULL)
  if (sum(!is.na(cluster)) < 2L || length(unique(cluster[!is.na(cluster)])) <= 1L) return(NULL)
  if (!requireNamespace("sandwich", quietly = TRUE)) return(NULL)
  force(cluster)
  function(x) sandwich::vcovCL(x, cluster = cluster)
}

#' make tables
#'
#' @return A named list of data frames consumed by save_tables().
make_tables <- function(selection_data, ame_results, district_panel, iv_models, first_stage_tests, cfg) {
  cons_iv <- tidy_iv_models(iv_models)
  cons_iv_required <- filter_table_model(cons_iv, c("consumption", "baseline"))
  table_failures <- c(
    table_status_failures(ame_results, cfg, "average marginal effects"),
    table_status_failures(first_stage_tests, cfg, "first-stage regression"),
    table_status_failures(cons_iv_required, cfg, "second-stage IV regression")
  )

  out <- list(
    selection_n = data.frame(n = nrow(as.data.frame(selection_data))),
    sum_tbl_probit_quant = make_selection_summary_numeric_table(selection_data),
    sum_tbl_probit_cat = make_selection_summary_categorical_table(selection_data),
    probit_mfx = make_probit_ame_table(ame_results),
    sum_tbl_iv = make_iv_summary_table(district_panel),
    fs_cons = make_first_stage_table(first_stage_tests, cfg),
    cons_iv = make_second_stage_table(iv_models),
    ame_results = as.data.frame(ame_results),
    first_stage = as.data.frame(first_stage_tests)
  )
  table_failures <- c(table_failures, attr(out$fs_cons, "legacy_table_input_failures"))
  table_failures <- unique(stats::na.omit(table_failures))
  if (length(table_failures)) attr(out, "legacy_table_input_failures") <- table_failures
  out
}

make_selection_summary_numeric_table <- function(selection_data) {
  meta <- data.frame(
    var = c("AGE", "HH_SIZE", "ENROLLMENT_COST", "dmean_num_IS_EDU_FREE", "dmean_num_TUTION_FEE_WAIVED", "dmean_num_RECD_SCHOLARSHIP_STIPEND", "dmean_num_RECD_TXT_BOOKS", "dmean_num_RECD_STATIONERY", "dmean_num_MID_DAY_MEAL_ETC_RECD", "dmean_num_ENROLLMENT_COST"),
    label = c("Age", "Household size", "Enrollment cost (Rs.)", "Educ. free available? (Yes = 1)", "Tuition waived?", "Scholarship/Stipend?", "Textbooks received?", "Stationery received?", "Mid-day meal or more received?", "Avg. district enrollment cost"),
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

make_first_stage_table <- function(first_stage_tests, cfg = list()) {
  fs <- as.data.frame(first_stage_tests, stringsAsFactors = FALSE)
  required_cols <- c("model", "term", "estimate", "std.error", "p.value", "partial_f", "partial_p", "status")
  missing_cols <- setdiff(required_cols, names(fs))
  if (length(missing_cols)) {
    msg <- paste("First-stage results are missing columns:", paste(missing_cols, collapse = ", "))
    if (is_final_mode(cfg)) stop(msg, call. = FALSE)
    return(table_status_row("first_stage", "unavailable", msg))
  }
  if ("model" %in% names(fs) && any(fs$model %in% c("consumption", "baseline"))) fs <- fs[fs$model %in% c("consumption", "baseline"), , drop = FALSE]
  if (any(grepl("^[0-9]+$", as.character(fs$term)))) {
    msg <- "First-stage coefficient terms are numeric row positions; coefficient names were lost upstream."
    if (is_final_mode(cfg)) stop(msg, call. = FALSE)
    return(table_status_row("first_stage", "malformed", msg))
  }
  status_reasons <- unique(stats::na.omit(as.character(fs$reason[!is.na(fs$status) & fs$status != "estimated"])))
  fs <- fs[fs$status == "estimated" & fs$term %in% c("wavg_ling_degrees", "(Intercept)"), , drop = FALSE]
  required_terms <- c("wavg_ling_degrees", "(Intercept)")
  missing_terms <- setdiff(required_terms, fs$term)
  if (length(missing_terms)) {
    if (length(status_reasons)) {
      msg <- paste(status_reasons, collapse = "; ")
      out <- table_status_row("first_stage", "out_of_active_pipeline", msg)
      attr(out, "legacy_table_input_failures") <- paste0("First-stage table lacks required coefficient(s): ", paste(missing_terms, collapse = ", "), ". Reasons: ", msg)
      return(out)
    }
    msg <- paste("First-stage table lacks required coefficient(s):", paste(missing_terms, collapse = ", "))
    if (is_final_mode(cfg)) stop(msg, call. = FALSE)
    return(table_status_row("first_stage", "unavailable", msg))
  }
  fs <- fs[match(required_terms, fs$term), , drop = FALSE]
  fs$stars <- ifelse(is.finite(fs$p.value) & fs$p.value < 0.001, "***", ifelse(is.finite(fs$p.value) & fs$p.value < 0.01, "**", ifelse(is.finite(fs$p.value) & fs$p.value < 0.05, "*", "")))
  out <- data.frame(
    Term = ifelse(fs$term == "wavg_ling_degrees", "Linguistic distance", "Constant"),
    Estimate = paste0(sprintf("%.3f", fs$estimate), fs$stars),
    `Std. Error` = sprintf("(%.3f)", fs$std.error),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  stat <- fs[fs$term == "wavg_ling_degrees", , drop = FALSE]
  if (nrow(stat)) {
    f_value <- first_finite_value(stat, c("legacy_model_f", "partial_f"))
    f_p <- first_finite_value(stat, c("legacy_model_p", "partial_p"))
    out <- rbind(out, data.frame(
      Term = "Instrument's F-Statistic",
      Estimate = paste0(sprintf("%.2f", f_value), ifelse(is.finite(f_p) & f_p < 0.001, "***", ifelse(is.finite(f_p) & f_p < 0.01, "**", ifelse(is.finite(f_p) & f_p < 0.05, "*", "")))),
      `Std. Error` = "",
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  out
}

first_finite_value <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) next
    value <- suppressWarnings(as.numeric(df[[col]][[1]]))
    if (length(value) && is.finite(value)) return(value)
  }
  NA_real_
}

make_second_stage_table <- function(iv_models) {
  out <- tidy_iv_models(iv_models)
  out <- filter_table_model(out, c("consumption", "baseline"))
  if (!nrow(out)) return(data.frame(Term = character(), Estimate = character(), `Std. Error` = character(), check.names = FALSE))
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
