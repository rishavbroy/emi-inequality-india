# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

table_output_dir <- function(cfg) {
  "outputs/tables/main"
}

table_formats <- function(cfg) {
  out <- cfg$output_formats$tables %||% "csv"
  # yaml::read_yaml() represents sequence values as lists, not necessarily as
  # atomic character vectors.  Calling as.character() directly on that list
  # emits "argument is not an atomic vector; coercing", which targets records as
  # a warning on the file target even when all table files are written correctly.
  out <- unlist(out, recursive = TRUE, use.names = FALSE)
  out <- as.character(out)
  unique(out[nzchar(out)])
}

table_label <- function(name) {
  gsub("_", "-", name, fixed = TRUE)
}

quarto_table_label <- function(name) {
  paste0("tbl-", table_label(name))
}

regression_star_note <- function() "* p < 0.05, ** p < 0.01, *** p < 0.001"

legacy_table_caption_text <- function(name) {
  captions <- c(
    selection_n = "Enrollment Participation Model Sample Size",
    sum_tbl_probit_quant = "Summary Statistics for Enrollment Participation Model (Numeric Variables)",
    sum_tbl_probit_cat = "Summary Statistics for Enrollment Participation Model (Categorical Variables)",
    probit_mfx = "Average Marginal Effects and Counterfactual Comparisons for Enrollment Probit",
    sum_tbl_iv = "Summary Statistics for 2SLS Model",
    fs_cons = "First-Stage Regression: EMI Exposure on Linguistic Distance",
    cons_iv = "Second-Stage Regression: Consumption Growth on EMIE (Fitted)",
    ame_results = "Average Marginal Effects Results",
    first_stage = "First-Stage Diagnostic Results"
  )
  captions[[name]] %||% name
}

regression_caption <- function(cap) {
  cap
}

table_caption <- function(name) {
  cap <- legacy_table_caption_text(name)
  cap
}

nice_column_name <- function(x) {
  x <- gsub("\\.", " ", x)
  x <- gsub("_", " ", x)
  x <- gsub("p value", "p-value", x, ignore.case = TRUE)
  x <- gsub("std error", "Std. Error", x, ignore.case = TRUE)
  tools::toTitleCase(x)
}

legacy_table_note <- function(name) {
  switch(name,
    sum_tbl_probit_quant = "Min. = minimum; 1Q = first quartile; Med. = median; 3Q = third quartile; Max. = maximum; Mean = arithmetic mean; SD = standard deviation; N = number of observations.",
    sum_tbl_iv = "Min. = minimum; 1Q = first quartile; Med. = median; 3Q = third quartile; Max. = maximum; Mean = arithmetic mean; SD = standard deviation; N = number of observations.",
    sum_tbl_probit_cat = "Values = all possible values; Mode = most frequent value; Pct. Mode = percent of observations taking the modal value; Least Freq. = least frequent value; Pct. Least Freq. = percent of observations taking the least frequent value; N = number of observations.",
    probit_mfx = "Data from the 64th round of the NSS, \"Participation and Expenditure in Education\" in 2007-08. All standard errors are design-based (clustered and nested within strata).",
    fs_cons = "Standard errors clustered by state in parentheses.",
    cons_iv = "Standard errors clustered by state in parentheses.",
    NULL
  )
}

wrap_table_cell <- function(x, width = 28L) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

wrap_table_text_columns <- function(df, name = NULL) {
  # Column widths below give LaTeX's tabular engine the wrapping constraints;
  # do not inject literal `\\` breaks into cell text.
  as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
}


table_cell_to_string <- function(value) {
  if (length(value) == 0L) return("")
  if (is.data.frame(value)) value <- unlist(value, recursive = TRUE, use.names = FALSE)
  if (is.list(value)) value <- unlist(value, recursive = TRUE, use.names = FALSE)
  if (length(value) == 0L || all(is.na(value))) return("")
  paste(as.character(value), collapse = "; ")
}

table_column_to_strings <- function(col) {
  if (is.factor(col)) col <- as.character(col)
  if (is.list(col)) {
    out <- vapply(col, table_cell_to_string, character(1))
  } else {
    out <- as.character(col)
  }
  out[is.na(out)] <- ""
  out
}

table_column_or_default <- function(df, column, default) {
  if (!column %in% names(df)) return(rep(default, nrow(df)))
  out <- table_column_to_strings(df[[column]])
  out[!nzchar(out)] <- default
  out
}

is_blank_table_column <- function(col) {
  values <- table_column_to_strings(col)
  all(!nzchar(values))
}

drop_empty_output_columns <- function(out) {
  if (!nrow(out)) return(out)
  keep <- vapply(out, function(col) !is_blank_table_column(col), logical(1))
  out[, keep, drop = FALSE]
}

format_public_summary_columns <- function(out) {
  if (all(c("var", "label") %in% names(out))) {
    out$Variable <- out$label
    group <- startsWith(table_column_to_strings(out$var), ".group_")
    out$var <- NULL
    out$label <- NULL
    if ("desc" %in% names(out)) names(out)[names(out) == "desc"] <- "Description"
    out <- out[, c("Variable", setdiff(names(out), "Variable")), drop = FALSE]
    if (any(group)) {
      for (nm in setdiff(names(out), "Variable")) out[[nm]][group] <- ""
    }
  } else if ("desc" %in% names(out)) {
    names(out)[names(out) == "desc"] <- "Description"
  }
  rename <- c("% Mode" = "Pct. Mode", "% Least Freq." = "Pct. Least Freq.")
  for (old in names(rename)) if (old %in% names(out)) names(out)[names(out) == old] <- rename[[old]]
  preferred <- c("Variable", "Description", "Values", "Mode", "Pct. Mode", "Least Freq.", "Pct. Least Freq.", "Min", "1Q", "Med", "3Q", "Max", "Mean", "SD", "N")
  ordered <- c(intersect(preferred, names(out)), setdiff(names(out), preferred))
  out[, ordered, drop = FALSE]
}


is_status_only_table <- function(out) {
  if (!nrow(out) || !"status" %in% names(out)) return(FALSE)
  substantive <- setdiff(names(out), c("status", "reason", "method", "model"))
  if (!length(substantive)) return(TRUE)
  all(vapply(out[substantive], function(col) all(!nzchar(table_column_to_strings(col))), logical(1)))
}

format_status_table_for_output <- function(out, public = TRUE) {
  status <- table_column_or_default(out, "status", "unavailable")
  reason <- table_column_or_default(out, "reason", "No completed model output is available.")
  model <- table_column_or_default(out, "model", "output")

  if (public) {
    data.frame(
      Term = model,
      Estimate = status,
      `Std. Error` = reason,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      model = model,
      status = status,
      reason = reason,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
}

format_table_for_output <- function(table, public = TRUE) {
  out <- as.data.frame(table, check.names = FALSE)
  if (!nrow(out)) return(data.frame(Note = "No rows to display.", stringsAsFactors = FALSE))

  if (is_status_only_table(out)) {
    return(format_status_table_for_output(out, public = public))
  }

  if (!public) {
    # Internal/diagnostic CSVs are audited by exact schema. Preserve machine-
    # readable names such as std.error, p.value, conf.low, and conf.high.
    formatted <- drop_empty_output_columns(out)
    if (!length(names(formatted))) formatted <- format_status_table_for_output(out, public = FALSE)
    return(formatted)
  }

  # Public paper tables should not expose pipeline status scaffolding when
  # substantive rows exist. Pure status tables are handled above so incomplete
  # final model outputs can still be rendered and audited instead of crashing in
  # the table writer.
  out$status <- NULL
  out$reason <- NULL
  if ("method" %in% names(out) && length(unique(stats::na.omit(out$method))) <= 1L) out$method <- NULL

  out <- format_public_summary_columns(out)
  out <- drop_empty_output_columns(out)
  if (!length(names(out))) return(data.frame(Note = "No displayable columns.", stringsAsFactors = FALSE))
  already_polished <- any(names(out) %in% c("Term", "Estimate", "Std. Error", "N", "Min", "1Q", "Med", "3Q", "Max", "Mean", "SD", "Variable", "Description", "Consumption Growth", "EMI Exposure", "Enrolled (1 = yes)"))
  if (!already_polished) names(out) <- vapply(names(out), nice_column_name, character(1))
  out
}


summary_table_groups <- function(df) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(df) || !length(names(df))) return(list(data = df, groups = data.frame()))
  empty_rest <- if (ncol(df) > 1L) {
    apply(df[-1], 1, function(x) all(!nzchar(table_column_to_strings(x))))
  } else {
    rep(TRUE, nrow(df))
  }
  first_col <- table_column_to_strings(df[[1]])
  group_row <- grepl(":$", first_col) & empty_rest
  group_idx <- which(group_row)
  if (!length(group_idx)) return(list(data = df, groups = data.frame()))

  groups <- lapply(seq_along(group_idx), function(i) {
    start_orig <- group_idx[[i]] + 1L
    end_orig <- if (i < length(group_idx)) group_idx[[i + 1L]] - 1L else nrow(df)
    start <- start_orig - sum(group_idx < start_orig)
    end <- end_orig - sum(group_idx <= end_orig)
    if (start > end) return(NULL)
    data.frame(
      label = first_col[[group_idx[[i]]]],
      start = start,
      end = end,
      stringsAsFactors = FALSE
    )
  })
  groups <- do.call(rbind, Filter(Negate(is.null), groups))
  if (is.null(groups)) groups <- data.frame()
  list(data = df[!group_row, , drop = FALSE], groups = groups)
}

render_table_math_labels <- function(df) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    if (!is.character(df[[nm]])) next
    df[[nm]] <- gsub("$\\%\\Delta\\text{Consumption}$", "Percent change in consumption", df[[nm]], fixed = TRUE)
    df[[nm]] <- gsub("$%\\Delta\\text{Consumption}$", "Percent change in consumption", df[[nm]], fixed = TRUE)
    df[[nm]] <- gsub("$\\Delta\\text{Gini}^{\\text{Consumption}}$", "Change in Gini of consumption", df[[nm]], fixed = TRUE)
  }
  df
}

table_header_labels <- function(df, name) {
  labels <- names(df)
  wrap <- c(
    "Pct. Mode" = "Pct.\nMode",
    "Least Freq." = "Least\nFreq.",
    "Pct. Least Freq." = "Pct. Least\nFreq.",
    "Adjusted R-squared" = "Adjusted\nR-squared"
  )
  labels <- ifelse(labels %in% names(wrap), unname(wrap[labels]), labels)
  if (name == "sum_tbl_iv") {
    labels <- ifelse(labels == "Description", "Description", labels)
  }
  vapply(labels, function(x) {
    if (grepl("\n", x, fixed = TRUE)) kableExtra::linebreak(x, align = "c") else x
  }, character(1))
}

table_alignments <- function(df, name) {
  if (name %in% c("probit_mfx", "fs_cons", "cons_iv")) return(c("l", rep("c", max(0, ncol(df) - 1L))))
  if (ncol(df) <= 1L) return("l")
  c("l", rep("c", ncol(df) - 1L))
}

caption_for_latex <- function(name) {
  # Do not run full captions through kableExtra::linebreak().  That helper is
  # intended for table cells/headers and can corrupt kable captions by
  # repeatedly injecting caption fragments separated by alignment markers.
  # Caption wrapping is handled globally by the LaTeX caption package.
  table_caption(name)
}

latex_escape_text <- function(x) {
  # modelsummary/tinytable handle LaTeX escaping.  The helper exists only to
  # standardize list/factor columns before handing them to that renderer.
  table_column_to_strings(x)
}

stack_estimate_se_rows <- function(df, estimate_col = "Estimate", se_col = "Std. Error") {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  if (!all(c("Term", estimate_col, se_col) %in% names(df))) return(df)
  terms <- table_column_to_strings(df$Term)
  estimates <- table_column_to_strings(df[[estimate_col]])
  ses <- table_column_to_strings(df[[se_col]])

  out <- data.frame(
    Term = rep(terms, each = 2L),
    Estimate = as.vector(rbind(estimates, ses)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  out$Term[seq.int(2L, nrow(out), by = 2L)] <- ""
  out <- out[nzchar(out$Estimate), , drop = FALSE]
  rownames(out) <- NULL
  out
}


regression_rows_for_modelsummary <- function(df) {
  df <- sanitize_table_for_kable(df)
  if (ncol(df) < 2L) return(df)
  model_col <- names(df)[[2]]
  out <- data.frame(
    Term = latex_escape_text(df[[1]]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out[[model_col]] <- latex_escape_text(df[[2]])
  out$Term[!nzchar(out$Term)] <- " "
  out
}


legacy_datasummary_table_tex <- function(df, name) {
  body <- suppress_modelsummary_latex_preamble_warning(
    modelsummary::datasummary_df(
      df,
      output = "latex_tabular",
      fmt = identity,
      align = table_alignments(df, name)
    )
  )
  body <- paste(as.character(body), collapse = "\n")
  note <- legacy_table_note(name)
  note_tex <- if (!is.null(note)) {
    paste0("\n\\begin{tablenotes}[flushleft]\n\\footnotesize\n\\item ", note, "\n\\end{tablenotes}")
  } else {
    ""
  }
  paste0(
    "\\begin{table}[!h]\n",
    "\\centering\n",
    "\\caption{\\label{", quarto_table_label(name), "}", table_caption(name), "}\n",
    "\\begin{threeparttable}\n",
    body,
    note_tex,
    "\n\\end{threeparttable}\n",
    "\\end{table}"
  )
}

legacy_ame_modelsummary_object <- function(table) {
  native <- attr(table, "legacy_marginaleffects", exact = TRUE)
  if (is.null(native)) return(NULL)
  if (!is.data.frame(native)) return(NULL)
  if (!"term" %in% names(native)) return(NULL)

  out <- native
  labels <- table_column_to_strings(table$Term)
  labels <- labels[nzchar(labels)]
  if (length(labels) == nrow(as.data.frame(out))) {
    out$term <- labels
  } else {
    labeled <- tryCatch(attach_legacy_ame_labels(as.data.frame(out)), error = function(e) NULL)
    if (!is.null(labeled) && "Term" %in% names(labeled) && nrow(labeled) == nrow(as.data.frame(out))) {
      out$term <- table_column_to_strings(labeled$Term)
    }
  }
  out
}

legacy_ame_add_rows <- function(table) {
  n <- attr(table, "legacy_marginaleffects_n", exact = TRUE)
  n <- suppressWarnings(as.numeric(n))
  if (!length(n) || !is.finite(n[[1]])) return(NULL)
  data.frame(
    term = "Observations",
    `Enrolled (1 = yes)` = sprintf("%.0f", n[[1]]),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

legacy_ame_modelsummary_table <- function(table, name) {
  need_pkg("modelsummary", "native marginaleffects AME table rendering")
  mfx <- legacy_ame_modelsummary_object(table)
  if (is.null(mfx)) return(NULL)
  old_knit_to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  old_opt <- getOption("modelsummary_format_numeric_latex")
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = old_knit_to), add = TRUE)
  on.exit(options(modelsummary_format_numeric_latex = old_opt), add = TRUE)
  knitr::opts_knit$set(rmarkdown.pandoc.to = "latex")
  options(modelsummary_format_numeric_latex = "plain")

  args <- list(
    models = list(`Enrolled (1 = yes)` = mfx),
    shape = stats::as.formula("term ~ model"),
    gof_omit = ".*",
    add_rows = legacy_ame_add_rows(table),
    stars = c("*" = .05, "**" = .01, "***" = .001),
    fmt = 3,
    title = table_caption(name),
    output = "kableExtra",
    escape = FALSE,
    notes = list(legacy_table_note(name))
  )
  if (is.null(args$add_rows)) args$add_rows <- NULL
  tex <- suppress_modelsummary_latex_preamble_warning(do.call(modelsummary::modelsummary, args))
  tex <- kableExtra::kable_styling(
    tex,
    latex_options = c("hold_position", "repeat_header", "striped", "longtable"),
    position = "center",
    full_width = FALSE
  )
  tex
}

modelsummary_regression_table <- function(df, name) {
  need_pkg("modelsummary", "standard regression table rendering")
  df <- regression_rows_for_modelsummary(df)
  if (ncol(df) < 2L) return(NULL)
  model_col <- switch(name,
    probit_mfx = "Enrolled (1 = yes)",
    fs_cons = "EMI Exposure",
    cons_iv = "Consumption Growth",
    names(df)[[2]]
  )
  names(df) <- c("Term", model_col)
  old_opt <- getOption("modelsummary_format_numeric_latex")
  on.exit(options(modelsummary_format_numeric_latex = old_opt), add = TRUE)
  options(modelsummary_format_numeric_latex = "plain")
  legacy_datasummary_table_tex(df, name)
}

legacy_regression_coef_map <- function() {
  c(
    "EMIE" = "EMI exposure (fitted)",
    "emie_2007" = "EMI exposure (fitted)",
    "wavg_ling_degrees" = "Linguistic distance",
    "consumption_0708" = "Consumption (2007-08)",
    "gini_cons_0708" = "Gini of Consumption (2007-08)",
    "pct_urban" = "Pct. Urban (ref: Rural)",
    "avg_hh_size" = "Average HH size",
    "dependency_ratio" = "Dependency ratio x 100",
    "pct_fem_head" = "Pct. Female head",
    "pct_hindu" = "Pct. Hindu  (ref: Other)",
    "pct_muslim" = "Pct. Muslim",
    "pct_st" = "Pct. Scheduled Tribe (ref: Other)",
    "pct_sc" = "Pct. Scheduled Caste",
    "pct_obc" = "Pct. OBC",
    "pct_small_land" = "Pct. Small Land-owner (ref: No Land)",
    "pct_medium_land" = "Pct. Medium Land-owner",
    "pct_large_land" = "Pct. Large Land-owner",
    "pct_head_lit_to_primary" = "Pct. Head Educ., Lit.-Primary (ref: Illiterate)",
    "pct_head_secondary_plus" = "Pct. Head Educ., Secondary+",
    "(Intercept)" = "Intercept"
  )
}

legacy_modelsummary_gof_map <- function(name) {
  if (identical(name, "fs_cons")) {
    return(list(
      list(raw = "nobs", clean = "Observations", fmt = 0),
      list(raw = "r.squared", clean = "$R^2$", fmt = 3),
      list(raw = "adj.r.squared", clean = "Adjusted $R^2$", fmt = 3),
      list(raw = "sigma", clean = "Residual Std. Error", fmt = 3),
      list(raw = "statistic", clean = "Model's F-Statistic", fmt = 2)
    ))
  }
  list(
    list(raw = "nobs", clean = "Observations", fmt = 0),
    list(raw = "r.squared", clean = "$R^2$", fmt = 3),
    list(raw = "adj.r.squared", clean = "Adjusted $R^2$", fmt = 3),
    list(raw = "sigma", clean = "Residual Std. Error", fmt = 3),
    list(raw = "waldtest", clean = "F-Statistic", fmt = 2)
  )
}

legacy_modelsummary_table <- function(model, name, vcov_matrix = NULL, add_rows = NULL) {
  need_pkg("modelsummary", "legacy regression table rendering")
  old_knit_to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  old_opt <- getOption("modelsummary_format_numeric_latex")
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = old_knit_to), add = TRUE)
  on.exit(options(modelsummary_format_numeric_latex = old_opt), add = TRUE)
  knitr::opts_knit$set(rmarkdown.pandoc.to = "latex")
  options(modelsummary_format_numeric_latex = "plain")
  args <- list(
    models = model,
    coef_map = legacy_regression_coef_map(),
    gof_map = legacy_modelsummary_gof_map(name),
    stars = c("*" = .05, "**" = .01, "***" = .001),
    fmt = 3,
    title = table_caption(name),
    output = "kableExtra",
    escape = FALSE,
    notes = list(legacy_table_note(name))
  )
  if (!is.null(vcov_matrix)) args$vcov <- vcov_matrix
  if (!is.null(add_rows)) args$add_rows <- add_rows
  tex <- suppress_modelsummary_latex_preamble_warning(do.call(modelsummary::modelsummary, args))
  tex <- kableExtra::kable_styling(
    tex,
    latex_options = c("hold_position", "repeat_header", "striped", "longtable"),
    position = "center",
    full_width = FALSE
  )
  header <- switch(name,
    fs_cons = c(" " = 1, "EMI Exposure" = 1),
    cons_iv = c(" " = 1, "Consumption Growth" = 1)
  )
  kableExtra::add_header_above(tex, header)
}

regression_standard_error_rows <- function(df) {
  if (!"Term" %in% names(df) || ncol(df) < 2L) return(integer())
  terms <- table_column_to_strings(df$Term)
  vals <- table_column_to_strings(df[[2]])
  which((is.na(terms) | !nzchar(terms)) & grepl("^\\(", vals))
}

sanitize_table_for_kable <- function(df) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(df)) df <- data.frame(Note = "No rows to display.", stringsAsFactors = FALSE)
  if (!length(names(df))) df <- data.frame(Note = rep("No displayable columns.", nrow(df)), stringsAsFactors = FALSE)
  for (nm in names(df)) {
    df[[nm]] <- table_column_to_strings(df[[nm]])
  }
  render_table_math_labels(df)
}

regression_summary_start <- function(df) {
  if (!"Term" %in% names(df)) return(NA_integer_)
  terms <- table_column_to_strings(df$Term)
  hit <- which(terms %in% c("Observations", "R-squared", "Adjusted R-squared", "Instrument's F-Statistic", "Model's F-Statistic", "F-Statistic"))
  if (length(hit)) hit[[1]] else NA_integer_
}

style_regression_table <- function(tex, df, name) {
  if (!name %in% c("probit_mfx", "fs_cons", "cons_iv")) return(tex)
  if (nrow(df)) {
    tex <- kableExtra::row_spec(tex, seq_len(nrow(df)), background = "white")
  }
  # Keep standard-error rows in the same roman face as modelsummary's native
  # regression output. Italic grey SE rows made the AME table visually diverge
  # from the legacy modelsummary tables.
  start <- regression_summary_start(df)
  if (is.finite(start) && start > 1L) {
    tex <- kableExtra::row_spec(tex, start - 1L, hline_after = TRUE)
  }
  if (is.finite(start) && start <= nrow(df)) {
    tex <- kableExtra::row_spec(tex, start:nrow(df), background = "white")
  }
  tex
}

suppress_atomic_vector_coercion_warning <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (identical(conditionMessage(w), "argument is not an atomic vector; coercing")) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

save_table_csv <- function(table, path, public = TRUE) {
  utils::write.csv(sanitize_table_for_kable(format_table_for_output(table, public = public)), path, row.names = FALSE)
  path
}

is_formatted_status_table <- function(df) {
  all(c("Term", "Estimate", "Std. Error") %in% names(df)) &&
    any(table_column_to_strings(df$Estimate) %in% c("out_of_active_pipeline", "unavailable", "not_run", "failed"))
}

suppress_modelsummary_latex_preamble_warning <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("To compile a LaTeX document with this table", msg, fixed = TRUE) ||
          grepl("latex_siunitx_preamble", msg, fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

normalize_quarto_table_labels <- function(tex, name) {
  tex <- as.character(tex)
  label <- table_label(name)
  tex <- gsub(
    paste0("\\\\label\\{tab:", label, "\\}"),
    paste0("\\\\label{", quarto_table_label(name), "}"),
    tex
  )
  if (!any(grepl("\\\\label\\{", tex))) {
    tex <- sub(
      "\\\\caption\\{",
      paste0("\\\\caption{\\\\label{", quarto_table_label(name), "}"),
      tex
    )
  }
  tex
}

write_table_tex <- function(tex, path, name) {
  writeLines(normalize_quarto_table_labels(tex, name), path)
  path
}

save_table_tex <- function(table, path, name, public = TRUE) {
  need_pkg("kableExtra", "LaTeX table output")
  legacy_model <- attr(table, "legacy_model")
  if (name %in% c("fs_cons", "cons_iv") && !is.null(legacy_model) && !is_formatted_status_table(as.data.frame(table, check.names = FALSE))) {
    tex <- legacy_modelsummary_table(
      legacy_model,
      name,
      vcov_matrix = attr(table, "legacy_vcov"),
      add_rows = attr(table, "legacy_add_rows")
    )
    return(write_table_tex(tex, path, name))
  }
  if (identical(name, "probit_mfx") && !is_formatted_status_table(as.data.frame(table, check.names = FALSE))) {
    tex <- legacy_ame_modelsummary_table(table, name)
    if (!is.null(tex)) return(write_table_tex(tex, path, name))
  }
  df <- sanitize_table_for_kable(format_table_for_output(table, public = public))
  grouped <- summary_table_groups(df)
  df_render <- wrap_table_text_columns(grouped$data, name)
  wide_summary_table <- name %in% c("sum_tbl_iv", "sum_tbl_probit_quant", "sum_tbl_probit_cat")
  regression_table <- name %in% c("probit_mfx", "fs_cons", "cons_iv") && !is_formatted_status_table(df_render)
  if (!regression_table) {
    names(df_render) <- table_header_labels(df_render, name)
  }
  if (identical(name, "probit_mfx") && !is_formatted_status_table(df_render)) {
    df_render <- stack_estimate_se_rows(df_render)
    tex <- modelsummary_regression_table(df_render, name)
    return(write_table_tex(tex, path, name))
  }
  tex <- kableExtra::kbl(
    df_render,
    format = "latex",
    booktabs = TRUE,
    longtable = wide_summary_table || regression_table,
    label = table_label(name),
    caption = caption_for_latex(name),
    escape = FALSE,
    linesep = "",
    digits = 3,
    align = table_alignments(df_render, name),
    row.names = FALSE
  )
  if (regression_table) {
    latex_options <- c("hold_position", "repeat_header", "striped")
  } else if (wide_summary_table) {
    # Wide summary tables must be true longtables inside pdflscape. A floating
    # table can escape the landscape environment when emitted as raw TeX from
    # Quarto, leaving a clipped portrait page. Longtable keeps the table content
    # inside the environment so pdflscape can rotate the page metadata.
    latex_options <- c("repeat_header", "striped")
  } else {
    latex_options <- c("striped", "repeat_header")
  }
  tex <- kableExtra::kable_styling(
    tex,
    latex_options = latex_options,
    full_width = FALSE,
    position = "center",
    font_size = if (wide_summary_table || regression_table) 9 else NULL
  )
  if (nrow(grouped$groups)) {
    for (i in rev(seq_len(nrow(grouped$groups)))) {
      tex <- kableExtra::pack_rows(
        tex,
        grouped$groups$label[[i]],
        grouped$groups$start[[i]],
        grouped$groups$end[[i]],
        bold = TRUE,
        italic = FALSE,
        background = "white",
        escape = FALSE
      )
    }
  }
  if (name == "sum_tbl_probit_cat") {
    tex <- tex |>
      kableExtra::column_spec(1, width = "3.6cm") |>
      kableExtra::column_spec(2, width = "6.1cm") |>
      kableExtra::column_spec(3, width = "3.1cm") |>
      kableExtra::column_spec(4, width = "1.55cm") |>
      kableExtra::column_spec(5, width = "3.25cm") |>
      kableExtra::column_spec(6, width = "1.65cm") |>
      kableExtra::column_spec(7, width = "1.35cm")
  }
  if (name == "sum_tbl_iv") {
    tex <- tex |>
      kableExtra::column_spec(1, width = "3.5cm") |>
      kableExtra::column_spec(2, width = "5.0cm") |>
      kableExtra::column_spec(3:ncol(df_render), width = "1.55cm")
  }
  if (name == "sum_tbl_probit_quant") {
    tex <- tex |>
      kableExtra::column_spec(1, width = "5.4cm") |>
      kableExtra::column_spec(2:ncol(df_render), width = "2.0cm")
  }
  if (regression_table) {
    header <- switch(name,
      probit_mfx = c(" " = 1, "Enrolled (1 = yes)" = 1),
      fs_cons = c(" " = 1, "EMI Exposure" = 1),
      cons_iv = c(" " = 1, "Consumption Growth" = 1)
    )
    tex <- kableExtra::add_header_above(tex, header, escape = FALSE)
    tex <- tex |>
      kableExtra::column_spec(1, width = "5.8cm") |>
      kableExtra::column_spec(2, width = "2.6cm")
    tex <- style_regression_table(tex, df_render, name)
  }
  note <- legacy_table_note(name)
  if (!is.null(note)) {
    tex <- kableExtra::footnote(tex, general = note, threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE)
  }
  if (wide_summary_table) {
    tex <- kableExtra::landscape(tex)
    tex <- paste0("\\clearpage\n", as.character(tex), "\n\\clearpage")
  }
  write_table_tex(tex, path, name)
}

#' save tables
#'
#' @return A character vector of generated table paths.
save_tables <- function(tables, cfg) {
  dir <- table_output_dir(cfg)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  formats <- table_formats(cfg)
  public_table_names <- setdiff(names(tables), c("ame_results", "first_stage", "selection_n"))
  paths <- character()

  append_path <- function(value) {
    value <- unlist(value, recursive = TRUE, use.names = FALSE)
    if (!length(value)) return(invisible(NULL))
    value <- as.character(value)
    value <- value[nzchar(value)]
    if (length(value)) paths <<- c(paths, value)
    invisible(NULL)
  }

  for (n in names(tables)) {
    public <- n %in% public_table_names
    if ("csv" %in% formats) {
      append_path(suppress_atomic_vector_coercion_warning(
        save_table_csv(tables[[n]], file.path(dir, paste0(n, ".csv")), public = public)
      ))
    }
    if ("tex" %in% formats) {
      append_path(suppress_atomic_vector_coercion_warning(
        save_table_tex(tables[[n]], file.path(dir, paste0(n, ".tex")), n, public = public)
      ))
    }
  }

  unname(unique(paths))
}

#' save table csv tex
#'
#' @return Generated table paths.
save_table_csv_tex <- function(table, path_base) {
  c(
    save_table_csv(table, paste0(path_base, ".csv")),
    save_table_tex(table, paste0(path_base, ".tex"), basename(path_base))
  )
}

#' save table html if requested
#'
#' @return Generated HTML path.
save_table_html_if_requested <- function(table, path_base) {
  html <- paste0(path_base, ".html")
  html
}
