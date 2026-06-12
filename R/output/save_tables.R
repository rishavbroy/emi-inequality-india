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

regression_star_note <- function() "* p < 0.05, ** p < 0.01, *** p < 0.001"

legacy_table_caption_text <- function(name) {
  captions <- c(
    selection_n = "Enrollment Participation Model Sample Size",
    sum_tbl_probit_quant = "Summary Statistics for Enrollment Participation Model\n(Numeric Variables)",
    sum_tbl_probit_cat = "Summary Statistics for Enrollment Participation Model\n(Categorical Variables)",
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
  # Keep captions as plain text. Raw LaTeX line-break helpers inside kable
  # captions are fragile in Quarto excerpt renders and can be escaped into
  # invalid TeX. A literal newline keeps the source caption two-line and lets
  # LaTeX/Pandoc handle wrapping without injecting commands into captions.
  paste(regression_star_note(), cap, sep = "\\n")
}

table_caption <- function(name) {
  cap <- legacy_table_caption_text(name)
  if (name %in% c("probit_mfx", "fs_cons", "cons_iv")) return(regression_caption(cap))
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
  cap <- table_caption(name)
  if (name %in% c("sum_tbl_probit_quant", "sum_tbl_probit_cat", "sum_tbl_iv", "probit_mfx", "fs_cons", "cons_iv")) {
    return(kableExtra::linebreak(cap, align = "c"))
  }
  cap
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
  se_rows <- regression_standard_error_rows(df)
  if (length(se_rows)) {
    tex <- kableExtra::row_spec(tex, se_rows, italic = TRUE, color = "gray35")
  }
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

save_table_tex <- function(table, path, name, public = TRUE) {
  need_pkg("kableExtra", "LaTeX table output")
  df <- sanitize_table_for_kable(format_table_for_output(table, public = public))
  grouped <- summary_table_groups(df)
  df_render <- wrap_table_text_columns(grouped$data, name)
  wide_summary_table <- name %in% c("sum_tbl_iv", "sum_tbl_probit_quant", "sum_tbl_probit_cat")
  regression_table <- name %in% c("probit_mfx", "fs_cons", "cons_iv")
  if (regression_table && ncol(df_render) >= 2L) {
    names(df_render)[[1]] <- ""
    names(df_render)[[2]] <- "(1)"
  } else {
    names(df_render) <- table_header_labels(df_render, name)
  }
  tex <- kableExtra::kbl(
    df_render,
    format = "latex",
    booktabs = TRUE,
    longtable = !(wide_summary_table || regression_table),
    label = table_label(name),
    caption = caption_for_latex(name),
    escape = FALSE,
    linesep = "",
    digits = 3,
    align = table_alignments(df_render, name),
    row.names = FALSE
  )
  latex_options <- if (regression_table) character() else c("striped")
  if (!(wide_summary_table || regression_table)) latex_options <- c(latex_options, "repeat_header")
  if (regression_table) latex_options <- c(latex_options, "hold_position")
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
      kableExtra::column_spec(1, width = "3.0cm") |>
      kableExtra::column_spec(2, width = "5.0cm") |>
      kableExtra::column_spec(3, width = "2.6cm") |>
      kableExtra::column_spec(4, width = "1.35cm") |>
      kableExtra::column_spec(5, width = "2.9cm") |>
      kableExtra::column_spec(6, width = "1.45cm") |>
      kableExtra::column_spec(7, width = "1.25cm")
  }
  if (name == "sum_tbl_iv") {
    tex <- tex |>
      kableExtra::column_spec(1, width = "3.5cm") |>
      kableExtra::column_spec(2, width = "5.0cm") |>
      kableExtra::column_spec(3:ncol(df_render), width = "1.55cm")
  }
  if (name == "sum_tbl_probit_quant") {
    tex <- tex |>
      kableExtra::column_spec(1, width = "4.3cm") |>
      kableExtra::column_spec(2:ncol(df_render), width = "1.75cm")
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
  }
  writeLines(as.character(tex), path)
  path
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
