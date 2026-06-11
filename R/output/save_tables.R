# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

table_output_dir <- function(cfg) {
  "outputs/tables/main"
}

table_formats <- function(cfg) {
  out <- cfg$output_formats$tables %||% "csv"
  unique(as.character(out))
}

table_label <- function(name) {
  gsub("_", "-", name, fixed = TRUE)
}

table_caption <- function(name) {
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

nice_column_name <- function(x) {
  x <- gsub("\\.", " ", x)
  x <- gsub("_", " ", x)
  x <- gsub("p value", "p-value", x, ignore.case = TRUE)
  x <- gsub("std error", "Std. Error", x, ignore.case = TRUE)
  tools::toTitleCase(x)
}

drop_empty_output_columns <- function(out) {
  if (!nrow(out)) return(out)
  keep <- vapply(out, function(col) !all(is.na(col) | !nzchar(as.character(col))), logical(1))
  out[, keep, drop = FALSE]
}

format_public_summary_columns <- function(out) {
  if (all(c("var", "label") %in% names(out))) {
    out$Variable <- out$label
    group <- startsWith(as.character(out$var), ".group_")
    out$var <- NULL
    out$label <- NULL
    out <- out[, c("Variable", setdiff(names(out), "Variable")), drop = FALSE]
    if (any(group)) {
      for (nm in setdiff(names(out), "Variable")) out[[nm]][group] <- ""
    }
  }
  rename <- c("% Mode" = "Pct. Mode", "% Least Freq." = "Pct. Least Freq.")
  for (old in names(rename)) if (old %in% names(out)) names(out)[names(out) == old] <- rename[[old]]
  preferred <- c("Variable", "Values", "Mode", "Pct. Mode", "Least Freq.", "Pct. Least Freq.", "Min", "1Q", "Med", "3Q", "Max", "Mean", "SD", "N")
  ordered <- c(intersect(preferred, names(out)), setdiff(names(out), preferred))
  out[, ordered, drop = FALSE]
}


is_status_only_table <- function(out) {
  if (!nrow(out) || !"status" %in% names(out)) return(FALSE)
  substantive <- setdiff(names(out), c("status", "reason", "method", "model"))
  if (!length(substantive)) return(TRUE)
  all(vapply(out[substantive], function(col) all(is.na(col) | !nzchar(as.character(col))), logical(1)))
}

format_status_table_for_output <- function(out, public = TRUE) {
  status <- if ("status" %in% names(out)) as.character(out$status) else rep("unavailable", nrow(out))
  reason <- if ("reason" %in% names(out)) as.character(out$reason) else rep(NA_character_, nrow(out))
  model <- if ("model" %in% names(out)) as.character(out$model) else rep(NA_character_, nrow(out))

  status[is.na(status) | !nzchar(status)] <- "unavailable"
  reason[is.na(reason) | !nzchar(reason)] <- "No completed model output is available."
  model[is.na(model) | !nzchar(model)] <- "output"

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
  already_polished <- any(names(out) %in% c("Term", "Estimate", "Std. Error", "N", "Min", "1Q", "Med", "3Q", "Max", "Mean", "SD", "Variable", "Consumption Growth", "EMI Exposure", "Enrolled in School (1 = yes)"))
  if (!already_polished) names(out) <- vapply(names(out), nice_column_name, character(1))
  out
}


summary_table_groups <- function(df) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(df) || !length(names(df))) return(list(data = df, groups = data.frame()))
  empty_rest <- if (ncol(df) > 1L) {
    apply(df[-1], 1, function(x) all(is.na(x) | !nzchar(as.character(x))))
  } else {
    rep(TRUE, nrow(df))
  }
  group_row <- grepl(":$", as.character(df[[1]])) & empty_rest
  group_idx <- which(group_row)
  if (!length(group_idx)) return(list(data = df, groups = data.frame()))

  groups <- lapply(seq_along(group_idx), function(i) {
    start_orig <- group_idx[[i]] + 1L
    end_orig <- if (i < length(group_idx)) group_idx[[i + 1L]] - 1L else nrow(df)
    start <- start_orig - sum(group_idx < start_orig)
    end <- end_orig - sum(group_idx <= end_orig)
    if (start > end) return(NULL)
    data.frame(
      label = as.character(df[[1]][[group_idx[[i]]]]),
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

sanitize_table_for_kable <- function(df) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(df)) df <- data.frame(Note = "No rows to display.", stringsAsFactors = FALSE)
  if (!length(names(df))) df <- data.frame(Note = rep("No displayable columns.", nrow(df)), stringsAsFactors = FALSE)
  for (nm in names(df)) {
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
    if (is.list(df[[nm]])) {
      df[[nm]] <- vapply(df[[nm]], function(value) {
        if (length(value) == 0L || all(is.na(value))) return("")
        paste(as.character(value), collapse = "; ")
      }, character(1))
    }
    if (is.character(df[[nm]])) df[[nm]][is.na(df[[nm]])] <- ""
  }
  render_table_math_labels(df)
}

save_table_csv <- function(table, path, public = TRUE) {
  utils::write.csv(sanitize_table_for_kable(format_table_for_output(table, public = public)), path, row.names = FALSE)
  path
}

save_table_tex <- function(table, path, name, public = TRUE) {
  need_pkg("kableExtra", "LaTeX table output")
  df <- sanitize_table_for_kable(format_table_for_output(table, public = public))
  grouped <- summary_table_groups(df)
  df_render <- grouped$data
  wide_summary_table <- name %in% c("sum_tbl_iv", "sum_tbl_probit_quant", "sum_tbl_probit_cat")
  tex <- kableExtra::kbl(
    df_render,
    format = "latex",
    booktabs = TRUE,
    longtable = !wide_summary_table,
    label = table_label(name),
    caption = table_caption(name),
    escape = FALSE,
    digits = 3,
    row.names = FALSE
  )
  latex_options <- c("striped")
  if (!wide_summary_table) latex_options <- c(latex_options, "repeat_header")
  if (wide_summary_table) latex_options <- c(latex_options, "scale_down")
  if (name %in% c("probit_mfx", "fs_cons", "cons_iv")) latex_options <- c(latex_options, "hold_position")
  tex <- kableExtra::kable_styling(
    tex,
    latex_options = latex_options,
    full_width = FALSE,
    position = "center",
    font_size = if (wide_summary_table) 8 else NULL
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
        background = "gray!12",
        escape = FALSE
      )
    }
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
  unlist(lapply(names(tables), function(n) {
    paths <- character()
    public <- n %in% public_table_names
    if ("csv" %in% formats) paths <- c(paths, save_table_csv(tables[[n]], file.path(dir, paste0(n, ".csv")), public = public))
    if ("tex" %in% formats) paths <- c(paths, save_table_tex(tables[[n]], file.path(dir, paste0(n, ".tex")), n, public = public))
    paths
  }), use.names = FALSE)
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
