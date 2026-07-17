# Shared helpers used by public Quarto documents and extracted samples.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

find_targets_store <- function(start = getwd()) {
  here <- normalizePath(start, mustWork = TRUE)
  repeat {
    candidate <- file.path(here, "_targets")
    if (dir.exists(candidate)) return(candidate)
    parent <- dirname(here)
    if (identical(parent, here)) return("_targets")
    here <- parent
  }
}

load_public_report_values <- function() {
  tryCatch(targets::tar_read(report_values, store = find_targets_store()), error = function(e) list())
}

initialize_public_qmd_helpers <- function(env = parent.frame()) {
  assign("report_values", load_public_report_values(), envir = env)
  invisible(TRUE)
}

is_report_value_status <- function(value) {
  is.list(value) && !is.null(value$status) && !is.null(value$reason)
}

report_value <- function(key) {
  value <- report_values[[key]]
  if (is.null(value)) value <- NA
  if (is_report_value_status(value)) {
    display <- value$value
    if (is.null(display) || length(display) == 0L || all(is.na(display))) display <- value$display
    if (is.null(display) || length(display) == 0L || all(is.na(display))) display <- "—"
    value <- display
  }
  if (length(value) == 0L || all(is.na(value))) return("—")
  paste(value, collapse = ", ")
}

regression_star_note <- function() "* p < 0.05, ** p < 0.01, *** p < 0.001"

public_table_caption_text <- function(name) {
  captions <- c(
    sum_tbl_probit_quant = "Summary Statistics for Enrollment Participation Model (Numeric Variables)",
    sum_tbl_probit_cat = "Summary Statistics for Enrollment Participation Model (Categorical Variables)",
    probit_mfx = "Average Marginal Effects and Counterfactual Comparisons for Enrollment Probit",
    sum_tbl_iv = "Summary Statistics for 2SLS Model",
    fs_cons = "First-Stage Regression: EMI Exposure on Linguistic Distance",
    cons_iv = "Second-Stage Regression: Consumption Growth on EMIE (Fitted)"
  )
  captions[[name]] %||% name
}

regression_caption <- function(cap) cap

table_caption <- function(name) {
  public_table_caption_text(name)
}

table_note <- function(name) {
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

resolve_public_output_path <- function(path) {
  candidates <- unique(c(path, file.path(getwd(), path), file.path("paper", path), file.path(dirname(knitr::current_input()), path), sub("^\\.\\/", "", path)))
  hit <- candidates[file.exists(candidates) & file.info(candidates)$size > 0]
  if (length(hit)) return(hit[[1]])
  stop("Missing table output: ", path, call. = FALSE)
}

read_public_table <- function(path) {
  df <- utils::read.csv(resolve_public_output_path(path), check.names = FALSE, na.strings = character())
  for (nm in names(df)) if (is.character(df[[nm]])) df[[nm]][is.na(df[[nm]])] <- ""
  df
}

render_public_tex <- function(path) {
  tex <- paste(readLines(resolve_public_output_path(path), warn = FALSE), collapse = "\n")
  knitr::asis_output(paste0("\n\n", tex, "\n\n"))
}

cell_string <- function(x) {
  if (length(x) == 0L) return("")
  if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
  if (length(x) == 0L || all(is.na(x))) return("")
  paste(as.character(x), collapse = "; ")
}

column_strings <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.list(x)) out <- vapply(x, cell_string, character(1)) else out <- as.character(x)
  out[is.na(out)] <- ""
  out
}

summary_table_groups <- function(df) {
  if (!nrow(df) || !length(names(df))) return(list(data = df, groups = data.frame()))
  empty_rest <- if (ncol(df) > 1L) apply(df[-1], 1, function(x) all(!nzchar(column_strings(x)))) else rep(TRUE, nrow(df))
  first_col <- column_strings(df[[1]])
  group_row <- grepl(":$", first_col) & empty_rest
  group_idx <- which(group_row)
  if (!length(group_idx)) return(list(data = df, groups = data.frame()))
  groups <- lapply(seq_along(group_idx), function(i) {
    start_orig <- group_idx[[i]] + 1L
    end_orig <- if (i < length(group_idx)) group_idx[[i + 1L]] - 1L else nrow(df)
    start <- start_orig - sum(group_idx < start_orig)
    end <- end_orig - sum(group_idx <= end_orig)
    if (start > end) return(NULL)
    data.frame(label = first_col[[group_idx[[i]]]], start = start, end = end, stringsAsFactors = FALSE)
  })
  groups <- do.call(rbind, Filter(Negate(is.null), groups))
  if (is.null(groups)) groups <- data.frame()
  list(data = df[!group_row, , drop = FALSE], groups = groups)
}

wrap_table_text <- function(df) as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)

table_header_labels <- function(df, name) {
  labels <- names(df)
  wrap <- c("Pct. Mode" = "Pct.\nMode", "Least Freq." = "Least\nFreq.", "Pct. Least Freq." = "Pct. Least\nFreq.", "Adjusted R-squared" = "Adjusted\nR-squared")
  labels <- ifelse(labels %in% names(wrap), unname(wrap[labels]), labels)
  vapply(labels, function(x) if (grepl("\n", x, fixed = TRUE)) kableExtra::linebreak(x, align = "c") else x, character(1))
}

caption_for_latex <- function(name) {
  # Keep captions as plain text. Caption wrapping is handled by the LaTeX caption package;
  # kableExtra::linebreak() is for cells/headers and corrupts full kable captions.
  table_caption(name)
}

latex_escape_text <- function(x) {
  column_strings(x)
}

render_regression_table <- function(df, name) {
  if (!requireNamespace("modelsummary", quietly = TRUE)) stop("modelsummary is required for regression table rendering.", call. = FALSE)
  if (ncol(df) < 2L) return(knitr::kable(df, row.names = FALSE))
  model_col <- switch(name, probit_mfx = "Enrolled (1 = yes)", fs_cons = "EMI Exposure", cons_iv = "Consumption Growth", names(df)[[2]])
  out <- data.frame(Term = latex_escape_text(df[[1]]), stringsAsFactors = FALSE, check.names = FALSE)
  out[[model_col]] <- latex_escape_text(df[[2]])
  out$Term[!nzchar(out$Term)] <- "~"
  # Use modelsummary for layout, but emit Markdown into Quarto rather than raw LaTeX.
  # Raw modelsummary LaTeX tabular output is fragile inside extracted writing-sample
  # chunks with Quarto table captions; Markdown lets Pandoc own the final LaTeX table.
  note <- table_note(name)
  tab <- suppressWarnings(modelsummary::datasummary_df(out, output = "markdown", fmt = identity, align = "lc"))
  md <- as.character(tab)
  if (!is.null(note) && !grepl(note, md, fixed = TRUE)) md <- paste0(md, "\n\n_", note, "_")
  md
}

render_public_table <- function(path, name) {
  if (tolower(tools::file_ext(path)) == "tex") return(render_public_tex(path))
  df <- read_public_table(path)
  grouped <- summary_table_groups(df)
  df_render <- wrap_table_text(grouped$data)
  wide <- name %in% c("sum_tbl_iv", "sum_tbl_probit_quant", "sum_tbl_probit_cat")
  regression <- name %in% c("probit_mfx", "fs_cons", "cons_iv")
  if (regression) return(knitr::asis_output(render_regression_table(df_render, name)))
  names(df_render) <- table_header_labels(df_render, name)
  tab <- knitr::kable(df_render, digits = 3, booktabs = knitr::is_latex_output(), longtable = knitr::is_latex_output() && !wide, escape = FALSE, row.names = FALSE, caption = caption_for_latex(name), linesep = "")
  if (knitr::is_latex_output() && requireNamespace("kableExtra", quietly = TRUE)) {
    opts <- c("striped")
    if (!wide) opts <- c(opts, "repeat_header")
    tab <- kableExtra::kable_styling(tab, latex_options = opts, position = "center", full_width = FALSE, font_size = 10)
    if (nrow(grouped$groups)) {
      for (i in rev(seq_len(nrow(grouped$groups)))) {
        tab <- kableExtra::pack_rows(tab, grouped$groups$label[[i]], grouped$groups$start[[i]], grouped$groups$end[[i]], bold = TRUE, italic = FALSE, background = "white", escape = FALSE)
      }
    }
    if (name == "sum_tbl_probit_cat") {
      tab <- kableExtra::column_spec(kableExtra::column_spec(kableExtra::column_spec(kableExtra::column_spec(kableExtra::column_spec(kableExtra::column_spec(kableExtra::column_spec(tab, 1, width = "3.0cm"), 2, width = "5.0cm"), 3, width = "2.4cm"), 4, width = "1.35cm"), 5, width = "2.7cm"), 6, width = "1.45cm"), 7, width = "1.25cm")
    }
    if (name == "sum_tbl_iv") tab <- kableExtra::column_spec(kableExtra::column_spec(kableExtra::column_spec(tab, 1, width = "3.0cm"), 2, width = "4.6cm"), 3:ncol(df_render), width = "1.45cm")
    if (name == "sum_tbl_probit_quant") tab <- kableExtra::column_spec(kableExtra::column_spec(tab, 1, width = "4.0cm"), 2:ncol(df_render), width = "1.55cm")
    note <- table_note(name)
    if (!is.null(note)) tab <- kableExtra::footnote(tab, general = note, threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE)
    if (wide) tab <- kableExtra::landscape(tab)
  }
  tab
}
