# Shared helpers used by public Quarto documents and extracted samples.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}


source_table_contract <- function(start = getwd(), env = parent.frame()) {
  here <- normalizePath(start, mustWork = TRUE)
  repeat {
    candidate <- file.path(here, "R", "output", "table_contract.R")
    if (file.exists(candidate)) {
      sys.source(candidate, envir = env)
      sys.source(file.path(here, "R", "output", "report_value_core.R"), envir = env)
      return(invisible(candidate))
    }
    parent <- dirname(here)
    if (identical(parent, here)) stop("Cannot locate R/output/table_contract.R", call. = FALSE)
    here <- parent
  }
}

if (!exists("public_table_caption_text", mode = "function")) {
  source_table_contract(env = environment())
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

wrap_table_text <- function(df) as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)

render_regression_table <- function(df, name) {
  if (!requireNamespace("modelsummary", quietly = TRUE)) stop("modelsummary is required for regression table rendering.", call. = FALSE)
  if (ncol(df) < 2L) return(knitr::kable(df, row.names = FALSE))
  model_col <- switch(name, probit_mfx = "Enrolled (1 = yes)", fs_cons = "EMI Exposure", cons_iv = "Real Log Consumption Growth", names(df)[[2]])
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
