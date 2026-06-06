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

format_table_for_output <- function(table) {
  out <- as.data.frame(table)
  names(out) <- gsub("\\.", " ", names(out), fixed = TRUE)
  names(out) <- gsub("_", " ", names(out), fixed = TRUE)
  names(out) <- tools::toTitleCase(names(out))
  out
}

save_table_csv <- function(table, path) {
  utils::write.csv(as.data.frame(table), path, row.names = FALSE)
  path
}

save_table_tex <- function(table, path, name) {
  need_pkg("kableExtra", "LaTeX table output")
  df <- format_table_for_output(table)
  tex <- kableExtra::kbl(
    df,
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    label = table_label(name),
    caption = table_caption(name),
    escape = FALSE,
    digits = 3
  )
  tex <- kableExtra::kable_styling(
    tex,
    latex_options = c("repeat_header", "striped"),
    full_width = FALSE
  )
  if (name %in% c("sum_tbl_iv", "sum_tbl_probit_quant", "sum_tbl_probit_cat")) {
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
  unlist(lapply(names(tables), function(n) {
    paths <- character()
    if ("csv" %in% formats) {
      paths <- c(paths, save_table_csv(tables[[n]], file.path(dir, paste0(n, ".csv"))))
    }
    if ("tex" %in% formats) {
      paths <- c(paths, save_table_tex(tables[[n]], file.path(dir, paste0(n, ".tex")), n))
    }
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
