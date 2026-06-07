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

format_table_for_output <- function(table, public = TRUE) {
  out <- as.data.frame(table)
  if (!nrow(out)) return(out)

  if (public) {
    # Diagnostic status columns are kept in internal tables only. They are
    # removed from polished public tables whenever they contain no substantive
    # warnings; final audits fail if public tables still expose them.
    if ("status" %in% names(out) && all(is.na(out$status) | out$status %in% c("mapped", "estimated"))) out$status <- NULL
    if ("reason" %in% names(out) && all(is.na(out$reason) | !nzchar(as.character(out$reason)))) out$reason <- NULL
    if ("method" %in% names(out) && length(unique(stats::na.omit(out$method))) <= 1L) out$method <- NULL
  }

  keep <- vapply(out, function(col) !all(is.na(col) | !nzchar(as.character(col))), logical(1))
  out <- out[, keep, drop = FALSE]
  names(out) <- vapply(names(out), nice_column_name, character(1))
  out
}

save_table_csv <- function(table, path, public = TRUE) {
  utils::write.csv(format_table_for_output(table, public = public), path, row.names = FALSE)
  path
}

save_table_tex <- function(table, path, name, public = TRUE) {
  need_pkg("kableExtra", "LaTeX table output")
  df <- format_table_for_output(table, public = public)
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
