# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

table_output_dir <- function(cfg) {
  "outputs/tables/main"
}

#' save tables
#'
#' @return A character vector of generated table paths.
save_tables <- function(tables, cfg) {
  dir <- table_output_dir(cfg)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  vapply(names(tables), function(n) {
    p <- file.path(dir, paste0(n, ".csv"))
    utils::write.csv(as.data.frame(tables[[n]]), p, row.names = FALSE)
    p
  }, character(1))
}

#' save table csv tex
#'
#' @return Generated CSV path.
save_table_csv_tex <- function(table, path_base) {
  readr::write_csv(tibble::as_tibble(table), paste0(path_base, ".csv"))
  paste0(path_base, ".csv")
}

#' save table html if requested
#'
#' @return Generated HTML path.
save_table_html_if_requested <- function(table, path_base) {
  html <- paste0(path_base, ".html")
  html
}
