# Shared helpers for optional diagnostics and benchmarking outputs.

diagnostic_output_dir <- function(..., root = "outputs/diagnostics/extended") {
  file.path(root, ...)
}

benchmark_output_dir <- function(..., root = "outputs/benchmarking") {
  file.path(root, ...)
}

write_diagnostic_csv <- function(x, path, row.names = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  utils::write.csv(x, path, row.names = row.names)
  normalizePath(path, mustWork = FALSE)
}

write_diagnostic_matrix <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(as.matrix(x), path, row.names = TRUE)
  normalizePath(path, mustWork = FALSE)
}

output_manifest <- function(paths, description = names(paths)) {
  paths <- unlist(paths, use.names = FALSE)
  paths <- paths[nzchar(paths)]
  if (!length(paths)) {
    return(data.frame(path = character(), description = character(), stringsAsFactors = FALSE))
  }
  data.frame(
    path = paths,
    description = rep_len(description %||% basename(paths), length(paths)),
    stringsAsFactors = FALSE
  )
}

present_cols <- function(df, cols) {
  intersect(cols, names(as.data.frame(df)))
}

numeric_like <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

safe_pairwise_cor <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!nrow(df) || ncol(df) < 2L) return(matrix(numeric(), nrow = 0L, ncol = 0L))
  keep <- vapply(df, function(x) {
    y <- suppressWarnings(as.numeric(as.character(x)))
    sum(is.finite(y)) > 1L && stats::sd(y, na.rm = TRUE) > 0
  }, logical(1))
  if (sum(keep) < 2L) return(matrix(numeric(), nrow = 0L, ncol = 0L))
  num_df <- as.data.frame(lapply(df[keep], numeric_like), check.names = FALSE)
  stats::cor(num_df, use = "pairwise.complete.obs")
}

elapsed_seconds <- function(expr) {
  unname(system.time(force(expr))[["elapsed"]])
}

diagnostic_status_table <- function(diagnostic, status, reason = NA_character_) {
  data.frame(
    diagnostic = diagnostic,
    status = status,
    reason = reason,
    stringsAsFactors = FALSE
  )
}
