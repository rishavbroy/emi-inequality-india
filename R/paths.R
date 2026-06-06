# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' build paths
#'
#' @return Function-specific return value.
build_paths <- function(root = ".") {
  root <- normalizePath(root, mustWork = FALSE)
  list(root = root, data = file.path(root, "data"), raw = file.path(root, "data", "raw"), raw_future = file.path(root, "data", "raw_future"), interim = file.path(root, "data", "interim"), processed = file.path(root, "data", "processed"), metadata = file.path(root, "data", "metadata"), assets = file.path(root, "assets"), outputs = file.path(root, "outputs"), figures = file.path(root, "outputs", "figures"), tables = file.path(root, "outputs", "tables"), diagnostics = file.path(root, "outputs", "diagnostics"), paper_output = file.path(root, "paper", "output"), app_samples_output = file.path(root, "application-samples", "output"))
}

#' ensure project dirs
#'
#' @return Function-specific return value.
ensure_project_dirs <- function(paths) {
  dirs <- unlist(paths[setdiff(names(paths), "root")], use.names = FALSE)
  invisible(vapply(dirs, dir.create, logical(1), recursive = TRUE, showWarnings = FALSE))
}

#' path raw
#'
#' @return Function-specific return value.
path_raw <- function(paths, ...) {
  file.path(paths$raw, ...)
}

#' path from project root
#'
#' @return Absolute path under the project root.
path_project <- function(paths, ...) {
  file.path(paths$root, ...)
}

#' path processed
#'
#' @return Function-specific return value.
path_processed <- function(paths, ...) {
  file.path(paths$processed, ...)
}

#' path metadata
#'
#' @return Function-specific return value.
path_metadata <- function(paths, ...) {
  file.path(paths$metadata, ...)
}

#' path outputs
#'
#' @return Function-specific return value.
path_outputs <- function(paths, ...) {
  file.path(paths$outputs, ...)
}

#' path figures
#'
#' @return Function-specific return value.
path_figures <- function(paths, ...) {
  file.path(paths$figures, ...)
}

#' path tables
#'
#' @return Function-specific return value.
path_tables <- function(paths, ...) {
  file.path(paths$tables, ...)
}

#' path diagnostics
#'
#' @return Function-specific return value.
path_diagnostics <- function(paths, ...) {
  file.path(paths$diagnostics, ...)
}

#' path assets
#'
#' @return Function-specific return value.
path_assets <- function(paths, ...) {
  file.path(paths$assets, ...)
}
