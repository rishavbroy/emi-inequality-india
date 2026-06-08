# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' build paths
#'
build_paths <- function(root = ".") {
  root <- normalizePath(root, mustWork = FALSE)
  list(root = root, data = file.path(root, "data"), raw = file.path(root, "data", "raw"), raw_future = file.path(root, "data", "raw_future"), interim = file.path(root, "data", "interim"), processed = file.path(root, "data", "processed"), metadata = file.path(root, "data", "metadata"), assets = file.path(root, "assets"), outputs = file.path(root, "outputs"), figures = file.path(root, "outputs", "figures"), tables = file.path(root, "outputs", "tables"), diagnostics = file.path(root, "outputs", "diagnostics"), paper_output = file.path(root, "paper", "output"), app_samples_output = file.path(root, "application-samples", "output"))
}

#' ensure project dirs
#'
ensure_project_dirs <- function(paths) {
  dirs <- unlist(paths[setdiff(names(paths), "root")], use.names = FALSE)
  invisible(vapply(dirs, dir.create, logical(1), recursive = TRUE, showWarnings = FALSE))
}

#' path raw
#'
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
path_processed <- function(paths, ...) {
  file.path(paths$processed, ...)
}

#' path metadata
#'
path_metadata <- function(paths, ...) {
  file.path(paths$metadata, ...)
}

#' path outputs
#'
path_outputs <- function(paths, ...) {
  file.path(paths$outputs, ...)
}

#' path figures
#'
path_figures <- function(paths, ...) {
  file.path(paths$figures, ...)
}

#' path tables
#'
path_tables <- function(paths, ...) {
  file.path(paths$tables, ...)
}

#' path diagnostics
#'
path_diagnostics <- function(paths, ...) {
  file.path(paths$diagnostics, ...)
}

#' path assets
#'
path_assets <- function(paths, ...) {
  file.path(paths$assets, ...)
}
