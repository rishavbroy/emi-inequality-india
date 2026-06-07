# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' build paths
#'
#' @return Internal pipeline output used by the targets graph.
build_paths <- function(root = ".") {
  root <- normalizePath(root, mustWork = FALSE)
  list(root = root, data = file.path(root, "data"), raw = file.path(root, "data", "raw"), raw_future = file.path(root, "data", "raw_future"), interim = file.path(root, "data", "interim"), processed = file.path(root, "data", "processed"), metadata = file.path(root, "data", "metadata"), assets = file.path(root, "assets"), outputs = file.path(root, "outputs"), figures = file.path(root, "outputs", "figures"), tables = file.path(root, "outputs", "tables"), diagnostics = file.path(root, "outputs", "diagnostics"), paper_output = file.path(root, "paper", "output"), app_samples_output = file.path(root, "application-samples", "output"))
}

#' ensure project dirs
#'
#' @return Internal pipeline output used by the targets graph.
ensure_project_dirs <- function(paths) {
  dirs <- unlist(paths[setdiff(names(paths), "root")], use.names = FALSE)
  invisible(vapply(dirs, dir.create, logical(1), recursive = TRUE, showWarnings = FALSE))
}

#' path raw
#'
#' @return Internal pipeline output used by the targets graph.
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
#' @return Internal pipeline output used by the targets graph.
path_processed <- function(paths, ...) {
  file.path(paths$processed, ...)
}

#' path metadata
#'
#' @return Internal pipeline output used by the targets graph.
path_metadata <- function(paths, ...) {
  file.path(paths$metadata, ...)
}

#' path outputs
#'
#' @return Internal pipeline output used by the targets graph.
path_outputs <- function(paths, ...) {
  file.path(paths$outputs, ...)
}

#' path figures
#'
#' @return Internal pipeline output used by the targets graph.
path_figures <- function(paths, ...) {
  file.path(paths$figures, ...)
}

#' path tables
#'
#' @return Internal pipeline output used by the targets graph.
path_tables <- function(paths, ...) {
  file.path(paths$tables, ...)
}

#' path diagnostics
#'
#' @return Internal pipeline output used by the targets graph.
path_diagnostics <- function(paths, ...) {
  file.path(paths$diagnostics, ...)
}

#' path assets
#'
#' @return Internal pipeline output used by the targets graph.
path_assets <- function(paths, ...) {
  file.path(paths$assets, ...)
}
