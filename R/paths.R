# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' build paths
#'
build_paths <- function(root = ".") {
  root <- normalizePath(root, mustWork = FALSE)
  list(root = root, data = file.path(root, "data"), raw = file.path(root, "data", "raw"), raw_future = file.path(root, "data", "raw_future"), interim = file.path(root, "data", "interim"), processed = file.path(root, "data", "processed"), metadata = file.path(root, "data", "metadata"), assets = file.path(root, "assets"), outputs = file.path(root, "outputs"), figures = file.path(root, "outputs", "figures"), tables = file.path(root, "outputs", "tables"), diagnostics = file.path(root, "outputs", "diagnostics"), paper_output = file.path(root, "paper", "output"), app_samples_output = file.path(root, "application-samples", "output"))
}



#' path from project root
#'
#' @return Absolute path under the project root.
path_project <- function(paths, ...) {
  file.path(paths$root, ...)
}


#' path metadata
#'
path_metadata <- function(paths, ...) {
  file.path(paths$metadata, ...)
}
