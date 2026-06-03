# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' read raw file manifest
#'
#' @return Data frame with one row per manifest entry.
read_manifest <- function(paths = build_paths()) {
  manifest_path <- path_metadata(paths, "file_manifest.csv")
  if (!file.exists(manifest_path)) stop("Missing file manifest: ", manifest_path, call. = FALSE)
  utils::read.csv(manifest_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}

#' filter manifest rows for source or target
#'
#' @return Data frame of required manifest rows with absolute paths and existence status.
manifest_rows <- function(paths, source_id = NULL, target_name = NULL) {
  manifest <- read_manifest(paths)
  if (!is.null(source_id)) manifest <- manifest[manifest$source_id %in% source_id, , drop = FALSE]
  if (!is.null(target_name)) manifest <- manifest[manifest$target_name %in% target_name, , drop = FALSE]
  if ("required_for_current_pipeline" %in% names(manifest)) {
    manifest <- manifest[tolower(as.character(manifest$required_for_current_pipeline)) == "true", , drop = FALSE]
  }
  manifest$absolute_path <- file.path(paths$root, manifest$relative_path)
  manifest$exists <- file.exists(manifest$absolute_path)
  manifest
}

#' format missing raw-data message
#'
#' @return Character scalar suitable for an error message.
missing_data_message <- function(rows, label = NULL) {
  missing <- rows[!rows$exists, , drop = FALSE]
  paste0(
    "Missing raw data for ", label %||% paste(unique(rows$source_id), collapse = ", "), ".\n",
    "The pipeline checks data/metadata/file_manifest.csv before reading raw data.\n",
    "Place these files at the listed paths, or edit the manifest if your local layout differs:\n",
    paste0("  - ", missing$relative_path, collapse = "\n"),
    "\n\nRaw data are intentionally not tracked in GitHub."
  )
}

#' validate raw files
#'
#' @return Data frame with manifest metadata, absolute paths, existence, and size checks.
validate_raw_files <- function(paths = build_paths()) {
  manifest <- manifest_rows(paths)
  manifest$expected_size_bytes <- suppressWarnings(as.numeric(manifest$expected_size_bytes))
  manifest$size_bytes <- ifelse(manifest$exists, file.info(manifest$absolute_path)$size, NA_real_)
  manifest$size_matches <- is.na(manifest$expected_size_bytes) |
    is.na(manifest$size_bytes) |
    manifest$expected_size_bytes == manifest$size_bytes
  manifest
}

#' require manifest files before reading raw data
#'
#' @return Data frame of matching manifest rows, invisibly if all required files exist.
require_manifest_files <- function(paths, source_id = NULL, target_name = NULL) {
  rows <- manifest_rows(paths, source_id = source_id, target_name = target_name)
  if (!nrow(rows)) stop("No matching rows in file_manifest.csv.", call. = FALSE)
  if (any(!rows$exists)) stop(missing_data_message(rows, source_id %||% target_name), call. = FALSE)
  rows
}

#' stop if required files missing
#'
#' @return Invisible TRUE when all active required files exist.
stop_if_required_files_missing <- function(manifest_status) {
  required <- tolower(as.character(manifest_status$required_for_current_pipeline)) == "true"
  missing <- manifest_status[required & !manifest_status$exists, , drop = FALSE]
  if (nrow(missing)) stop(missing_data_message(missing), call. = FALSE)
  invisible(TRUE)
}
