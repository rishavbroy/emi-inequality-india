# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' read raw file manifest
#'
#' @return Data frame with one row per manifest entry.
read_manifest <- function(paths = build_paths()) {
  manifest_path <- path_metadata(paths, "file_manifest.csv")
  if (!file.exists(manifest_path)) stop("Missing file manifest: ", manifest_path, call. = FALSE)

  header <- names(utils::read.csv(manifest_path, nrows = 0L, stringsAsFactors = FALSE))
  col_classes <- rep(NA_character_, length(header))
  names(col_classes) <- header
  if ("sha256" %in% header) col_classes[["sha256"]] <- "character"

  utils::read.csv(
    manifest_path,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA"),
    colClasses = col_classes
  )
}

#' filter manifest rows for source or target
#'
#' @return Data frame of required manifest rows with absolute paths and existence status.
manifest_rows <- function(paths, source_id = NULL, target_name = NULL, required_only = TRUE) {
  manifest <- read_manifest(paths)
  if (!is.null(source_id)) manifest <- manifest[manifest$source_id %in% source_id, , drop = FALSE]
  if (!is.null(target_name)) manifest <- manifest[manifest$target_name %in% target_name, , drop = FALSE]
  if (isTRUE(required_only) && "required_for_current_pipeline" %in% names(manifest)) {
    manifest <- manifest[tolower(as.character(manifest$required_for_current_pipeline)) == "true", , drop = FALSE]
  }
  manifest$absolute_path <- path_project(paths, manifest$relative_path)
  manifest$exists <- file.exists(manifest$absolute_path)
  manifest
}

#' format missing raw-data message
#'
#' @return Character scalar suitable for an error message.
missing_data_message <- function(rows, label = NULL) {
  missing <- rows[!rows$exists, , drop = FALSE]
  label <- label %||% unique(rows$source_id)
  label <- paste(unique(plain_chr(label)), collapse = ", ")
  paste0(
    "Missing raw data for ", label, ".\n",
    "The pipeline checks data/metadata/file_manifest.csv before reading raw data.\n",
    "Place these files at the listed paths, or edit the manifest if your local layout differs:\n",
    paste0("  - ", missing$relative_path, collapse = "\n"),
    "\n\nRaw data are intentionally not tracked in GitHub."
  )
}

source_integrity_message <- function(rows) {
  bad_size <- rows$exists & !rows$size_matches
  bad_hash <- rows$exists & !rows$sha256_matches
  lines <- character()
  if (any(bad_size)) {
    lines <- c(lines, paste0(
      "  - ", rows$relative_path[bad_size],
      " (", rows$size_bytes[bad_size], " bytes; expected ",
      rows$expected_size_bytes[bad_size], ")"
    ))
  }
  if (any(bad_hash)) {
    lines <- c(lines, paste0(
      "  - ", rows$relative_path[bad_hash], " (SHA-256 mismatch)"
    ))
  }
  paste0(
    "Raw-source integrity check failed.\n",
    "The following file(s) differ from data/metadata/file_manifest.csv:\n",
    paste(unique(lines), collapse = "\n"),
    "\nReacquire the registered source bytes or intentionally update the manifest."
  )
}

validate_manifest_rows <- function(rows) {
  if (!"expected_size_bytes" %in% names(rows)) rows$expected_size_bytes <- NA_real_
  if (!"sha256" %in% names(rows)) rows$sha256 <- NA_character_

  rows$expected_size_bytes <- suppressWarnings(as.numeric(rows$expected_size_bytes))
  rows$sha256 <- normalize_sha256(rows$sha256)
  malformed_hash <- !is.na(rows$sha256) & !is_sha256(rows$sha256)
  if (any(malformed_hash)) {
    stop(
      "file_manifest.csv contains malformed SHA-256 value(s) for: ",
      paste(rows$relative_path[malformed_hash], collapse = ", "),
      call. = FALSE
    )
  }

  is_regular_file <- rows$exists & !dir.exists(rows$absolute_path)
  rows$size_bytes <- ifelse(is_regular_file, file.info(rows$absolute_path)$size, NA_real_)
  rows$size_matches <- is.na(rows$expected_size_bytes) |
    !is_regular_file |
    rows$expected_size_bytes == rows$size_bytes

  should_hash <- is_regular_file & !is.na(rows$sha256)
  rows$actual_sha256 <- NA_character_
  if (any(should_hash)) {
    rows$actual_sha256[should_hash] <- sha256_files(rows$absolute_path[should_hash])
  }
  rows$sha256_matches <- is.na(rows$sha256) |
    !is_regular_file |
    rows$sha256 == rows$actual_sha256
  rows
}

#' validate raw files
#'
#' @return Data frame with manifest metadata, absolute paths, existence, size, and SHA-256 checks.
validate_raw_files <- function(paths = build_paths()) {
  validate_manifest_rows(manifest_rows(paths))
}

#' require manifest files before reading raw data
#'
#' @return Data frame of matching manifest rows, invisibly if all required files are valid.
require_manifest_files <- function(
  paths, source_id = NULL, target_name = NULL, required_only = TRUE
) {
  rows <- manifest_rows(
    paths, source_id = source_id, target_name = target_name, required_only = required_only
  )
  if (!nrow(rows)) stop("No matching rows in file_manifest.csv.", call. = FALSE)
  rows <- validate_manifest_rows(rows)
  if (any(!rows$exists)) stop(missing_data_message(rows, source_id %||% target_name), call. = FALSE)
  if (any(!rows$size_matches | !rows$sha256_matches)) {
    stop(source_integrity_message(rows), call. = FALSE)
  }
  rows
}

#' stop if required files are missing or differ from pinned source identity
#'
#' @return Invisible TRUE when all active required files exist and pass registered checks.
stop_if_required_files_invalid <- function(manifest_status) {
  required <- tolower(as.character(manifest_status$required_for_current_pipeline)) == "true"
  missing <- manifest_status[required & !manifest_status$exists, , drop = FALSE]
  if (nrow(missing)) stop(missing_data_message(missing), call. = FALSE)

  invalid <- manifest_status[
    required & manifest_status$exists &
      (!manifest_status$size_matches | !manifest_status$sha256_matches),
    , drop = FALSE
  ]
  if (nrow(invalid)) stop(source_integrity_message(invalid), call. = FALSE)
  invisible(TRUE)
}
