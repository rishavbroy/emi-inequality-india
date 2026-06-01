# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' validate raw files
#'
#' @return A tibble, model object, list, or file path depending on context.
validate_raw_files <- function(paths) {
  manifest_path <- path_metadata(paths, "file_manifest.csv")
  if (!file.exists(manifest_path)) stop("Missing file manifest: ", manifest_path)
  manifest <- readr::read_csv(manifest_path, show_col_types = FALSE)
  manifest <- manifest |> dplyr::mutate(exists = file.exists(relative_path), actual_size_bytes = dplyr::if_else(exists, file.info(relative_path)$size, NA_real_))
  manifest
}

#' stop if required files missing
#'
#' @return A tibble, model object, list, or file path depending on context.
stop_if_required_files_missing <- function(manifest_status) {
  missing <- manifest_status |> dplyr::filter(required_for_current_pipeline, !exists)
  if (nrow(missing)) stop("Missing required raw files:
  ", paste(missing$relative_path, collapse = "
  "))
  invisible(TRUE)
}

