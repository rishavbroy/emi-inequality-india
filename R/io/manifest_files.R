# Shared manifest lookup helpers for extended source modules.

manifest_file_by_id <- function(paths, source_id, file_id, label = file_id) {
  rows <- require_manifest_files(paths, source_id = source_id, required_only = FALSE)
  row <- rows[rows$file_id == file_id, , drop = FALSE]
  if (nrow(row) != 1L) stop("Expected one ", label, " manifest row.", call. = FALSE)
  row$absolute_path[[1L]]
}
