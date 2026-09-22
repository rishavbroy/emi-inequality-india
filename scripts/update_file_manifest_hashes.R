# Pin local raw-source bytes in file_manifest.csv for one or more source families.
# Usage: Rscript scripts/update_file_manifest_hashes.R SOURCE_ID [SOURCE_ID ...]

source("R/io/utils_data_frame.R")
source("R/io/file_integrity.R")

source_ids <- unique(commandArgs(trailingOnly = TRUE))
if (!length(source_ids)) {
  stop(
    "Usage: Rscript scripts/update_file_manifest_hashes.R SOURCE_ID [SOURCE_ID ...]",
    call. = FALSE
  )
}

manifest_path <- "data/metadata/file_manifest.csv"
manifest <- utils::read.csv(
  manifest_path,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)
if (!"sha256" %in% names(manifest)) manifest$sha256 <- NA_character_

unknown <- setdiff(source_ids, unique(manifest$source_id))
if (length(unknown)) {
  stop("Unknown source_id value(s): ", paste(unknown, collapse = ", "), call. = FALSE)
}

selected <- manifest$source_id %in% source_ids
paths <- manifest$relative_path[selected]
missing <- paths[!file.exists(paths)]
if (length(missing)) {
  stop(
    "Cannot pin missing source file(s):\n  - ",
    paste(missing, collapse = "\n  - "),
    call. = FALSE
  )
}
directories <- paths[dir.exists(paths)]
if (length(directories)) {
  stop(
    "Cannot assign file SHA-256 values to directory manifest row(s):\n  - ",
    paste(directories, collapse = "\n  - "),
    call. = FALSE
  )
}

manifest$expected_size_bytes[selected] <- as.numeric(file.info(paths)$size)
manifest$sha256[selected] <- sha256_files(paths)
if (!all(is_sha256(manifest$sha256[selected]))) {
  stop("Failed to compute SHA-256 for one or more selected files.", call. = FALSE)
}

utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")
message(
  "Pinned ", sum(selected), " file(s) across ", length(source_ids),
  " source family/families in ", manifest_path, "."
)
message("Run `Rscript scripts/update_checksums.R` before committing metadata changes.")
