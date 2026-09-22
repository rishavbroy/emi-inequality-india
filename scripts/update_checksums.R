# Update SHA-256 checksums for tracked metadata files.

source("R/io/utils_data_frame.R")
source("R/io/file_integrity.R")

paths <- list.files(
  "data/metadata",
  pattern = "\\.(csv|tsv)$",
  full.names = TRUE
)

paths <- sort(unique(paths[file.exists(paths)]))
paths <- setdiff(paths, "data/metadata/checksums.csv")
if (!length(paths)) {
  stop("No metadata CSV/TSV files found for checksums.", call. = FALSE)
}

out <- data.frame(
  path = paths,
  sha256 = sha256_files(paths),
  stringsAsFactors = FALSE
)

if (!all(is_sha256(out$sha256))) {
  stop("Failed to compute SHA-256 for one or more metadata files.", call. = FALSE)
}

dir.create("data/metadata", recursive = TRUE, showWarnings = FALSE)
utils::write.csv(out, "data/metadata/checksums.csv", row.names = FALSE, quote = TRUE)
message("Wrote data/metadata/checksums.csv with ", nrow(out), " entries.")
