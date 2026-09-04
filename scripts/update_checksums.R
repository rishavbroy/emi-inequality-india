# Update checksums for tracked metadata files.

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

checksums <- tools::md5sum(paths)
out <- data.frame(
  path = names(checksums),
  md5 = unname(checksums),
  stringsAsFactors = FALSE
)

dir.create("data/metadata", recursive = TRUE, showWarnings = FALSE)
utils::write.csv(out, "data/metadata/checksums.csv", row.names = FALSE, quote = TRUE)
message("Wrote data/metadata/checksums.csv with ", nrow(out), " entries.")
