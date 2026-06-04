# Update checksums for tracked metadata and processed data files.

paths <- c(
  list.files("data/metadata", pattern = "\\.csv$", full.names = TRUE),
  list.files("data/processed", pattern = "\\.csv$", full.names = TRUE)
)

paths <- sort(unique(paths[file.exists(paths)]))
if (!length(paths)) {
  stop("No metadata or processed CSV files found for checksums.", call. = FALSE)
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
