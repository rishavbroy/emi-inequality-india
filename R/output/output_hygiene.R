# Generated-output hygiene checks used by the final build audit.
#
# Exact byte equality is advisory because independently valid analyses can
# produce identical results. Schema-less CSVs are invalid because downstream
# readers cannot recover the intended table structure from an empty file.

output_hygiene_csv_files <- function(roots = "outputs") {
  roots <- unique(as.character(roots))
  roots <- roots[dir.exists(roots)]
  if (!length(roots)) return(character())
  unique(unlist(lapply(roots, function(root) {
    list.files(root, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  }), use.names = FALSE))
}

schema_less_csv_files <- function(roots = "outputs") {
  files <- output_hygiene_csv_files(roots)
  files[vapply(files, function(path) {
    header <- tryCatch(
      names(utils::read.csv(path, nrows = 0L, check.names = FALSE)),
      error = function(e) character()
    )
    !length(header) || all(!nzchar(trimws(header)))
  }, logical(1))]
}

output_hygiene_duplicate_candidates <- function(roots = "outputs") {
  files <- output_hygiene_csv_files(roots)
  empty <- data.frame(
    path_a = character(), path_b = character(), md5 = character(),
    size_bytes = numeric(), stringsAsFactors = FALSE
  )
  if (length(files) < 2L) return(empty)

  # Replication copies are intentionally byte-identical to the independently
  # produced results they verify. Validation files are also excluded because
  # several independent zero-discrepancy checks legitimately serialize to the
  # same compact table.
  normalized <- gsub("\\\\", "/", files)
  keep <- !grepl("/replication/processed/", paste0("/", normalized), fixed = TRUE) &
    !grepl("_validation\\.csv$", normalized)
  files <- files[keep]
  normalized <- normalized[keep]
  if (length(files) < 2L) return(empty)

  # Header-only tables are valid zero-row results. Equal headers do not imply
  # duplicated analytical output, so duplicate warnings require at least one
  # serialized data row.
  has_data_row <- vapply(files, function(path) {
    length(readLines(path, n = 2L, warn = FALSE)) > 1L
  }, logical(1))
  files <- files[has_data_row]
  normalized <- normalized[has_data_row]
  if (length(files) < 2L) return(empty)

  size <- file.info(files)$size
  md5 <- unname(tools::md5sum(files))
  # Restrict warnings to files in the same generated directory. Cross-directory
  # equality is common when distinct domains happen to produce the same result.
  key <- paste(dirname(normalized), size, md5, sep = "::")
  groups <- split(seq_along(files), key)
  groups <- groups[lengths(groups) > 1L]
  if (!length(groups)) return(empty)

  rows <- lapply(groups, function(index) {
    pairs <- utils::combn(index, 2L)
    data.frame(
      path_a = normalized[pairs[1L, ]],
      path_b = normalized[pairs[2L, ]],
      md5 = md5[pairs[1L, ]],
      size_bytes = as.numeric(size[pairs[1L, ]]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

write_output_hygiene_reports <- function(
    schema_less, duplicates, directory = "outputs/build") {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  schema_path <- file.path(directory, "schema_less_csvs.csv")
  duplicate_path <- file.path(directory, "duplicate_generated_csvs.csv")
  utils::write.csv(
    data.frame(path = as.character(schema_less), stringsAsFactors = FALSE),
    schema_path, row.names = FALSE
  )
  utils::write.csv(duplicates, duplicate_path, row.names = FALSE)
  c(schema_less = schema_path, duplicates = duplicate_path)
}
