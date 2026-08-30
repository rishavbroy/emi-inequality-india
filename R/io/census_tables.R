# Shared helpers for manifest-driven Census table readers.

normalize_census_code <- function(x, width) {
  raw <- gsub("[^0-9]", "", plain_chr(x))
  vapply(raw, function(value) {
    if (is.na(value) || !nzchar(value)) return(NA_character_)
    sprintf(paste0("%0", width, "d"), as.integer(value))
  }, character(1))
}

clean_census_district_label <- function(x) {
  out <- sub("^District\\s*-\\s*", "", trimws(plain_chr(x)), ignore.case = TRUE)
  out <- sub("\\s*\\([0-9]+\\)\\s*$", "", out)
  out <- sub("\\s+\\*?\\s*[0-9]+\\s*$", "", out)
  out <- sub("\\s*\\*\\s*$", "", out)
  trimws(out)
}

census_manifest_files <- function(paths, census_year, table, manifest_file = NULL) {
  census_year <- as.integer(census_year)
  if (!census_year %in% c(2001L, 2011L)) {
    stop("Census table reader supports only 2001 and 2011.", call. = FALSE)
  }
  table <- toupper(trimws(plain_chr(table)))
  if (length(table) != 1L || is.na(table) || !nzchar(table)) {
    stop("Census table must be one non-empty value.", call. = FALSE)
  }
  manifest_file <- manifest_file %||% path_metadata(
    paths, sprintf("census_%d_download_manifest.tsv", census_year)
  )
  manifest <- utils::read.delim(
    manifest_file,
    sep = "\t",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character"
  )
  required <- c("table", "state_code", "relative_path", "url")
  if (!identical(names(manifest), required)) {
    stop("Unexpected Census download-manifest schema: ", manifest_file, call. = FALSE)
  }
  rows <- manifest[toupper(manifest$table) == table, , drop = FALSE]
  expected_states <- sprintf("%02d", 1:35)
  if (nrow(rows) != length(expected_states) || anyDuplicated(rows$state_code) ||
      !setequal(rows$state_code, expected_states)) {
    stop(
      sprintf(
        "Census %d %s manifest must contain one row for each state/UT code 01-35.",
        census_year, table
      ),
      call. = FALSE
    )
  }
  files <- file.path(paths$root, rows$relative_path)
  missing <- files[!file.exists(files) | file.info(files)$size <= 0]
  if (length(missing)) {
    stop(
      sprintf("Missing Census %s files. Run `make download-census-tables` before extended diagnostics.\n", table),
      paste(missing, collapse = "\n"),
      call. = FALSE
    )
  }
  files
}
