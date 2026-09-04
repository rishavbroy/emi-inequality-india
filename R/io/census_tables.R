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

census_1991_keys <- function() c("state_code_1991", "district_code_1991")

validate_census_1991_district_keys <- function(x, source) {
  x <- safe_df(x)
  keys <- census_1991_keys()
  missing <- setdiff(keys, names(x))
  if (length(missing)) stop(source, " lacks standardized 1991 district keys.", call. = FALSE)
  if (any(!stats::complete.cases(x[keys]))) stop(source, " contains missing 1991 district keys.", call. = FALSE)
  if (anyDuplicated(x[keys])) stop(source, " contains duplicate 1991 district keys.", call. = FALSE)
  x
}

census_1991_district_key <- function(x) {
  x <- validate_census_1991_district_keys(x, "Census-1991 district source")
  paste(x$state_code_1991, x$district_code_1991, sep = "__")
}


read_census_download_manifest <- function(manifest_file) {
  manifest <- utils::read.delim(
    manifest_file, sep = "\t", stringsAsFactors = FALSE,
    check.names = FALSE, colClasses = "character"
  )
  required <- c("table", "state_code", "relative_path", "url")
  if (!identical(names(manifest), required)) {
    stop("Unexpected Census download-manifest schema: ", manifest_file, call. = FALSE)
  }
  manifest
}

census_manifest_files <- function(
    paths, census_year, table, manifest_file = NULL, expected_states = NULL) {
  census_year <- as.integer(census_year)
  if (!census_year %in% c(1991L, 2001L, 2011L)) {
    stop("Census table reader supports 1991, 2001, and 2011.", call. = FALSE)
  }
  table <- toupper(trimws(plain_chr(table)))
  if (length(table) != 1L || is.na(table) || !nzchar(table)) {
    stop("Census table must be one non-empty value.", call. = FALSE)
  }
  manifest_file <- manifest_file %||% path_metadata(
    paths, sprintf("census_%d_download_manifest.tsv", census_year)
  )
  manifest <- read_census_download_manifest(manifest_file)
  rows <- manifest[toupper(manifest$table) == table, , drop = FALSE]
  rows$state_code <- normalize_census_code(rows$state_code, 2L)
  if (is.null(expected_states)) {
    if (census_year == 1991L) {
      stop("Census 1991 table readers must declare their expected geographic file scope.", call. = FALSE)
    }
    expected_states <- sprintf("%02d", 1:35)
  }
  expected_states <- unique(normalize_census_code(expected_states, 2L))
  if (!length(expected_states) || anyNA(expected_states)) {
    stop("Census manifest expected-state scope must contain valid two-digit codes.", call. = FALSE)
  }
  if (nrow(rows) != length(expected_states) || anyDuplicated(rows$state_code) ||
      !setequal(rows$state_code, expected_states)) {
    stop(
      sprintf(
        "Census %d %s manifest must contain one row for each declared geographic file code: %s.",
        census_year, table, paste(expected_states, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  rows <- rows[match(expected_states, rows$state_code), , drop = FALSE]
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

# Shared reader for SHRUG Census district products distributed inside ZIP archives.
read_shrug_district_archive <- function(path, member, source = "SHRUG Census archive") {
  if (!file.exists(path)) stop("Missing ", source, ": ", path, call. = FALSE)
  listing <- utils::unzip(path, list = TRUE)$Name
  hit <- listing[basename(listing) == member]
  if (length(hit) != 1L) stop(source, " must contain exactly one ", member, ".", call. = FALSE)
  utils::read.csv(unz(path, hit[[1L]]), stringsAsFactors = FALSE, check.names = FALSE)
}
