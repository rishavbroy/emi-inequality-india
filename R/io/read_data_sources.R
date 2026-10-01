# Source-level provenance catalog shared by domain registries.

data_source_catalog_path <- function(
    paths = build_paths(Sys.getenv("EMI_PROJECT_ROOT", unset = "."))) {
  path_metadata(paths, "data_sources.csv")
}

validate_data_source_catalog <- function(catalog) {
  x <- safe_df(catalog)
  required <- c(
    "source_id", "source_name", "source_type", "used_in_current_pipeline",
    "current_or_future", "local_raw_path", "source_url", "access_date",
    "license_or_terms_notes", "citation_key", "notes"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Data source catalog is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!nrow(x)) stop("Data source catalog is empty.", call. = FALSE)

  text_fields <- setdiff(required, "used_in_current_pipeline")
  for (field in text_fields) {
    x[[field]] <- trimws(plain_chr(x[[field]]))
    x[[field]][is.na(x[[field]])] <- ""
  }
  if (any(!nzchar(x$source_id)) || anyDuplicated(x$source_id)) {
    stop("Data source_id values must be non-empty and unique.", call. = FALSE)
  }
  if (any(!nzchar(x$source_name)) || any(!nzchar(x$source_type))) {
    stop("Data source catalog requires non-empty source_name and source_type values.", call. = FALSE)
  }

  active <- tolower(trimws(plain_chr(x$used_in_current_pipeline)))
  if (any(!active %in% c("true", "false"))) {
    stop("Data source used_in_current_pipeline must be true or false.", call. = FALSE)
  }
  x$used_in_current_pipeline <- active == "true"

  allowed_status <- c("current", "future", "both")
  if (any(!x$current_or_future %in% allowed_status)) {
    stop("Data source current_or_future must be current, future, or both.", call. = FALSE)
  }
  rownames(x) <- NULL
  x
}

read_data_source_catalog_file <- function(path) {
  if (!file.exists(path)) stop("Data source catalog is missing: ", path, call. = FALSE)
  validate_data_source_catalog(utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  ))
}

read_data_source_catalog <- function(
    paths = build_paths(Sys.getenv("EMI_PROJECT_ROOT", unset = "."))) {
  read_data_source_catalog_file(data_source_catalog_path(paths))
}
