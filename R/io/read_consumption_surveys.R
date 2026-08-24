# Declarative metadata for household-consumption survey designs.

consumption_survey_registry_path <- function(paths = build_paths(Sys.getenv("EMI_PROJECT_ROOT", unset = "."))) {
  path_metadata(paths, "consumption_survey_registry.csv")
}

read_consumption_survey_registry_file <- function(path) {
  if (!file.exists(path)) stop("Consumption survey registry is missing: ", path, call. = FALSE)
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
  validate_consumption_survey_registry(out)
}

read_consumption_survey_registry <- function(paths = build_paths(Sys.getenv("EMI_PROJECT_ROOT", unset = "."))) {
  read_consumption_survey_registry_file(consumption_survey_registry_path(paths))
}

validate_consumption_survey_registry <- function(registry) {
  x <- safe_df(registry)
  required <- c(
    "survey_id", "survey_family", "survey_label", "survey_start", "survey_end",
    "schedule_variant", "analysis_role", "raw_path", "price_timing",
    "price_group_months", "district_identity_source", "mpce_contract", "legacy_wave"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Consumption survey registry is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!nrow(x)) stop("Consumption survey registry is empty.", call. = FALSE)

  text_fields <- setdiff(required, c("survey_start", "survey_end", "price_group_months", "legacy_wave"))
  for (field in text_fields) x[[field]] <- trimws(plain_chr(x[[field]]))
  empty_text <- vapply(text_fields, function(field) any(is.na(x[[field]]) | !nzchar(x[[field]])), logical(1))
  if (any(empty_text)) {
    stop("Consumption survey registry has empty required fields: ", paste(text_fields[empty_text], collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(x$survey_id)) {
    stop("Consumption survey_id values must be non-empty and unique.", call. = FALSE)
  }

  x$survey_start <- as.Date(x$survey_start)
  x$survey_end <- as.Date(x$survey_end)
  if (anyNA(x$survey_start) || anyNA(x$survey_end) || any(x$survey_end < x$survey_start)) {
    stop("Consumption survey dates must be valid and ordered.", call. = FALSE)
  }
  months <- vapply(seq_len(nrow(x)), function(i) {
    length(seq(x$survey_start[[i]], x$survey_end[[i]], by = "month"))
  }, integer(1))
  if (any(months != 12L)) stop("Each registered consumption survey must span exactly 12 survey months.", call. = FALSE)

  allowed_timing <- c("quarterly_subround", "three_visit_panel")
  if (any(!x$price_timing %in% allowed_timing)) {
    stop("Unsupported consumption price_timing value.", call. = FALSE)
  }
  x$price_group_months <- suppressWarnings(as.integer(x$price_group_months))
  if (any(is.na(x$price_group_months) | x$price_group_months <= 0L)) {
    stop("price_group_months must be a positive integer.", call. = FALSE)
  }
  x$legacy_wave <- suppressWarnings(as.integer(x$legacy_wave))
  legacy <- !is.na(x$legacy_wave)
  if (anyDuplicated(x$legacy_wave[legacy])) stop("legacy_wave values must be unique when supplied.", call. = FALSE)
  rownames(x) <- NULL
  x
}

consumption_survey_spec <- function(registry, survey_id) {
  x <- validate_consumption_survey_registry(registry)
  id <- trimws(as.character(survey_id))
  hit <- which(x$survey_id == id)
  if (length(hit) != 1L) stop("Unknown consumption survey_id: ", id, call. = FALSE)
  x[hit, , drop = FALSE]
}

consumption_survey_spec_for_wave <- function(registry, wave) {
  x <- validate_consumption_survey_registry(registry)
  value <- suppressWarnings(as.integer(wave))
  hit <- which(!is.na(x$legacy_wave) & x$legacy_wave == value)
  if (length(hit) != 1L) stop("Unsupported legacy NSS price wave: ", wave, call. = FALSE)
  x[hit, , drop = FALSE]
}

survey_period_months <- function(specification) {
  spec <- validate_consumption_survey_registry(specification)
  if (nrow(spec) != 1L) stop("A single consumption survey specification is required.", call. = FALSE)
  seq(spec$survey_start[[1]], spec$survey_end[[1]], by = "month")
}
