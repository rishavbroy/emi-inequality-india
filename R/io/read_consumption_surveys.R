# Declarative metadata for household-consumption survey designs.

consumption_survey_registry_path <- function(paths = build_paths(Sys.getenv("EMI_PROJECT_ROOT", unset = "."))) {
  path_metadata(paths, "consumption_survey_registry.csv")
}

read_consumption_survey_registry_file <- function(path) {
  if (!file.exists(path)) stop("Consumption survey registry is missing: ", path, call. = FALSE)
  field_counts <- utils::count.fields(path, sep = ",", quote = "\"")
  if (!length(field_counts) || anyNA(field_counts) || any(field_counts != field_counts[[1]])) {
    stop("Consumption survey registry must be a rectangular CSV.", call. = FALSE)
  }
  out <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA"),
    row.names = NULL
  )
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
    "price_group_months", "district_identity_source", "mpce_contract", "legacy_wave",
    "household_adapter", "household_id_field", "mpce_field", "mpce_scale",
    "household_size_field", "weight_field", "state_field", "district_field",
    "sector_field", "subround_field"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Consumption survey registry is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!nrow(x)) stop("Consumption survey registry is empty.", call. = FALSE)

  required_text <- c(
    "survey_id", "survey_family", "survey_label", "schedule_variant", "analysis_role",
    "raw_path", "price_timing", "district_identity_source", "mpce_contract", "household_adapter"
  )
  adapter_fields <- c(
    "household_id_field", "mpce_field", "household_size_field", "weight_field",
    "state_field", "district_field", "sector_field", "subround_field"
  )
  text_fields <- c(required_text, adapter_fields)
  for (field in text_fields) x[[field]] <- trimws(plain_chr(x[[field]]))
  empty_text <- vapply(required_text, function(field) any(is.na(x[[field]]) | !nzchar(x[[field]])), logical(1))
  if (any(empty_text)) {
    stop("Consumption survey registry has empty required fields: ", paste(required_text[empty_text], collapse = ", "), call. = FALSE)
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
  x$mpce_scale <- suppressWarnings(as.numeric(x$mpce_scale))
  direct <- x$household_adapter %in% c("direct_mpce", "split_household_mpce")
  if (any(direct & (is.na(x$mpce_scale) | !is.finite(x$mpce_scale) | x$mpce_scale <= 0))) {
    stop("Detailed-MPCE registry rows require a positive finite mpce_scale.", call. = FALSE)
  }
  direct_fields_missing <- vapply(adapter_fields, function(field) any(direct & !nzchar(x[[field]])), logical(1))
  if (any(direct_fields_missing)) {
    stop("Detailed-MPCE registry rows have empty adapter fields: ", paste(adapter_fields[direct_fields_missing], collapse = ", "), call. = FALSE)
  }
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

consumption_adapter_fields <- function(specification) {
  spec <- validate_consumption_survey_registry(specification)
  if (nrow(spec) != 1L) stop("A single consumption survey specification is required.", call. = FALSE)
  fields <- c(
    household_id = "household_id_field",
    mpce = "mpce_field",
    household_size = "household_size_field",
    weight = "weight_field",
    state = "state_field",
    district = "district_field",
    sector = "sector_field",
    subround = "subround_field"
  )
  values <- stats::setNames(vapply(fields, function(field) {
    if (!field %in% names(spec)) return("")
    trimws(plain_chr(spec[[field]][[1]] %||% ""))
  }, character(1)), names(fields))
  values
}

validate_direct_consumption_adapter <- function(specification) {
  spec <- validate_consumption_survey_registry(specification)
  if (nrow(spec) != 1L) stop("A single consumption survey specification is required.", call. = FALSE)
  adapter <- trimws(plain_chr(spec$household_adapter[[1]] %||% ""))
  supported <- c("direct_mpce", "split_household_mpce")
  if (!adapter %in% supported) {
    stop("Consumption survey does not use a direct detailed-MPCE adapter: ", spec$survey_id[[1]], call. = FALSE)
  }
  fields <- consumption_adapter_fields(spec)
  missing <- names(fields)[!nzchar(fields)]
  if (length(missing)) {
    stop("Consumption survey adapter is missing field declarations: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  scale <- suppressWarnings(as.numeric(spec$mpce_scale[[1]]))
  if (length(scale) != 1L || !is.finite(scale) || scale <= 0) {
    stop("Consumption survey adapter requires a positive finite mpce_scale.", call. = FALSE)
  }
  spec
}

require_consumption_columns <- function(data, columns, context) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(context, " is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

canonicalize_detailed_consumption_households <- function(households, specification, mpce_data = NULL) {
  spec <- validate_direct_consumption_adapter(specification)
  adapter <- spec$household_adapter[[1]]
  fields <- consumption_adapter_fields(spec)
  hh <- safe_df(households)
  require_consumption_columns(
    hh,
    unname(fields[c("household_id", "household_size", "weight", "state", "district", "sector", "subround")]),
    paste0(spec$survey_id[[1]], " household data")
  )

  raw_household_id <- plain_chr(hh[[fields[["household_id"]]]])
  if (any(is.na(raw_household_id) | !nzchar(trimws(raw_household_id)))) {
    stop(spec$survey_id[[1]], " contains empty household identifiers.", call. = FALSE)
  }
  hh$.canonical_household_id <- canon(raw_household_id)
  hh <- collapse_identical_key_rows(
    hh, ".canonical_household_id", context = paste(spec$survey_id[[1]], "household data")
  )

  if (identical(adapter, "direct_mpce")) {
    require_consumption_columns(hh, fields[["mpce"]], paste0(spec$survey_id[[1]], " household data"))
    mpce <- num(hh[[fields[["mpce"]]]])
  } else {
    mp <- safe_df(mpce_data)
    if (!nrow(mp)) stop(spec$survey_id[[1]], " requires a separate MPCE data frame.", call. = FALSE)
    require_consumption_columns(
      mp, c(fields[["household_id"]], fields[["mpce"]]),
      paste0(spec$survey_id[[1]], " MPCE data")
    )
    raw_mpce_household_id <- plain_chr(mp[[fields[["household_id"]]]])
    if (any(is.na(raw_mpce_household_id) | !nzchar(trimws(raw_mpce_household_id)))) {
      stop(spec$survey_id[[1]], " MPCE data contain empty household identifiers.", call. = FALSE)
    }
    mp$.canonical_household_id <- canon(raw_mpce_household_id)
    mp <- collapse_identical_key_rows(
      mp, ".canonical_household_id", context = paste(spec$survey_id[[1]], " MPCE data")
    )
    if (anyDuplicated(mp$.canonical_household_id)) {
      stop(spec$survey_id[[1]], " MPCE data contain duplicate household identifiers.", call. = FALSE)
    }
    pos <- match(hh$.canonical_household_id, mp$.canonical_household_id)
    if (anyNA(pos)) {
      stop(spec$survey_id[[1]], " household and MPCE files do not have one-to-one household coverage.", call. = FALSE)
    }
    mpce <- num(mp[[fields[["mpce"]]]][pos])
  }

  mpce <- mpce * as.numeric(spec$mpce_scale[[1]])
  size <- num(hh[[fields[["household_size"]]]])
  weight <- num(hh[[fields[["weight"]]]])
  valid <- positive_finite(mpce) & positive_finite(size) & positive_finite(weight)
  if (!all(valid)) {
    stop(spec$survey_id[[1]], " contains non-positive or non-finite MPCE, household size, or survey weight.", call. = FALSE)
  }

  out <- data.frame(
    survey_id = rep(spec$survey_id[[1]], nrow(hh)),
    household_id = hh$.canonical_household_id,
    state_code_source = plain_chr(hh[[fields[["state"]]]]),
    district_code_source = plain_chr(hh[[fields[["district"]]]]),
    sector = plain_chr(hh[[fields[["sector"]]]]),
    subround = plain_chr(hh[[fields[["subround"]]]]),
    household_size = size,
    survey_weight = weight,
    nominal_mpce = mpce,
    nominal_household_consumption = mpce * size,
    mpce_contract = rep(spec$mpce_contract[[1]], nrow(hh)),
    stringsAsFactors = FALSE
  )
  missing_state <- is.na(out$state_code_source) | !nzchar(trimws(out$state_code_source))
  missing_district <- is.na(out$district_code_source) | !nzchar(trimws(out$district_code_source))
  if (any(missing_state) || any(missing_district)) {
    stop(spec$survey_id[[1]], " contains missing source geography codes.", call. = FALSE)
  }
  if (anyDuplicated(out$household_id)) {
    stop(spec$survey_id[[1]], " canonical household output contains duplicate household identifiers.", call. = FALSE)
  }
  rownames(out) <- NULL
  out
}
