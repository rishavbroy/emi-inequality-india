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
    "household_size_field", "household_size_encoding", "weight_field",
    "state_field", "district_field",
    "sector_field", "subround_field", "fsu_field", "stratum_field", "sub_stratum_field"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Consumption survey registry is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!nrow(x)) stop("Consumption survey registry is empty.", call. = FALSE)

  # Most detailed-consumption releases identify a household with one published
  # ID. A few combined-estimate releases need a posted/revised sample suffix.
  # Treat the suffix as optional metadata so generic specifications do not need
  # to manufacture an empty column.
  if (!"household_id_suffix_field" %in% names(x)) {
    x$household_id_suffix_field <- ""
  }

  required_text <- c(
    "survey_id", "survey_family", "survey_label", "schedule_variant", "analysis_role",
    "raw_path", "price_timing", "district_identity_source", "mpce_contract", "household_adapter"
  )
  adapter_fields <- c(
    "household_id_field", "mpce_field", "household_size_field", "weight_field",
    "state_field", "district_field", "sector_field", "subround_field",
    "fsu_field", "stratum_field", "sub_stratum_field"
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

  x$household_id_suffix_field <- trimws(plain_chr(x$household_id_suffix_field))
  x$household_id_suffix_field[is.na(x$household_id_suffix_field)] <- ""

  x$household_size_encoding <- trimws(plain_chr(x$household_size_encoding))
  allowed_size_encoding <- c("value", "label_numeric")
  if (any(is.na(x$household_size_encoding) |
          !x$household_size_encoding %in% allowed_size_encoding)) {
    stop(
      "Unsupported consumption household_size_encoding value.",
      call. = FALSE
    )
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

registered_consumption_price_window <- function(registry) {
  x <- validate_consumption_survey_registry(registry)
  implemented <- x$household_adapter != "legacy_schedule_pending"
  if (!any(implemented)) {
    stop("Consumption survey registry has no implemented household adapters.", call. = FALSE)
  }
  x <- x[implemented, , drop = FALSE]
  data.frame(
    start_period = as.Date(format(min(x$survey_start), "%Y-%m-01")),
    end_period = as.Date(format(max(x$survey_end), "%Y-%m-01")),
    first_survey_id = x$survey_id[which.min(x$survey_start)],
    last_survey_id = x$survey_id[which.max(x$survey_end)],
    stringsAsFactors = FALSE
  )
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
    subround = "subround_field",
    fsu = "fsu_field",
    stratum = "stratum_field",
    sub_stratum = "sub_stratum_field"
  )
  values <- stats::setNames(vapply(fields, function(field) {
    if (!field %in% names(spec)) return("")
    trimws(plain_chr(spec[[field]][[1]] %||% ""))
  }, character(1)), names(fields))
  values
}


consumption_household_id_fields <- function(specification) {
  spec <- validate_consumption_survey_registry(specification)
  if (nrow(spec) != 1L) stop("A single consumption survey specification is required.", call. = FALSE)
  fields <- consumption_adapter_fields(spec)
  suffix <- trimws(plain_chr(spec$household_id_suffix_field[[1L]] %||% ""))
  unique(c(fields[["household_id"]], suffix[nzchar(suffix)]))
}

consumption_household_id <- function(data, specification) {
  spec <- validate_consumption_survey_registry(specification)
  fields <- consumption_household_id_fields(spec)
  require_consumption_columns(data, fields, paste0(spec$survey_id[[1L]], " household identifier"))
  values <- lapply(fields, function(field) canon(plain_chr(data[[field]])))
  if (any(vapply(values, function(x) any(is.na(x) | !nzchar(x)), logical(1)))) {
    stop(spec$survey_id[[1L]], " contains empty household identifier components.", call. = FALSE)
  }
  do.call(paste, c(values, sep = "__"))
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

consumption_numeric_field <- function(x, encoding = "value", context = "field") {
  method <- trimws(plain_chr(encoding[[1L]] %||% "value"))
  if (identical(method, "value")) return(num(x))
  if (!identical(method, "label_numeric")) {
    stop("Unsupported numeric field encoding for ", context, ": ", method, call. = FALSE)
  }
  if (!inherits(x, c("haven_labelled", "haven_labelled_spss", "labelled"))) {
    stop(context, " requires distributed numeric value labels.", call. = FALSE)
  }
  need_pkg("haven", paste0(context, " numeric value labels"))
  labels <- trimws(plain_chr(haven::as_factor(x, levels = "labels")))
  values <- suppressWarnings(as.numeric(labels))
  if (any(is.na(values) | !is.finite(values))) {
    stop(context, " contains non-numeric or missing value labels.", call. = FALSE)
  }
  values
}

canonicalize_detailed_consumption_households <- function(households, specification, mpce_data = NULL) {
  spec <- validate_direct_consumption_adapter(specification)
  adapter <- spec$household_adapter[[1]]
  fields <- consumption_adapter_fields(spec)
  hh <- safe_df(households)
  require_consumption_columns(
    hh,
    unname(fields[c(
      "household_id", "household_size", "weight", "state", "district", "sector",
      "subround", "fsu", "stratum", "sub_stratum"
    )]),
    paste0(spec$survey_id[[1]], " household data")
  )

  hh$.canonical_household_id <- consumption_household_id(hh, spec)
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
  size <- consumption_numeric_field(
    hh[[fields[["household_size"]]]],
    spec$household_size_encoding[[1L]],
    paste0(spec$survey_id[[1L]], " household size")
  )
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
    fsu = plain_chr(hh[[fields[["fsu"]]]]),
    stratum = plain_chr(hh[[fields[["stratum"]]]]),
    sub_stratum = plain_chr(hh[[fields[["sub_stratum"]]]]),
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


consumption_source_directory <- function(paths, specification) {
  spec <- validate_consumption_survey_registry(specification)
  if (nrow(spec) != 1L) stop("A single consumption survey specification is required.", call. = FALSE)
  path_project(paths, spec$raw_path[[1]])
}

#' Locate the unique distributed CSV archive for a detailed consumption survey.
discover_consumption_csv_archive <- function(paths, specification) {
  directory <- consumption_source_directory(paths, specification)
  if (!dir.exists(directory)) {
    stop("Consumption survey raw directory is missing: ", directory, call. = FALSE)
  }
  archives <- list.files(directory, pattern = "\\.zip$", full.names = TRUE, recursive = FALSE)
  if (length(archives) != 1L) {
    stop(
      "Expected exactly one CSV archive in ", directory, "; found ", length(archives), ".",
      call. = FALSE
    )
  }
  normalizePath(archives[[1]], mustWork = TRUE)
}

consumption_zip_csv_members <- function(archive) {
  listing <- utils::unzip(archive, list = TRUE)$Name
  listing[grepl("\\.csv$", listing, ignore.case = TRUE)]
}

read_consumption_zip_csv <- function(archive, member, n_max = Inf) {
  need_pkg("readr", "consumption survey CSV archives")
  con <- unz(archive, member, open = "rb")
  on.exit(close(con), add = TRUE)
  readr::read_csv(
    con,
    col_types = readr::cols(.default = readr::col_character()),
    locale = readr::locale(encoding = "ISO-8859-1"),
    n_max = n_max,
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal"
  )
}

find_consumption_zip_member <- function(archive, required_columns, context) {
  members <- consumption_zip_csv_members(archive)
  if (!length(members)) stop("Consumption archive contains no CSV members: ", archive, call. = FALSE)
  hits <- vapply(members, function(member) {
    header <- names(read_consumption_zip_csv(archive, member, n_max = 0L))
    all(required_columns %in% header)
  }, logical(1))
  matched <- members[hits]
  if (length(matched) != 1L) {
    stop(
      context, " must resolve to exactly one CSV member; found ", length(matched), ".",
      call. = FALSE
    )
  }
  matched[[1]]
}

consumption_data_frames <- function(raw) {
  frames <- as_input_list(raw)
  frames <- frames[vapply(frames, inherits, logical(1), what = "data.frame")]
  if (!length(frames)) {
    stop("Registered consumption source contains no data frames.", call. = FALSE)
  }
  frames
}

find_consumption_data_frame <- function(raw, required_columns, context) {
  frames <- consumption_data_frames(raw)
  hits <- vapply(
    frames,
    function(frame) all(required_columns %in% names(frame)),
    logical(1)
  )
  matched <- frames[hits]
  if (length(matched) != 1L) {
    stop(
      context, " must resolve to exactly one data frame; found ",
      length(matched), ".", call. = FALSE
    )
  }
  matched[[1L]]
}

#' Canonicalize one registered detailed-consumption source already read through
#' the raw-file manifest (e.g. an SPSS release read by haven).
read_registered_detailed_consumption_frames <- function(raw, specification) {
  spec <- validate_direct_consumption_adapter(specification)
  fields <- consumption_adapter_fields(spec)
  household_fields <- unique(c(
    consumption_household_id_fields(spec),
    unname(fields[c(
      "household_size", "weight", "state", "district", "sector",
      "subround", "fsu", "stratum", "sub_stratum"
    )])
  ))
  if (identical(spec$household_adapter[[1L]], "direct_mpce")) {
    household_fields <- unique(c(household_fields, fields[["mpce"]]))
  }
  households <- find_consumption_data_frame(
    raw, household_fields, paste0(spec$survey_id[[1L]], " household source")
  )

  mpce_data <- NULL
  if (identical(spec$household_adapter[[1L]], "split_household_mpce")) {
    mpce_data <- find_consumption_data_frame(
      raw,
      c(fields[["household_id"]], fields[["mpce"]]),
      paste0(spec$survey_id[[1L]], " MPCE source")
    )
  }
  canonicalize_detailed_consumption_households(households, spec, mpce_data)
}

#' Read and canonicalize one registered legacy detailed-consumption archive.
read_registered_detailed_consumption <- function(archive, specification) {
  spec <- validate_direct_consumption_adapter(specification)
  fields <- consumption_adapter_fields(spec)
  household_fields <- unique(c(
    consumption_household_id_fields(spec),
    unname(fields[c(
      "household_size", "weight", "state", "district", "sector",
      "subround", "fsu", "stratum", "sub_stratum"
    )])
  ))
  if (identical(spec$household_adapter[[1]], "direct_mpce")) {
    household_fields <- unique(c(household_fields, fields[["mpce"]]))
  }
  household_member <- find_consumption_zip_member(
    archive, household_fields, paste0(spec$survey_id[[1]], " household source")
  )
  households <- read_consumption_zip_csv(archive, household_member)

  mpce_data <- NULL
  if (identical(spec$household_adapter[[1]], "split_household_mpce")) {
    mpce_member <- find_consumption_zip_member(
      archive,
      c(fields[["household_id"]], fields[["mpce"]]),
      paste0(spec$survey_id[[1]], " MPCE source")
    )
    mpce_data <- read_consumption_zip_csv(archive, mpce_member)
  }
  canonicalize_detailed_consumption_households(households, spec, mpce_data)
}
