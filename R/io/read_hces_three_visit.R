# Canonical HCES three-visit household-consumption reconstruction.
#
# HCES 2022-23 and 2023-24 publish questionnaire-summary rows in Level 14
# (Sections A1/B1/C1) and household-size/visit metadata in Level 15
# (Sections A2/B2/C2). This module owns release-schema differences and emits
# the same canonical household contract as the historical Schedule 1.0 readers.

hces_summary_items_path <- function(paths = build_paths(Sys.getenv("EMI_PROJECT_ROOT", unset = "."))) {
  path_metadata(paths, "hces_summary_items.csv")
}

read_hces_summary_items_file <- function(path) {
  if (!file.exists(path)) stop("HCES summary-item registry is missing: ", path, call. = FALSE)
  x <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character"
  )
  required <- c("questionnaire", "section", "item_code", "reference_days", "include_in_mpce")
  require_consumption_columns(x, required, "HCES summary-item registry")
  if (!nrow(x)) stop("HCES summary-item registry is empty.", call. = FALSE)

  x$questionnaire <- toupper(trimws(plain_chr(x$questionnaire)))
  x$section <- trimws(plain_chr(x$section))
  item_code <- suppressWarnings(as.integer(trimws(plain_chr(x$item_code))))
  if (anyNA(item_code)) {
    stop("HCES summary-item registry contains invalid item codes.", call. = FALSE)
  }
  x$item_code <- sprintf("%03d", item_code)
  x$reference_days <- suppressWarnings(as.integer(x$reference_days))
  include <- tolower(trimws(plain_chr(x$include_in_mpce)))
  if (any(!include %in% c("true", "false"))) {
    stop("HCES include_in_mpce must contain only TRUE/FALSE.", call. = FALSE)
  }
  x$include_in_mpce <- include == "true"

  if (!setequal(unique(x$questionnaire), c("F", "C", "D"))) {
    stop("HCES summary-item registry must cover questionnaires F, C, and D.", call. = FALSE)
  }
  if (any(!x$reference_days %in% c(7L, 30L, 365L))) {
    stop("HCES summary-item reference_days must be 7, 30, or 365.", call. = FALSE)
  }
  if (any(is.na(x$item_code)) || any(!nzchar(x$section))) {
    stop("HCES summary-item registry contains invalid section/item identities.", call. = FALSE)
  }
  key <- paste(x$questionnaire, x$section, x$item_code, sep = "\r")
  if (anyDuplicated(key)) {
    stop("HCES summary-item registry must be unique by questionnaire, section, and item_code.", call. = FALSE)
  }

  imputed_rent <- x$questionnaire == "C" & x$section == "11.4" & x$item_code == "539"
  if (sum(imputed_rent) != 1L || isTRUE(x$include_in_mpce[imputed_rent])) {
    stop("HCES summary-item registry must explicitly exclude imputed house/garage rent item 539.", call. = FALSE)
  }
  rownames(x) <- NULL
  x
}

validate_hces_three_visit_specification <- function(specification) {
  spec <- validate_consumption_survey_registry(specification)
  if (nrow(spec) != 1L) stop("A single consumption survey specification is required.", call. = FALSE)
  if (!identical(spec$survey_family[[1]], "hces_three_visit") ||
      !identical(spec$household_adapter[[1]], "three_questionnaire")) {
    stop("Consumption survey does not use the HCES three-questionnaire adapter: ", spec$survey_id[[1]], call. = FALSE)
  }
  spec
}

hces_release_schema <- function(specification) {
  spec <- validate_hces_three_visit_specification(specification)
  id <- spec$survey_id[[1]]

  if (identical(id, "hces_2022_23")) {
    identity <- c(
      fsu = "fsu",
      sector = "sector",
      state = "state",
      nss_region = "nss_region",
      district = "district",
      stratum = "stratum",
      sub_stratum = "sub_stratum",
      panel = "panel",
      sub_sample = "sub_sample",
      fod_sub_region = "fod_subregion",
      sample_su = "b1q1pt7",
      sample_subdivision = "b1q1pt10",
      second_stage_stratum = "b1q1pt11",
      sample_household = "b1q1pt12"
    )
    return(list(
      survey_id = id,
      identity = identity,
      questionnaire = "questionaire_no",
      level14 = c(section = "ba1b1c1_1", item_code = "ba1b1c1_2", value = "ba1b1c1_3"),
      level15 = c(section = "section", household_size = "ba2b2c2q9", multiplier = "mult"),
      visit = ""
    ))
  }

  if (identical(id, "hces_2023_24")) {
    identity <- c(
      fsu = "FSU_Serial_No",
      sector = "Sector",
      state = "State",
      nss_region = "NSS_Region",
      district = "District",
      stratum = "Stratum",
      sub_stratum = "Sub_stratum",
      panel = "Panel",
      sub_sample = "Sub_sample",
      fod_sub_region = "FOD_Sub_Region",
      sample_su = "Sample_SU_No",
      sample_subdivision = "Sample_Sub_Division_No",
      second_stage_stratum = "Second_Stage_Stratum_No",
      sample_household = "Sample_Household_No"
    )
    return(list(
      survey_id = id,
      identity = identity,
      questionnaire = "Questionnaire_No",
      level14 = c(section = "SECTION", item_code = "ITEM_CODE", value = "VALUE_RS"),
      level15 = c(section = "SECTION", household_size = "HOUSEHOLD_SIZE", multiplier = "MULTIPLIER"),
      visit = "VISIT"
    ))
  }

  stop("Unsupported HCES release schema: ", id, call. = FALSE)
}

find_hces_level_member <- function(archive, level) {
  members <- consumption_zip_csv_members(archive)
  pattern <- paste0("LEVEL\\s*-\\s*", as.integer(level), "\\b.*\\.csv$")
  hit <- members[grepl(pattern, basename(members), ignore.case = TRUE, perl = TRUE)]
  if (length(hit) != 1L) {
    stop(
      "HCES archive must contain exactly one Level ", as.integer(level),
      " CSV member; found ", length(hit), ".",
      call. = FALSE
    )
  }
  hit[[1]]
}

read_hces_zip_columns <- function(archive, member, columns) {
  need_pkg("data.table", "HCES summary-level reconstruction")
  td <- tempfile("hces-level-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(archive, files = member, exdir = td, junkpaths = TRUE, overwrite = TRUE)
  extracted <- file.path(td, basename(member))
  if (!file.exists(extracted)) stop("Could not extract HCES archive member: ", member, call. = FALSE)
  safe_df(data.table::fread(
    extracted,
    select = columns,
    colClasses = "character",
    data.table = FALSE,
    check.names = FALSE,
    showProgress = FALSE,
    na.strings = c("", "NA")
  ))
}

hces_household_identity_fields <- function() {
  c(
    "fsu", "sector", "state", "nss_region", "district", "stratum", "sub_stratum",
    "panel", "sub_sample", "fod_sub_region", "sample_su", "sample_subdivision",
    "second_stage_stratum", "sample_household"
  )
}

hces_household_id <- function(data) {
  x <- safe_df(data)
  fields <- hces_household_identity_fields()
  require_consumption_columns(x, fields, "HCES release rows")
  parts <- lapply(fields, function(field) {
    value <- trimws(plain_chr(x[[field]]))
    value[is.na(value)] <- ""
    value
  })
  names(parts) <- fields
  required <- setdiff(fields, "sample_subdivision")
  invalid <- Reduce(`|`, lapply(required, function(field) !nzchar(parts[[field]])))
  if (any(invalid)) {
    stop("HCES release rows contain incomplete household identity fields.", call. = FALSE)
  }
  do.call(paste, c(unname(parts), sep = "|"))
}

read_hces_release_level <- function(archive, specification, level) {
  schema <- hces_release_schema(specification)
  level <- as.integer(level)
  if (!level %in% c(14L, 15L)) stop("HCES reader supports only Levels 14 and 15.", call. = FALSE)

  mapping <- c(schema$identity, questionnaire = schema$questionnaire)
  if (level == 14L) {
    mapping <- c(mapping, schema$level14)
  } else {
    mapping <- c(mapping, schema$level15)
    if (nzchar(schema$visit)) mapping <- c(mapping, visit = schema$visit)
  }
  member <- find_hces_level_member(archive, level)
  raw <- read_hces_zip_columns(archive, member, unique(unname(mapping)))
  require_consumption_columns(raw, unname(mapping), paste0(schema$survey_id, " HCES Level ", level))
  names(raw)[match(unname(mapping), names(raw))] <- names(mapping)
  out <- raw[names(mapping)]
  out$household_id <- hces_household_id(out)
  out
}

hces_item_code <- function(x) {
  value <- suppressWarnings(as.integer(trimws(plain_chr(x))))
  out <- rep(NA_character_, length(value))
  ok <- !is.na(value)
  out[ok] <- sprintf("%03d", value[ok])
  out
}

hces_monthly_questionnaire_expenditure <- function(level14, summary_items) {
  need_pkg("data.table", "HCES summary-level reconstruction")
  x <- safe_df(level14)
  items <- safe_df(summary_items)
  require_consumption_columns(
    x, c("household_id", "questionnaire", "section", "item_code", "value"),
    "HCES Level 14"
  )
  require_consumption_columns(
    items, c("questionnaire", "section", "item_code", "reference_days", "include_in_mpce"),
    "HCES summary-item registry"
  )

  x$questionnaire <- toupper(trimws(plain_chr(x$questionnaire)))
  x$section <- trimws(plain_chr(x$section))
  x$item_code <- hces_item_code(x$item_code)
  value <- num(x$value)
  if (any(!x$questionnaire %in% c("F", "C", "D")) ||
      anyNA(x$item_code) || any(!is.finite(value)) || any(value < 0)) {
    stop("HCES Level 14 contains invalid questionnaire, item, or expenditure values.", call. = FALSE)
  }

  observed_key <- paste(x$questionnaire, x$section, x$item_code, sep = "\r")
  item_key <- paste(items$questionnaire, items$section, items$item_code, sep = "\r")
  pos <- match(observed_key, item_key)
  if (anyNA(pos)) {
    bad <- unique(paste(x$questionnaire[is.na(pos)], x$section[is.na(pos)], x$item_code[is.na(pos)], sep = "/"))
    stop(
      "HCES Level 14 contains unregistered summary item(s): ",
      paste(utils::head(bad, 10L), collapse = ", "),
      call. = FALSE
    )
  }

  reference_days <- items$reference_days[pos]
  include <- items$include_in_mpce[pos]
  monthly <- ifelse(include, value * 30 / reference_days, 0)
  dt <- data.table::data.table(
    household_id = x$household_id,
    questionnaire = x$questionnaire,
    monthly_expenditure = monthly
  )
  out <- dt[, list(monthly_expenditure = sum(monthly_expenditure)), by = list(household_id, questionnaire)]
  safe_df(out)
}

validate_hces_visit_rows <- function(level15, specification) {
  need_pkg("data.table", "HCES three-visit reconstruction")
  spec <- validate_hces_three_visit_specification(specification)
  x <- safe_df(level15)
  required <- c(
    "household_id", "questionnaire", "household_size", "multiplier",
    hces_household_identity_fields()
  )
  require_consumption_columns(x, required, paste0(spec$survey_id[[1]], " HCES Level 15"))

  x$questionnaire <- toupper(trimws(plain_chr(x$questionnaire)))
  x <- x[x$questionnaire %in% c("F", "C", "D"), , drop = FALSE]
  x$household_size <- num(x$household_size)
  x$multiplier <- num(x$multiplier)
  if (!nrow(x) || any(!positive_finite(x$household_size)) || any(!positive_finite(x$multiplier))) {
    stop("HCES Level 15 contains invalid household size or multiplier values.", call. = FALSE)
  }

  dt <- data.table::as.data.table(x)
  coverage <- dt[, list(
    n_rows = .N,
    n_f = sum(questionnaire == "F"),
    n_c = sum(questionnaire == "C"),
    n_d = sum(questionnaire == "D")
  ), by = household_id]
  complete <- coverage$n_rows == 3L & coverage$n_f == 1L & coverage$n_c == 1L & coverage$n_d == 1L
  if (!all(complete)) {
    stop("HCES Level 15 requires exactly one F, C, and D questionnaire row per household.", call. = FALSE)
  }

  id <- spec$survey_id[[1]]
  if (identical(id, "hces_2023_24")) {
    require_consumption_columns(x, "visit", "HCES 2023-24 Level 15")
    dt[, visit := suppressWarnings(as.integer(visit))]
    if (anyNA(dt$visit) || any(!dt$visit %in% 1:3)) {
      stop("HCES 2023-24 visit identifiers must be 1, 2, or 3.", call. = FALSE)
    }
    visits <- dt[, list(n_visit = data.table::uniqueN(visit)), by = household_id]
    if (any(visits$n_visit != 3L)) {
      stop("HCES 2023-24 requires visits 1, 2, and 3 exactly once per household.", call. = FALSE)
    }
    weight <- dt[visit == 3L, list(survey_weight = multiplier), by = household_id]
    if (anyDuplicated(weight$household_id)) {
      stop("HCES 2023-24 must have exactly one third-visit multiplier per household.", call. = FALSE)
    }
  } else if (identical(id, "hces_2022_23")) {
    weights <- dt[, list(n_weight = data.table::uniqueN(multiplier)), by = household_id]
    if (any(weights$n_weight != 1L)) {
      stop("HCES 2022-23 requires equal F/C/D multipliers because visit order is absent.", call. = FALSE)
    }
    weight <- dt[, list(survey_weight = multiplier[[1L]]), by = household_id]
  } else {
    stop("Unsupported HCES visit contract: ", id, call. = FALSE)
  }

  list(rows = safe_df(dt), weights = safe_df(weight))
}

join_hces_questionnaire_expenditure <- function(level15_rows, components) {
  need_pkg("data.table", "HCES three-visit reconstruction")
  rows <- data.table::as.data.table(safe_df(level15_rows))
  expense <- data.table::as.data.table(safe_df(components))
  require_consumption_columns(
    rows, c("household_id", "questionnaire"),
    "HCES Level 15 questionnaire rows"
  )
  require_consumption_columns(
    expense, c("household_id", "questionnaire", "monthly_expenditure"),
    "HCES Level 14 questionnaire summaries"
  )

  row_key <- paste(rows$household_id, rows$questionnaire, sep = "\r")
  expense_key <- paste(expense$household_id, expense$questionnaire, sep = "\r")
  unexpected <- !expense_key %in% row_key
  if (any(unexpected)) {
    bad <- unique(expense_key[unexpected])
    stop(
      "HCES Level 14 contains questionnaire summaries absent from Level 15: ",
      paste(utils::head(bad, 10L), collapse = ", "),
      call. = FALSE
    )
  }

  joined <- merge(
    rows, expense,
    by = c("household_id", "questionnaire"),
    all.x = TRUE, all.y = FALSE, sort = FALSE
  )
  if (nrow(joined) != nrow(rows)) {
    stop("HCES Level 14 questionnaire summaries are not unique within household.", call. = FALSE)
  }
  joined$summary_row_present <- !is.na(joined$monthly_expenditure)
  joined$monthly_expenditure[!joined$summary_row_present] <- 0
  safe_df(joined)
}

summarize_hces_summary_coverage <- function(level14, level15, specification, summary_items) {
  spec <- validate_hces_three_visit_specification(specification)
  components <- hces_monthly_questionnaire_expenditure(level14, summary_items)
  visit <- validate_hces_visit_rows(level15, spec)
  joined <- join_hces_questionnaire_expenditure(visit$rows, components)

  groups <- split(seq_len(nrow(joined)), joined$questionnaire)
  out <- safe_bind_rows(lapply(names(groups), function(questionnaire) {
    i <- groups[[questionnaire]]
    data.frame(
      survey_id = spec$survey_id[[1L]],
      questionnaire = questionnaire,
      n_households = length(i),
      n_summary_present = sum(joined$summary_row_present[i]),
      n_summary_zero_filled = sum(!joined$summary_row_present[i]),
      share_summary_zero_filled = mean(!joined$summary_row_present[i]),
      stringsAsFactors = FALSE
    )
  }))
  out[order(out$questionnaire), , drop = FALSE]
}

canonicalize_hces_three_visit <- function(level14, level15, specification, summary_items) {
  need_pkg("data.table", "HCES three-visit reconstruction")
  spec <- validate_hces_three_visit_specification(specification)
  components <- hces_monthly_questionnaire_expenditure(level14, summary_items)
  visit <- validate_hces_visit_rows(level15, spec)
  joined <- data.table::as.data.table(
    join_hces_questionnaire_expenditure(visit$rows, components)
  )
  joined[, component_mpce := monthly_expenditure / household_size]

  totals <- joined[, list(nominal_mpce = sum(component_mpce)), by = household_id]
  fdq <- joined[questionnaire == "F", list(
    household_id,
    state_code_source = state,
    district_code_source = district,
    sector,
    subround = panel,
    fsu,
    stratum,
    sub_stratum,
    household_size
  )]
  weights <- data.table::as.data.table(visit$weights)
  out <- merge(fdq, totals, by = "household_id", all = TRUE, sort = FALSE)
  out <- merge(out, weights, by = "household_id", all = TRUE, sort = FALSE)

  if (any(!positive_finite(out$nominal_mpce)) ||
      any(!positive_finite(out$household_size)) ||
      any(!positive_finite(out$survey_weight))) {
    stop("HCES canonical household reconstruction produced invalid MPCE, size, or weight.", call. = FALSE)
  }
  out$nominal_household_consumption <- out$nominal_mpce * out$household_size
  out$survey_id <- spec$survey_id[[1]]
  out$mpce_contract <- spec$mpce_contract[[1]]
  out <- safe_df(out)
  out <- out[c(
    "survey_id", "household_id", "state_code_source", "district_code_source",
    "sector", "subround", "fsu", "stratum", "sub_stratum", "household_size",
    "survey_weight", "nominal_mpce", "nominal_household_consumption", "mpce_contract"
  )]
  if (anyDuplicated(out$household_id)) {
    stop("HCES canonical household output contains duplicate household identifiers.", call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

read_registered_hces_consumption <- function(archive, specification, summary_items) {
  spec <- validate_hces_three_visit_specification(specification)
  level14 <- read_hces_release_level(archive, spec, 14L)
  level15 <- read_hces_release_level(archive, spec, 15L)
  canonicalize_hces_three_visit(level14, level15, spec, summary_items)
}
