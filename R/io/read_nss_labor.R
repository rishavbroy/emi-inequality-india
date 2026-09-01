# Source-first readers for NSS employment/labor microdata.

nss64_design_columns <- function() {
  c(
    "key_memb", "Sector", "Sub_Round", "Sub_sample", "State_Region",
    "state", "District", "Stratum", "Sub_Stratum", "FSU", "Ss_stratum",
    "Sample_hhold_No", "wgt_combined"
  )
}

nss64_usual_activity_columns <- function() {
  c(
    nss64_design_columns(), "B4_c1", "B4_c4", "B4_c5", "B4_c7", "B4_c8",
    "B4_c9", "B4_c11", "B4_c12", "B4_c13", "B4_c14", "B4_c16", "B4_c17"
  )
}

nss64_migration_columns <- function() {
  c(
    nss64_design_columns(), "B6_c1", "B6_c2", "B6_c3", "B6_c4", "B6_c5",
    "B6_c6", "B6_c7", "B6_c8", "B6_c9", "B6_c10", "B6_c11", "B6_c13",
    "B6_c14", "B6_c15", "B6_c16"
  )
}

read_nss_sav_columns <- function(path, columns, label) {
  if (!file.exists(path)) stop("Missing ", label, " file: ", path, call. = FALSE)
  out <- haven::read_sav(path, col_select = tidyselect::all_of(columns))
  missing <- setdiff(columns, names(out))
  if (length(missing)) {
    stop(label, " is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  as.data.frame(out, stringsAsFactors = FALSE)
}


normalize_nss64_design <- function(raw, person_column, label) {
  required <- unique(c(nss64_design_columns(), person_column))
  missing <- setdiff(required, names(raw))
  if (length(missing)) stop(label, " is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)

  out <- data.frame(
    person_key = as.character(raw$key_memb),
    state_code = normalize_census_code(num(raw$state), 2L),
    district_code = normalize_census_code(num(raw$District), 2L),
    sector = num(raw$Sector),
    sub_round = num(raw$Sub_Round),
    sub_sample = num(raw$Sub_sample),
    nss_region = normalize_census_code(num(raw$State_Region), 3L),
    stratum = num(raw$Stratum),
    sub_stratum = num(raw$Sub_Stratum),
    fsu = num(raw$FSU),
    second_stage_stratum = num(raw$Ss_stratum),
    household_no = num(raw$Sample_hhold_No),
    person_no = num(raw[[person_column]]),
    survey_weight = num(raw$wgt_combined),
    stringsAsFactors = FALSE
  )

  if (any(!nzchar(out$person_key)) || anyDuplicated(out$person_key)) {
    stop(label, " person keys must be complete and unique.", call. = FALSE)
  }
  required_design <- c(
    "state_code", "district_code", "sector", "sub_round", "sub_sample",
    "nss_region", "stratum", "sub_stratum", "fsu", "second_stage_stratum",
    "household_no", "person_no", "survey_weight"
  )
  if (any(!stats::complete.cases(out[required_design]))) {
    stop(label, " has incomplete survey-design or geography fields.", call. = FALSE)
  }
  if (any(!is.finite(out$survey_weight) | out$survey_weight <= 0)) {
    stop(label, " survey weights must be finite and positive.", call. = FALSE)
  }
  out
}

read_nss64_usual_activity <- function(path) {
  raw <- read_nss_sav_columns(path, nss64_usual_activity_columns(), "NSS64 Block 4")
  out <- normalize_nss64_design(raw, "B4_c1", "NSS64 Block 4")
  out$sex <- num(raw$B4_c4)
  out$age <- num(raw$B4_c5)
  out$general_education <- num(raw$B4_c7)
  out$technical_education <- num(raw$B4_c8)
  out$usual_principal_status <- num(raw$B4_c9)
  out$usual_principal_nic2004 <- num(raw$B4_c11)
  out$usual_principal_nco2004 <- num(raw$B4_c12)
  out$has_subsidiary_activity <- num(raw$B4_c13)
  out$usual_subsidiary_status <- num(raw$B4_c14)
  out$usual_subsidiary_nic2004 <- num(raw$B4_c16)
  out$usual_subsidiary_nco2004 <- num(raw$B4_c17)
  out
}

read_nss64_migration <- function(path) {
  raw <- read_nss_sav_columns(path, nss64_migration_columns(), "NSS64 Block 6")
  out <- normalize_nss64_design(raw, "B6_c1", "NSS64 Block 6")
  out$age <- num(raw$B6_c2)
  out$stayed_away <- num(raw$B6_c3)
  out$away_spells <- num(raw$B6_c4)
  out$longest_spell_destination <- num(raw$B6_c5)
  out$longest_spell_nic2004 <- num(raw$B6_c6)
  out$enumeration_differs_last_upr <- num(raw$B6_c7)
  out$enumeration_was_upr_before <- num(raw$B6_c8)
  out$movement_nature <- num(raw$B6_c9)
  out$years_since_last_upr <- num(raw$B6_c10)
  out$last_upr_location <- num(raw$B6_c11)
  out$last_upr_state_country <- num(raw$B6_c13)
  out$usual_principal_status <- num(raw$B6_c14)
  out$usual_principal_nic2004 <- num(raw$B6_c15)
  out$reason_left_last_upr <- num(raw$B6_c16)
  out
}

read_nss64_eum_ddi_contract <- function(path) {
  doc <- xml2::read_xml(path)
  ns <- c(ddi = "http://www.icpsr.umich.edu/DDI")
  variables <- xml2::xml_find_all(doc, ".//ddi:dataDscr/ddi:var", ns)
  files <- xml2::xml_find_all(doc, ".//ddi:fileDscr", ns)
  if (!length(files) || !length(variables)) stop("NSS64 DDI is missing file or variable descriptions.", call. = FALSE)

  wanted <- list(F4 = nss64_usual_activity_columns(), F6 = nss64_migration_columns())
  rows <- lapply(names(wanted), function(file_id) {
    file_node <- files[xml2::xml_attr(files, "ID") == file_id]
    if (length(file_node) != 1L) stop("NSS64 DDI must contain exactly one ", file_id, " file description.", call. = FALSE)
    case_count <- suppressWarnings(as.numeric(trimws(xml2::xml_text(xml2::xml_find_first(
      file_node, "./ddi:fileTxt/ddi:dimensns/ddi:caseQnty", ns
    )))))
    file_vars <- xml2::xml_attr(variables, "name")[vapply(
      strsplit(trimws(xml2::xml_attr(variables, "files")), "[[:space:]]+"),
      function(ids) file_id %in% ids,
      logical(1)
    )]
    missing <- setdiff(wanted[[file_id]], file_vars)
    data.frame(
      file_id = file_id,
      case_count = case_count,
      required_variables_complete = !length(missing),
      missing_required_variables = paste(missing, collapse = ";"),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  if (any(!is.finite(out$case_count) | out$case_count <= 0)) stop("NSS64 DDI must report positive case counts.", call. = FALSE)
  if (any(!out$required_variables_complete)) {
    bad <- out[!out$required_variables_complete, , drop = FALSE]
    stop("NSS64 DDI source schema mismatch for ", bad$file_id[[1L]], ": ", bad$missing_required_variables[[1L]], call. = FALSE)
  }
  out
}
