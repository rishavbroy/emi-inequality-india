# Source adapters and validation contracts for Economic Census inputs.

economic_census_core_count_columns <- function() {
  c(
    "nonfarm_employment", "female_employment", "hired_employment",
    "private_employment", "informal_employment", "manufacturing_employment",
    "services_employment", "firms_total"
  )
}

validate_economic_census_source_counts <- function(source, source_label) {
  source <- safe_df(source)
  keys <- c("state_code", "district_code")
  required <- c(keys, economic_census_core_count_columns())
  missing <- setdiff(required, names(source))
  if (length(missing)) {
    stop(
      source_label, " is missing required canonical columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (any(!stats::complete.cases(source[keys])) || anyDuplicated(source[keys])) {
    stop(source_label, " must be unique by complete district keys.", call. = FALSE)
  }
  invalid <- vapply(economic_census_core_count_columns(), function(column) {
    value <- num(source[[column]])
    any(!is.finite(value) | value < 0)
  }, logical(1))
  if (any(invalid)) {
    stop(
      source_label, " has missing, non-finite, or negative core counts: ",
      paste(economic_census_core_count_columns()[invalid], collapse = ", "),
      call. = FALSE
    )
  }
  if (any(source$nonfarm_employment <= 0) || any(source$firms_total <= 0)) {
    stop(source_label, " requires positive employment and firm denominators.", call. = FALSE)
  }
  source
}

read_shrug_ec05_district <- function(path) {
  raw <- read_shrug_district_archive(
    path,
    "ec05_pc01dist.csv",
    source = "SHRUG 2005 Economic Census district archive"
  )
  required <- c(
    "pc01_state_id", "pc01_district_id",
    "ec05_emp_all", "ec05_emp_f", "ec05_emp_hired", "ec05_emp_priv",
    "ec05_emp_inf", "ec05_emp_manuf", "ec05_emp_services", "ec05_count_all"
  )
  missing <- setdiff(required, names(raw))
  if (length(missing)) {
    stop(
      "SHRUG EC05 district source is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  validate_economic_census_source_counts(
    data.frame(
      state_code = normalize_census_code(raw$pc01_state_id, 2L),
      district_code = normalize_census_code(raw$pc01_district_id, 2L),
      nonfarm_employment = num(raw$ec05_emp_all),
      female_employment = num(raw$ec05_emp_f),
      hired_employment = num(raw$ec05_emp_hired),
      private_employment = num(raw$ec05_emp_priv),
      informal_employment = num(raw$ec05_emp_inf),
      manufacturing_employment = num(raw$ec05_emp_manuf),
      services_employment = num(raw$ec05_emp_services),
      firms_total = num(raw$ec05_count_all),
      stringsAsFactors = FALSE
    ),
    "SHRUG EC05 district source"
  )
}

read_economic_census_ddi_contract <- function(path) {
  doc <- xml2::read_xml(path)
  ns <- c(ddi = "http://www.icpsr.umich.edu/DDI")
  files <- xml2::xml_find_all(doc, ".//ddi:fileDscr", ns)
  if (!length(files)) {
    stop("Economic Census DDI contains no data-file descriptions.", call. = FALSE)
  }

  variables <- xml2::xml_find_all(doc, ".//ddi:dataDscr/ddi:var", ns)
  variable_names <- toupper(trimws(xml2::xml_attr(variables, "name")))
  variable_files <- trimws(xml2::xml_attr(variables, "files"))
  required_variables <- toupper(c(
    "ST", "DT", "BACT", "NIC3", "OWN_SHIP_C",
    "M_H", "F_H", "M_NH", "F_NH", "TOTAL_WORKER", "SECTOR"
  ))

  rows <- lapply(files, function(file) {
    file_id <- xml2::xml_attr(file, "ID")
    file_name <- trimws(xml2::xml_text(xml2::xml_find_first(file, "./ddi:fileTxt/ddi:fileName", ns)))
    case_count <- suppressWarnings(as.numeric(trimws(xml2::xml_text(
      xml2::xml_find_first(file, "./ddi:fileTxt/ddi:dimensns/ddi:caseQnty", ns)
    ))))
    state_code <- sub("^.*_ST([0-9]{2})_.*$", "\\1", file_name)
    if (!grepl("^[0-9]{2}$", state_code)) state_code <- NA_character_
    file_variables <- variable_names[vapply(
      strsplit(variable_files, "[[:space:]]+"),
      function(ids) file_id %in% ids,
      logical(1)
    )]
    missing <- setdiff(required_variables, file_variables)
    data.frame(
      file_id = file_id,
      state_code = state_code,
      file_name = file_name,
      case_count = case_count,
      required_variables_complete = !length(missing),
      missing_required_variables = paste(missing, collapse = ";"),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  if (any(!stats::complete.cases(out[c("file_id", "state_code", "file_name", "case_count")]))) {
    stop("Economic Census DDI has incomplete state-file metadata.", call. = FALSE)
  }
  if (anyDuplicated(out$file_id) || anyDuplicated(out$state_code)) {
    stop("Economic Census DDI state-file descriptions must be unique.", call. = FALSE)
  }
  if (any(out$case_count <= 0)) {
    stop("Economic Census DDI state files must report positive case counts.", call. = FALSE)
  }
  if (any(!out$required_variables_complete)) {
    bad <- out[!out$required_variables_complete, c("state_code", "missing_required_variables"), drop = FALSE]
    stop(
      "Economic Census DDI state files do not share the required establishment schema; first mismatch: ",
      bad$state_code[[1L]], " [", bad$missing_required_variables[[1L]], "]",
      call. = FALSE
    )
  }
  out
}
