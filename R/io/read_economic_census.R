# Source adapters and validation contracts for Economic Census inputs.

economic_census_common_count_columns <- function() {
  c(
    "nonfarm_employment", "female_employment", "hired_employment",
    "private_employment", "manufacturing_employment", "services_employment",
    "firms_total"
  )
}

economic_census_2005_count_columns <- function() {
  c(economic_census_common_count_columns(), "informal_employment")
}

validate_economic_census_source_counts <- function(
    source,
    source_label,
    count_columns = economic_census_common_count_columns()) {
  source <- safe_df(source)
  keys <- c("state_code", "district_code")
  count_columns <- unique(plain_chr(count_columns))
  required <- c(keys, count_columns)
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
  invalid <- vapply(count_columns, function(column) {
    value <- num(source[[column]])
    any(!is.finite(value) | value < 0)
  }, logical(1))
  if (any(invalid)) {
    stop(
      source_label, " has missing, non-finite, or negative core counts: ",
      paste(count_columns[invalid], collapse = ", "),
      call. = FALSE
    )
  }
  if (any(source$nonfarm_employment <= 0) || any(source$firms_total <= 0)) {
    stop(source_label, " requires positive employment and firm denominators.", call. = FALSE)
  }
  source
}

read_shrug_economic_census_district <- function(
    path,
    member,
    source_label,
    state_column,
    district_column,
    prefix,
    district_width,
    include_informal = FALSE) {
  raw <- read_shrug_district_archive(path, member, source = source_label)
  fields <- c(
    nonfarm_employment = "emp_all",
    female_employment = "emp_f",
    hired_employment = "emp_hired",
    private_employment = "emp_priv",
    manufacturing_employment = "emp_manuf",
    services_employment = "emp_services",
    firms_total = "count_all"
  )
  if (include_informal) fields <- c(fields, informal_employment = "emp_inf")
  source_columns <- paste0(prefix, "_", unname(fields))
  required <- c(state_column, district_column, source_columns)
  missing <- setdiff(required, names(raw))
  if (length(missing)) {
    stop(
      source_label, " is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  out <- data.frame(
    state_code = normalize_census_code(raw[[state_column]], 2L),
    district_code = normalize_census_code(raw[[district_column]], district_width),
    stringsAsFactors = FALSE
  )
  for (field in names(fields)) {
    out[[field]] <- num(raw[[paste0(prefix, "_", fields[[field]])]])
  }
  validate_economic_census_source_counts(
    out,
    source_label,
    if (include_informal) economic_census_2005_count_columns() else economic_census_common_count_columns()
  )
}

read_shrug_ec05_district <- function(path) {
  read_shrug_economic_census_district(
    path = path,
    member = "ec05_pc01dist.csv",
    source_label = "SHRUG EC05 district source",
    state_column = "pc01_state_id",
    district_column = "pc01_district_id",
    prefix = "ec05",
    district_width = 2L,
    include_informal = TRUE
  )
}

read_shrug_ec13_district <- function(path) {
  read_shrug_economic_census_district(
    path = path,
    member = "ec13_pc11dist.csv",
    source_label = "SHRUG EC13 district source",
    state_column = "pc11_state_id",
    district_column = "pc11_district_id",
    prefix = "ec13",
    district_width = 3L,
    include_informal = FALSE
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

economic_census_2005_it_fwf_positions <- function() {
  readr::fwf_positions(
    start = c(1L, 4L, 6L, 32L, 33L, 37L, 56L),
    end = c(2L, 5L, 7L, 32L, 36L, 37L, 60L),
    col_names = c(
      "schedule", "state_code", "district_code",
      "activity", "nic_2004", "agri_class", "workers"
    )
  )
}

economic_census_2005_raw_members <- function(path) {
  members <- utils::unzip(path, list = TRUE)$Name
  keep <- grepl("(^|/)ec05st[0-9]{2}\\.txt$", members, ignore.case = TRUE)
  members <- members[keep]
  state_code <- sub("^.*ec05st([0-9]{2})\\.txt$", "\\1", tolower(members))
  if (length(members) != 35L || anyDuplicated(state_code) ||
      !setequal(state_code, sprintf("%02d", 1:35))) {
    stop("Fifth Economic Census archive must contain one EC05 ASCII file for each state/UT code 01-35.", call. = FALSE)
  }
  data.frame(state_code = state_code, member = members, stringsAsFactors = FALSE)
}

summarise_economic_census_2005_it_rows <- function(raw, source_label = "Fifth Economic Census 2005") {
  raw <- safe_df(raw)
  required <- c("schedule", "state_code", "district_code", "activity", "nic_2004", "agri_class", "workers")
  missing <- setdiff(required, names(raw))
  if (length(missing)) {
    stop(source_label, " is missing fixed-width fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  raw$state_code <- normalize_census_code(raw$state_code, 2L)
  raw$district_code <- normalize_census_code(raw$district_code, 2L)
  raw$workers <- num(raw$workers)
  raw$activity <- trimws(plain_chr(raw$activity))
  raw$agri_class <- trimws(plain_chr(raw$agri_class))
  raw$nic_2004 <- trimws(plain_chr(raw$nic_2004))
  raw$schedule <- trimws(plain_chr(raw$schedule))

  # Schedule already defines the rural/urban form (53/54). The published EC05
  # archive contains a single otherwise valid schedule-54 record with a noncanonical
  # sector byte, and sector is not used by this estimand. Do not duplicate that
  # redundant field in the parsing contract; retain strict validation for every field
  # that identifies geography or enters the IT baseline.
  valid_structure <- raw$schedule %in% c("53", "54") &
    !is.na(raw$state_code) & !is.na(raw$district_code) & raw$district_code != "00" &
    is.finite(raw$workers) & raw$workers >= 0
  if (any(!valid_structure)) {
    stop(source_label, " contains malformed schedule/geography/worker fields.", call. = FALSE)
  }

  # Count establishments on their major activity only. Subsidiary-activity records are
  # excluded so one establishment cannot contribute twice to the opportunity baseline.
  raw <- raw[raw$activity == "1" & raw$agri_class == "2", , drop = FALSE]
  if (!nrow(raw)) stop(source_label, " contains no major-activity non-agricultural establishments.", call. = FALSE)
  raw$is_it <- substr(raw$nic_2004, 1L, 2L) == "72"
  key <- paste(raw$state_code, raw$district_code, sep = "/")
  groups <- split(seq_len(nrow(raw)), key)
  out <- safe_bind_rows(lapply(groups, function(index) {
    part <- raw[index, , drop = FALSE]
    data.frame(
      state_code = part$state_code[[1L]],
      district_code = part$district_code[[1L]],
      nonfarm_firms_raw = nrow(part),
      nonfarm_employment_raw = sum(part$workers),
      it_firms = sum(part$is_it),
      it_employment = sum(part$workers[part$is_it]),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

read_economic_census_2005_it_member <- function(path, member) {
  need_pkg("readr", "Fifth Economic Census fixed-width microdata")
  con <- unz(path, member, open = "rb")
  on.exit(close(con), add = TRUE)
  raw <- readr::read_fwf(
    con,
    col_positions = economic_census_2005_it_fwf_positions(),
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE,
    name_repair = "minimal"
  )
  raw$workers <- num(raw$workers)
  summarise_economic_census_2005_it_rows(raw, paste0("Fifth Economic Census member ", member))
}

economic_census_2005_directory <- function(path) {
  members <- utils::unzip(path, list = TRUE)$Name
  member <- members[grepl("(^|/)Directory\\.txt$", members, ignore.case = TRUE)]
  if (length(member) != 1L) stop("Fifth Economic Census archive must contain one Directory.txt.", call. = FALSE)
  con <- unz(path, member[[1L]], open = "r")
  on.exit(close(con), add = TRUE)
  lines <- readLines(con, warn = FALSE)
  keep <- grepl("^[0-9]{4}", lines)
  lines <- lines[keep]
  if (any(nchar(lines) < 35L)) {
    stop("Fifth Economic Census district directory contains malformed fixed-width rows.", call. = FALSE)
  }
  out <- data.frame(
    state_code = substr(lines, 1L, 2L),
    district_code = substr(lines, 3L, 4L),
    # Directory.txt is fixed width: columns 5--34 are the state/UT name and
    # the district-name field begins at column 35. Keep the terminal repeated
    # two-digit district code out of the name used for geography matching.
    district_name = trimws(sub(
      "[[:space:]]+[0-9]{2}[[:space:]]*$", "", substr(lines, 35L, nchar(lines))
    )),
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(out[c("state_code", "district_code")])) {
    stop("Fifth Economic Census district directory must be unique by district code.", call. = FALSE)
  }
  out
}

read_economic_census_2005_it_baseline <- function(path) {
  members <- economic_census_2005_raw_members(path)
  out <- safe_bind_rows(lapply(members$member, function(member) {
    read_economic_census_2005_it_member(path, member)
  }))
  if (anyDuplicated(out[c("state_code", "district_code")])) {
    stop("Fifth Economic Census IT baseline must be unique by district.", call. = FALSE)
  }
  directory <- economic_census_2005_directory(path)
  out <- merge(out, directory, by = c("state_code", "district_code"), all.x = TRUE, sort = FALSE)
  if (any(!nzchar(trimws(out$district_name))) || any(is.na(out$district_name))) {
    stop("Fifth Economic Census IT baseline has district codes missing from Directory.txt.", call. = FALSE)
  }
  out
}
