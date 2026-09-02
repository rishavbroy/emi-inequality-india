# Official Census-1991 district tables used to validate historical controls.
# These readers construct source-level sufficient statistics only; geography
# allocation and causal specification choices remain downstream concerns.

census_1991_validation_tables <- function() c("B01S", "C02T", "C02U", "C06T", "C09T")

census_1991_state_file_codes <- function() {
  sprintf("%02d", c(2:9, 11:33))
}

census_1991_validation_manifest_files <- function(paths, table, manifest_file = NULL) {
  table <- toupper(trimws(plain_chr(table)))
  if (length(table) != 1L || is.na(table) || !table %in% census_1991_validation_tables()) {
    stop(
      "Census 1991 validation reader supports: ",
      paste(census_1991_validation_tables(), collapse = ", "), ".",
      call. = FALSE
    )
  }
  expected <- if (table == "C09T") "01" else census_1991_state_file_codes()
  census_manifest_files(
    paths, 1991L, table, manifest_file = manifest_file,
    expected_states = expected
  )
}

census_1991_validation_sheet_name <- function(table) {
  table <- toupper(trimws(plain_chr(table)))
  c(B01S = "B01T", C02T = "C02T", C02U = "C02U", C06T = "C06T", C09T = "C09T")[[table]] %||%
    stop("Unsupported Census 1991 validation table: ", table, call. = FALSE)
}

census_1991_validation_skip <- function(table) {
  table <- toupper(trimws(plain_chr(table)))
  c(B01S = 10L, C02T = 15L, C02U = 15L, C06T = 10L, C09T = 10L)[[table]] %||%
    stop("Unsupported Census 1991 validation table: ", table, call. = FALSE)
}

read_census_1991_validation_sheet <- function(path, table) {
  need_pkg("readxl", "Census 1991 validation workbooks")
  sheet <- census_1991_validation_sheet_name(table)
  available <- readxl::excel_sheets(path)
  hit <- available[toupper(available) == toupper(sheet)]
  if (length(hit) != 1L) {
    stop("Expected worksheet `", sheet, "` in Census 1991 workbook: ", path, call. = FALSE)
  }
  readxl::read_excel(
    path, sheet = hit[[1L]], skip = census_1991_validation_skip(table),
    col_names = FALSE, col_types = "text", .name_repair = "minimal"
  )
}

census_1991_numeric_row_sum <- function(raw, columns) {
  values <- as.data.frame(lapply(raw[columns], num), check.names = FALSE)
  out <- rowSums(values)
  out[!stats::complete.cases(values)] <- NA_real_
  out
}

validate_census_1991_nonnegative_counts <- function(x, fields, label) {
  x <- safe_df(x)
  for (field in fields) {
    value <- num(x[[field]])
    if (any(!is.finite(value)) || any(value < 0)) {
      stop(label, " contains invalid count field `", field, "`.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

parse_census_1991_b01s_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 21L) stop("Census 1991 B-01(S) sheet has fewer than 21 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code_1991 = normalize_census_code(raw[[2L]], 2L),
    district_code_1991 = normalize_census_code(raw[[3L]], 2L),
    district_name = clean_census_district_label(raw[[4L]]),
    residence = trimws(plain_chr(raw[[5L]])),
    age_group = toupper(trimws(plain_chr(raw[[6L]]))),
    population_b01s_1991_count = num(raw[[7L]]),
    main_workers_b01s_1991_count = num(raw[[10L]]),
    marginal_workers_b01s_1991_count = num(raw[[13L]]),
    nonworkers_b01s_1991_count = num(raw[[16L]]),
    stringsAsFactors = FALSE
  )
  keep <- out$table == "B01T" & !is.na(out$state_code_1991) &
    !is.na(out$district_code_1991) & out$district_code_1991 != "00" &
    out$residence == "Total" & out$age_group == "TOTAL"
  out <- out[keep %in% TRUE, , drop = FALSE]
  validate_census_1991_nonnegative_counts(
    out,
    c(
      "population_b01s_1991_count", "main_workers_b01s_1991_count",
      "marginal_workers_b01s_1991_count", "nonworkers_b01s_1991_count"
    ),
    "Census 1991 B-01(S)"
  )
  if (any(
    out$main_workers_b01s_1991_count + out$marginal_workers_b01s_1991_count +
      out$nonworkers_b01s_1991_count != out$population_b01s_1991_count
  )) {
    stop("Census 1991 B-01(S) worker-status counts do not exhaust published population totals.", call. = FALSE)
  }
  validate_census_1991_district_keys(out, "Census 1991 B-01(S)")
}

parse_census_1991_c02t_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 29L) stop("Census 1991 C-02 total sheet has fewer than 29 columns.", call. = FALSE)
  rows <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code_1991 = normalize_census_code(raw[[2L]], 2L),
    district_code_1991 = normalize_census_code(raw[[3L]], 2L),
    district_name = clean_census_district_label(raw[[4L]]),
    residence = trimws(plain_chr(raw[[5L]])),
    age_group = trimws(plain_chr(raw[[6L]])),
    population = num(raw[[7L]]),
    secondary_plus = census_1991_numeric_row_sum(raw, 20:29),
    stringsAsFactors = FALSE
  )
  rows <- rows[
    rows$table == "C02T" & !is.na(rows$state_code_1991) &
      !is.na(rows$district_code_1991) & rows$district_code_1991 != "00" &
      rows$residence == "Total" & rows$age_group %in% c("All ages", "0-6"),
    , drop = FALSE
  ]
  groups <- split(seq_len(nrow(rows)), paste(rows$state_code_1991, rows$district_code_1991, sep = "|"))
  out <- safe_bind_rows(lapply(groups, function(index) {
    part <- rows[index, , drop = FALSE]
    if (!setequal(part$age_group, c("All ages", "0-6")) || anyDuplicated(part$age_group)) {
      stop("Census 1991 C-02 total district lacks unique All ages and 0-6 rows.", call. = FALSE)
    }
    all <- part[part$age_group == "All ages", , drop = FALSE]
    child <- part[part$age_group == "0-6", , drop = FALSE]
    data.frame(
      state_code_1991 = all$state_code_1991,
      district_code_1991 = all$district_code_1991,
      district_name = all$district_name,
      population_c02t_1991_count = all$population,
      population_0_6_c02t_1991_count = child$population,
      population_7plus_c02t_1991_count = all$population - child$population,
      secondary_plus_c02t_1991_count = all$secondary_plus,
      stringsAsFactors = FALSE
    )
  }))
  validate_census_1991_nonnegative_counts(
    out,
    c(
      "population_c02t_1991_count", "population_0_6_c02t_1991_count",
      "population_7plus_c02t_1991_count", "secondary_plus_c02t_1991_count"
    ),
    "Census 1991 C-02 total"
  )
  if (any(out$secondary_plus_c02t_1991_count > out$population_7plus_c02t_1991_count)) {
    stop("Census 1991 C-02 secondary-plus counts exceed the age-7+ population.", call. = FALSE)
  }
  validate_census_1991_district_keys(out, "Census 1991 C-02 total")
}

parse_census_1991_c02u_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 43L) stop("Census 1991 C-02 urban sheet has fewer than 43 columns.", call. = FALSE)
  out <- data.frame(
    state_code_1991 = normalize_census_code(raw[[2L]], 2L),
    district_code_1991 = normalize_census_code(raw[[3L]], 2L),
    district_name = clean_census_district_label(raw[[4L]]),
    table = trimws(plain_chr(raw[[1L]])),
    residence = trimws(plain_chr(raw[[5L]])),
    age_group = trimws(plain_chr(raw[[6L]])),
    urban_population_c02u_1991_count = num(raw[[7L]]),
    urban_secondary_plus_c02u_1991_count = census_1991_numeric_row_sum(raw, 20:43),
    stringsAsFactors = FALSE
  )
  out <- out[
    out$table == "C02U" & !is.na(out$state_code_1991) &
      !is.na(out$district_code_1991) & out$district_code_1991 != "00" &
      out$residence == "Urban" & out$age_group == "All ages",
    , drop = FALSE
  ]
  validate_census_1991_nonnegative_counts(
    out,
    c("urban_population_c02u_1991_count", "urban_secondary_plus_c02u_1991_count"),
    "Census 1991 C-02 urban"
  )
  if (any(out$urban_secondary_plus_c02u_1991_count > out$urban_population_c02u_1991_count)) {
    stop("Census 1991 C-02 urban secondary-plus counts exceed urban population.", call. = FALSE)
  }
  validate_census_1991_district_keys(out, "Census 1991 C-02 urban")
}

census_1991_c06_age_groups <- function() {
  c(
    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
    "65-69", "70-74", "75-79", "80-84", "85-89", "90-94",
    "95-99", "100+"
  )
}

parse_census_1991_c06t_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 12L) stop("Census 1991 C-06 sheet has fewer than 12 columns.", call. = FALSE)
  rows <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code_1991 = normalize_census_code(raw[[2L]], 2L),
    district_code_1991 = normalize_census_code(raw[[3L]], 2L),
    district_name = clean_census_district_label(raw[[4L]]),
    residence = trimws(plain_chr(raw[[5L]])),
    age_group = trimws(plain_chr(raw[[6L]])),
    population = num(raw[[7L]]) + num(raw[[8L]]),
    stringsAsFactors = FALSE
  )
  required_ages <- c("All ages", census_1991_c06_age_groups())
  rows <- rows[
    rows$table == "C06T" & !is.na(rows$state_code_1991) &
      !is.na(rows$district_code_1991) & rows$district_code_1991 != "00" &
      rows$residence == "Total" & rows$age_group %in% required_ages,
    , drop = FALSE
  ]
  groups <- split(seq_len(nrow(rows)), paste(rows$state_code_1991, rows$district_code_1991, sep = "|"))
  dependent <- c("0-4", "5-9", "10-14", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+")
  working <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64")
  out <- safe_bind_rows(lapply(groups, function(index) {
    part <- rows[index, , drop = FALSE]
    if (!setequal(part$age_group, required_ages) || anyDuplicated(part$age_group)) {
      stop("Census 1991 C-06 district lacks the registered age-group grid.", call. = FALSE)
    }
    by_age <- setNames(num(part$population), part$age_group)
    data.frame(
      state_code_1991 = part$state_code_1991[[1L]],
      district_code_1991 = part$district_code_1991[[1L]],
      district_name = part$district_name[[1L]],
      population_c06t_1991_count = by_age[["All ages"]],
      dependent_population_c06t_1991_count = sum(by_age[dependent]),
      working_age_population_c06t_1991_count = sum(by_age[working]),
      stringsAsFactors = FALSE
    )
  }))
  validate_census_1991_nonnegative_counts(
    out,
    c(
      "population_c06t_1991_count", "dependent_population_c06t_1991_count",
      "working_age_population_c06t_1991_count"
    ),
    "Census 1991 C-06"
  )
  if (any(out$dependent_population_c06t_1991_count + out$working_age_population_c06t_1991_count >
          out$population_c06t_1991_count)) {
    stop("Census 1991 C-06 age aggregates exceed published population totals.", call. = FALSE)
  }
  validate_census_1991_district_keys(out, "Census 1991 C-06")
}

parse_census_1991_c09t_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 32L) stop("Census 1991 C-09 sheet has fewer than 32 columns.", call. = FALSE)
  religion_columns <- c(9L, 12L, 15L, 18L, 21L, 24L, 27L, 30L)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code_1991 = normalize_census_code(raw[[2L]], 2L),
    district_code_1991 = normalize_census_code(raw[[3L]], 2L),
    district_name = clean_census_district_label(raw[[4L]]),
    residence = trimws(plain_chr(raw[[5L]])),
    population_c09t_1991_count = num(raw[[6L]]),
    muslim_population_c09t_1991_count = num(raw[[12L]]),
    religion_population_sum_c09t_1991_count =
      census_1991_numeric_row_sum(raw, religion_columns),
    stringsAsFactors = FALSE
  )
  out <- out[
    out$table == "C09T" & !is.na(out$state_code_1991) &
      !is.na(out$district_code_1991) & out$district_code_1991 != "00" &
      out$residence == "Total",
    , drop = FALSE
  ]
  validate_census_1991_nonnegative_counts(
    out,
    c(
      "population_c09t_1991_count", "muslim_population_c09t_1991_count",
      "religion_population_sum_c09t_1991_count"
    ),
    "Census 1991 C-09"
  )
  if (any(out$muslim_population_c09t_1991_count >
          out$religion_population_sum_c09t_1991_count)) {
    stop("Census 1991 C-09 Muslim counts exceed the sum of published religion categories.", call. = FALSE)
  }
  validate_census_1991_district_keys(out, "Census 1991 C-09")
}

read_census_1991_validation_files <- function(files, table, parser) {
  if (!length(files) || any(!file.exists(files))) {
    stop("Census 1991 ", table, " validation files are missing.", call. = FALSE)
  }
  out <- safe_bind_rows(lapply(files, function(path) {
    parser(read_census_1991_validation_sheet(path, table))
  }))
  validate_census_1991_district_keys(out, paste("Census 1991", table))
}

read_census_1991_b01s <- function(files) {
  read_census_1991_validation_files(files, "B01S", parse_census_1991_b01s_sheet)
}

read_census_1991_c02t <- function(files) {
  read_census_1991_validation_files(files, "C02T", parse_census_1991_c02t_sheet)
}

read_census_1991_c02u <- function(files) {
  read_census_1991_validation_files(files, "C02U", parse_census_1991_c02u_sheet)
}

read_census_1991_c06t <- function(files) {
  read_census_1991_validation_files(files, "C06T", parse_census_1991_c06t_sheet)
}

read_census_1991_c09t <- function(files) {
  read_census_1991_validation_files(files, "C09T", parse_census_1991_c09t_sheet)
}
