# Census migration tables used for baseline sorting checks and post-treatment mechanisms.

census_migration_manifest_files <- function(paths, census_year, table, manifest_file = NULL) {
  table <- toupper(trimws(plain_chr(table)))
  if (length(table) != 1L || is.na(table) || !table %in% c("D02", "D03")) {
    stop("Census migration reader currently supports D02 and D03.", call. = FALSE)
  }
  if (as.integer(census_year) == 2001L && identical(table, "D03")) {
    stop(
      "Census 2001 D03 state workbooks do not provide district rows; do not construct district reason measures from them.",
      call. = FALSE
    )
  }
  census_manifest_files(paths, census_year, table, manifest_file)
}

census_migration_sheet <- function(path, table) {
  need_pkg("readxl", "Census migration workbooks")
  expected <- sub("^D([0-9])([0-9])$", "D-\\1\\2", toupper(table))
  sheets <- readxl::excel_sheets(path)
  hit <- sheets[toupper(sheets) == toupper(expected)]
  if (length(hit) != 1L) {
    stop("Expected worksheet `", expected, "` in Census migration workbook: ", path, call. = FALSE)
  }
  hit[[1L]]
}

read_census_migration_sheet <- function(path, table) {
  readxl::read_excel(
    path,
    sheet = census_migration_sheet(path, table),
    skip = 5L,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )
}

parse_census_d02_sheet <- function(raw, census_year) {
  raw <- safe_df(raw)
  census_year <- as.integer(census_year)
  district_width <- if (census_year == 2001L) 2L else if (census_year == 2011L) 3L else NA_integer_
  if (!is.finite(district_width)) stop("Census D02 parser supports only 2001 and 2011.", call. = FALSE)
  if (ncol(raw) < 28L) stop("Census D02 sheet has fewer than 28 columns.", call. = FALSE)

  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], district_width),
    district_name = clean_census_district_label(raw[[4L]]),
    last_residence_area = trimws(plain_chr(raw[[5L]])),
    last_residence_type = trimws(plain_chr(raw[[6L]])),
    enumeration_sector = trimws(plain_chr(raw[[7L]])),
    migrants_total = num(raw[[8L]]),
    migrants_less_than_1_year = num(raw[[11L]]),
    migrants_1_4_years = num(raw[[14L]]),
    migrants_5_9_years = num(raw[[17L]]),
    migrants_10_19_years = num(raw[[20L]]),
    migrants_20_plus_years = num(raw[[23L]]),
    migrants_duration_not_stated = num(raw[[26L]]),
    stringsAsFactors = FALSE
  )
  expected_table <- "D0302"
  zero_district <- paste(rep("0", district_width), collapse = "")
  keep <- out$table == expected_table &
    !is.na(out$state_code) & !is.na(out$district_code) & out$district_code != zero_district &
    out$enumeration_sector == "Total" &
    out$last_residence_type %in% c("Total", "Total Population")
  out <- out[keep %in% TRUE, , drop = FALSE]
  rownames(out) <- NULL
  out
}

census_d02_origin_labels <- function() {
  c(
    total = "Total",
    within_state_outside_place = "Within the state of enumeration but outside the place of enumeration",
    within_district = "Elsewhere in the district of enumeration",
    other_district_same_state = "In other districts of the state of enumeration",
    interstate = "States in India beyond the state of enumeration",
    outside_india = "Last residence outside India"
  )
}

summarise_census_d02_district <- function(rows, census_year) {
  x <- safe_df(rows)
  labels <- census_d02_origin_labels()
  x <- x[x$last_residence_area %in% unname(labels), , drop = FALSE]
  key <- paste(x$state_code, x$district_code, x$last_residence_area, sep = "|")
  if (anyDuplicated(key)) stop("Census D02 contains duplicate district-by-origin rows.", call. = FALSE)

  districts <- unique(x[c("state_code", "district_code", "district_name")])
  groups <- split(seq_len(nrow(x)), paste(x$state_code, x$district_code, sep = "|"))
  out <- safe_bind_rows(lapply(groups, function(index) {
    part <- x[index, , drop = FALSE]
    by_label <- setNames(seq_len(nrow(part)), part$last_residence_area)
    missing <- setdiff(unname(labels), names(by_label))
    if (length(missing)) {
      stop(
        "Census D02 district is missing required origin rows: ",
        part$state_code[[1L]], "/", part$district_code[[1L]], " [",
        paste(missing, collapse = "; "), "]",
        call. = FALSE
      )
    }
    get_count <- function(id, column = "migrants_total") {
      num(part[[column]][by_label[[labels[[id]]]]])[[1L]]
    }
    total <- get_count("total")
    within_state <- get_count("within_state_outside_place")
    within_district <- get_count("within_district")
    other_district <- get_count("other_district_same_state")
    if (!isTRUE(all.equal(within_state, within_district + other_district, tolerance = 0))) {
      stop(
        "Census D02 within-state migration identity fails for district ",
        part$state_code[[1L]], "/", part$district_code[[1L]], ".",
        call. = FALSE
      )
    }
    recent <- sum(c(
      get_count("total", "migrants_less_than_1_year"),
      get_count("total", "migrants_1_4_years"),
      get_count("total", "migrants_5_9_years")
    ))
    if (!is.finite(total) || total < 0 || !is.finite(recent) || recent < 0 || recent > total) {
      stop("Census D02 contains invalid total/recent migrant counts.", call. = FALSE)
    }
    data.frame(
      census_year = as.integer(census_year),
      state_code = part$state_code[[1L]],
      district_code = part$district_code[[1L]],
      district_name = part$district_name[[1L]],
      migrants_total = total,
      migrants_recent_0_9 = recent,
      migrants_within_state_outside_place = within_state,
      migrants_within_district = within_district,
      migrants_other_district_same_state = other_district,
      migrants_interstate = get_count("interstate"),
      migrants_outside_india = get_count("outside_india"),
      stringsAsFactors = FALSE
    )
  }))
  if (nrow(out) != nrow(districts) || anyDuplicated(out[c("state_code", "district_code")])) {
    stop("Census D02 district summary is not unique by state-district.", call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

read_census_d02_file <- function(path, census_year) {
  rows <- parse_census_d02_sheet(read_census_migration_sheet(path, "D02"), census_year)
  summarise_census_d02_district(rows, census_year)
}

read_census_d02_district <- function(files, census_year) {
  out <- safe_bind_rows(lapply(files, read_census_d02_file, census_year = census_year))
  if (anyDuplicated(out[c("state_code", "district_code")])) {
    stop("Census D02 files contain duplicate state-district summaries.", call. = FALSE)
  }
  out[order(out$state_code, out$district_code), , drop = FALSE]
}

parse_census_d03_2011_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 32L) stop("Census 2011 D03 sheet has fewer than 32 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 3L),
    district_name = clean_census_district_label(raw[[4L]]),
    enumeration_sector = trimws(plain_chr(raw[[5L]])),
    duration = trimws(plain_chr(raw[[6L]])),
    last_residence_area = trimws(plain_chr(raw[[7L]])),
    last_residence_type = trimws(plain_chr(raw[[8L]])),
    migrants_total = num(raw[[9L]]),
    work_employment = num(raw[[12L]]),
    business = num(raw[[15L]]),
    education = num(raw[[18L]]),
    marriage = num(raw[[21L]]),
    moved_after_birth = num(raw[[24L]]),
    moved_with_household = num(raw[[27L]]),
    other_reason = num(raw[[30L]]),
    stringsAsFactors = FALSE
  )
  keep <- out$table == "D0603" & !is.na(out$state_code) &
    !is.na(out$district_code) & out$district_code != "000" &
    out$enumeration_sector == "Total" & out$duration == "All durations of residence" &
    out$last_residence_area == "Total" & out$last_residence_type == "Total"
  out <- out[keep %in% TRUE, , drop = FALSE]
  rownames(out) <- NULL
  out
}

summarise_census_d03_2011_district <- function(rows) {
  x <- safe_df(rows)
  if (anyDuplicated(x[c("state_code", "district_code")])) {
    stop("Census 2011 D03 contains duplicate district total rows.", call. = FALSE)
  }
  reason_cols <- c(
    "work_employment", "business", "education", "marriage",
    "moved_after_birth", "moved_with_household", "other_reason"
  )
  reason_sum <- rowSums(as.data.frame(lapply(x[reason_cols], num)), na.rm = FALSE)
  bad <- !is.finite(x$migrants_total) | x$migrants_total < 0 |
    !is.finite(reason_sum) | abs(reason_sum - x$migrants_total) > 0
  if (any(bad)) {
    stop("Census 2011 D03 reason counts do not sum exactly to total migrants.", call. = FALSE)
  }
  out <- x[c("state_code", "district_code", "district_name", "migrants_total", reason_cols)]
  out$census_year <- 2011L
  out <- out[c("census_year", setdiff(names(out), "census_year"))]
  rownames(out) <- NULL
  out
}

read_census_d03_2011_file <- function(path) {
  summarise_census_d03_2011_district(
    parse_census_d03_2011_sheet(read_census_migration_sheet(path, "D03"))
  )
}

read_census_d03_2011_district <- function(files) {
  out <- safe_bind_rows(lapply(files, read_census_d03_2011_file))
  if (anyDuplicated(out[c("state_code", "district_code")])) {
    stop("Census 2011 D03 files contain duplicate state-district summaries.", call. = FALSE)
  }
  out[order(out$state_code, out$district_code), , drop = FALSE]
}
