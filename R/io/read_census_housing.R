# Census housing/living-standard tables used for longitudinal mechanism diagnostics.

census_housing_manifest_files <- function(paths, table, manifest_file = NULL, census_year = 2011L) {
  census_year <- as.integer(census_year)
  table <- toupper(trimws(plain_chr(table)))
  supported <- switch(
    as.character(census_year),
    `2001` = c("H09", "H12", "H13"),
    `2011` = c("HL07", "HL11", "HL12"),
    character()
  )
  if (length(table) != 1L || is.na(table) || !table %in% supported) {
    stop(
      sprintf("Census %d housing reader supports: %s.", census_year, paste(supported, collapse = ", ")),
      call. = FALSE
    )
  }
  census_manifest_files(paths, census_year, table, manifest_file)
}

read_census_housing_sheet <- function(path, skip) {
  need_pkg("readxl", "Census housing workbooks")
  readxl::read_excel(
    path, sheet = 1L, skip = skip, col_names = FALSE,
    col_types = "text", .name_repair = "minimal"
  )
}

validate_household_partition <- function(total, parts, label) {
  total <- num(total)
  parts <- data.frame(lapply(safe_df(parts), num), check.names = FALSE)
  values <- as.matrix(parts)
  if (any(!is.finite(total)) || any(total < 0) || any(!is.finite(values)) || any(values < 0)) {
    stop(label, " counts must be finite and nonnegative.", call. = FALSE)
  }
  if (any(rowSums(values) != total)) {
    stop(label, " categories do not sum exactly to total households.", call. = FALSE)
  }
  invisible(TRUE)
}

validate_household_subcounts <- function(total, counts, label) {
  total <- num(total)
  counts <- data.frame(lapply(safe_df(counts), num), check.names = FALSE)
  values <- as.matrix(counts)
  if (any(!is.finite(total)) || any(total < 0) || any(!is.finite(values)) || any(values < 0) ||
      any(values > total)) {
    stop(label, " counts must be finite, nonnegative, and no larger than total households.", call. = FALSE)
  }
  invisible(TRUE)
}

parse_census_h09_2001_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 13L) stop("Census 2001 H09 sheet has fewer than 13 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 2L),
    local_code = trimws(plain_chr(raw[[4L]])),
    district_name = clean_census_district_label(raw[[5L]]),
    residence = trimws(plain_chr(raw[[6L]])),
    households_total = num(raw[[7L]]),
    lighting_electricity = num(raw[[8L]]),
    lighting_kerosene = num(raw[[9L]]),
    lighting_solar = num(raw[[10L]]),
    lighting_other_oil = num(raw[[11L]]),
    lighting_other = num(raw[[12L]]),
    lighting_none = num(raw[[13L]]),
    stringsAsFactors = FALSE
  )
  keep <- out$table == "H4009" & !is.na(out$state_code) & !is.na(out$district_code) &
    out$district_code != "00" & out$local_code == "0000" & out$residence == "Total"
  out <- out[keep %in% TRUE, , drop = FALSE]
  validate_household_partition(
    out$households_total,
    out[c("lighting_electricity", "lighting_kerosene", "lighting_solar", "lighting_other_oil", "lighting_other", "lighting_none")],
    "Census 2001 H09 lighting"
  )
  rownames(out) <- NULL
  out
}

parse_census_h12_2001_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 13L) stop("Census 2001 H12 sheet has fewer than 13 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 2L),
    local_code = trimws(plain_chr(raw[[4L]])),
    district_name = clean_census_district_label(raw[[5L]]),
    residence = trimws(plain_chr(raw[[6L]])),
    water_source = trimws(plain_chr(raw[[7L]])),
    water_location = trimws(plain_chr(raw[[8L]])),
    households_total = num(raw[[9L]]),
    electricity_available = num(raw[[10L]]),
    electricity_not_available = num(raw[[11L]]),
    latrine_available = num(raw[[12L]]),
    latrine_not_available = num(raw[[13L]]),
    stringsAsFactors = FALSE
  )
  keep <- out$table == "H4912" & !is.na(out$state_code) & !is.na(out$district_code) &
    out$district_code != "00" & out$local_code == "0000" & out$residence == "Total" &
    out$water_source == "All Sources" & out$water_location == "Total"
  out <- out[keep %in% TRUE, , drop = FALSE]
  validate_household_partition(
    out$households_total, out[c("electricity_available", "electricity_not_available")],
    "Census 2001 H12 electricity"
  )
  validate_household_partition(
    out$households_total, out[c("latrine_available", "latrine_not_available")],
    "Census 2001 H12 latrine"
  )
  rownames(out) <- NULL
  out
}

parse_census_h13_2001_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 15L) stop("Census 2001 H13 sheet has fewer than 15 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 2L),
    local_code = trimws(plain_chr(raw[[4L]])),
    district_name = clean_census_district_label(raw[[5L]]),
    residence = trimws(plain_chr(raw[[6L]])),
    households_total = num(raw[[7L]]),
    banking = num(raw[[8L]]),
    radio = num(raw[[9L]]),
    television = num(raw[[10L]]),
    telephone = num(raw[[11L]]),
    bicycle = num(raw[[12L]]),
    motorcycle = num(raw[[13L]]),
    car = num(raw[[14L]]),
    none_specified_assets = num(raw[[15L]]),
    stringsAsFactors = FALSE
  )
  keep <- out$table == "H5813" & !is.na(out$state_code) & !is.na(out$district_code) &
    out$district_code != "00" & out$local_code == "0000" & out$residence == "Total"
  out <- out[keep %in% TRUE, , drop = FALSE]
  validate_household_subcounts(
    out$households_total,
    out[c("banking", "radio", "television", "telephone", "bicycle", "motorcycle", "car", "none_specified_assets")],
    "Census 2001 H13 asset"
  )
  rownames(out) <- NULL
  out
}

parse_census_hl07_2011_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 14L) stop("Census 2011 HL07 sheet has fewer than 14 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 3L),
    subdistrict_code = trimws(plain_chr(raw[[4L]])),
    town_code = trimws(plain_chr(raw[[5L]])),
    district_name = clean_census_district_label(raw[[6L]]),
    residence = trimws(plain_chr(raw[[7L]])),
    households_total = num(raw[[8L]]),
    lighting_electricity = num(raw[[9L]]),
    lighting_kerosene = num(raw[[10L]]),
    lighting_solar = num(raw[[11L]]),
    lighting_other_oil = num(raw[[12L]]),
    lighting_other = num(raw[[13L]]),
    lighting_none = num(raw[[14L]]),
    stringsAsFactors = FALSE
  )
  keep <- out$table == "HH2507" & !is.na(out$state_code) & !is.na(out$district_code) &
    out$district_code != "000" & out$subdistrict_code == "00000" & out$town_code == "000000" &
    out$residence == "Total"
  out <- out[keep %in% TRUE, , drop = FALSE]
  validate_household_partition(
    out$households_total,
    out[c("lighting_electricity", "lighting_kerosene", "lighting_solar", "lighting_other_oil", "lighting_other", "lighting_none")],
    "Census 2011 HL07 lighting"
  )
  rownames(out) <- NULL
  out
}

parse_census_hl11_2011_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 14L) stop("Census 2011 HL11 sheet has fewer than 14 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 3L),
    subdistrict_code = trimws(plain_chr(raw[[4L]])),
    town_code = trimws(plain_chr(raw[[5L]])),
    district_name = clean_census_district_label(raw[[6L]]),
    residence = trimws(plain_chr(raw[[7L]])),
    water_source = trimws(plain_chr(raw[[8L]])),
    water_location = trimws(plain_chr(raw[[9L]])),
    households_total = num(raw[[10L]]),
    electricity_latrine = num(raw[[11L]]),
    electricity_no_latrine = num(raw[[12L]]),
    no_electricity_latrine = num(raw[[13L]]),
    no_electricity_no_latrine = num(raw[[14L]]),
    stringsAsFactors = FALSE
  )
  keep <- out$table == "HH3711" & !is.na(out$state_code) & !is.na(out$district_code) &
    out$district_code != "000" & out$subdistrict_code == "00000" & out$town_code == "000000" &
    out$residence == "Total" & out$water_source == "All Sources" &
    out$water_location == "Total number of households"
  out <- out[keep %in% TRUE, , drop = FALSE]
  validate_household_partition(
    out$households_total,
    out[c("electricity_latrine", "electricity_no_latrine", "no_electricity_latrine", "no_electricity_no_latrine")],
    "Census 2011 HL11 electricity-latrine"
  )
  out$electricity_available <- out$electricity_latrine + out$electricity_no_latrine
  out$latrine_available <- out$electricity_latrine + out$no_electricity_latrine
  rownames(out) <- NULL
  out
}

parse_census_hl12_2011_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 20L) stop("Census 2011 HL12 sheet has fewer than 20 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 3L),
    subdistrict_code = trimws(plain_chr(raw[[4L]])),
    town_code = trimws(plain_chr(raw[[5L]])),
    district_name = clean_census_district_label(raw[[6L]]),
    residence = trimws(plain_chr(raw[[7L]])),
    households_total = num(raw[[8L]]),
    banking = num(raw[[9L]]),
    radio = num(raw[[10L]]),
    television = num(raw[[11L]]),
    computer_internet = num(raw[[12L]]),
    computer_no_internet = num(raw[[13L]]),
    phone_landline_only = num(raw[[14L]]),
    phone_mobile_only = num(raw[[15L]]),
    phone_both = num(raw[[16L]]),
    bicycle = num(raw[[17L]]),
    motorcycle = num(raw[[18L]]),
    car = num(raw[[19L]]),
    high_asset_bundle = num(raw[[20L]]),
    stringsAsFactors = FALSE
  )
  keep <- out$table == "HH4012" & !is.na(out$state_code) & !is.na(out$district_code) &
    out$district_code != "000" & out$subdistrict_code == "00000" & out$town_code == "000000" &
    out$residence == "Total"
  out <- out[keep %in% TRUE, , drop = FALSE]
  validate_household_subcounts(
    out$households_total,
    out[c("banking", "radio", "television", "computer_internet", "computer_no_internet",
          "phone_landline_only", "phone_mobile_only", "phone_both", "bicycle", "motorcycle", "car", "high_asset_bundle")],
    "Census 2011 HL12 asset"
  )
  out$telephone <- out$phone_landline_only + out$phone_mobile_only + out$phone_both
  out$computer <- out$computer_internet + out$computer_no_internet
  validate_household_subcounts(out$households_total, out[c("telephone", "computer")], "Census 2011 HL12 combined asset")
  rownames(out) <- NULL
  out
}

read_census_housing_district_files <- function(files, reader, label) {
  out <- safe_bind_rows(lapply(files, reader))
  if (!nrow(out) || anyDuplicated(out[c("state_code", "district_code")])) {
    stop(label, " files must yield one row per district.", call. = FALSE)
  }
  out[order(out$state_code, out$district_code), , drop = FALSE]
}

read_census_h09_2001_file <- function(path) parse_census_h09_2001_sheet(read_census_housing_sheet(path, 5L))
read_census_h12_2001_file <- function(path) parse_census_h12_2001_sheet(read_census_housing_sheet(path, 6L))
read_census_h13_2001_file <- function(path) parse_census_h13_2001_sheet(read_census_housing_sheet(path, 6L))
read_census_hl07_2011_file <- function(path) parse_census_hl07_2011_sheet(read_census_housing_sheet(path, 6L))
read_census_hl11_2011_file <- function(path) parse_census_hl11_2011_sheet(read_census_housing_sheet(path, 6L))
read_census_hl12_2011_file <- function(path) parse_census_hl12_2011_sheet(read_census_housing_sheet(path, 6L))

read_census_h09_2001_district <- function(files) read_census_housing_district_files(files, read_census_h09_2001_file, "Census H09")
read_census_h12_2001_district <- function(files) read_census_housing_district_files(files, read_census_h12_2001_file, "Census H12")
read_census_h13_2001_district <- function(files) read_census_housing_district_files(files, read_census_h13_2001_file, "Census H13")
read_census_hl07_2011_district <- function(files) read_census_housing_district_files(files, read_census_hl07_2011_file, "Census HL07")
read_census_hl11_2011_district <- function(files) read_census_housing_district_files(files, read_census_hl11_2011_file, "Census HL11")
read_census_hl12_2011_district <- function(files) read_census_housing_district_files(files, read_census_hl12_2011_file, "Census HL12")
