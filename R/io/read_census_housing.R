# Census housing/living-standard tables used for longitudinal mechanism diagnostics.

census_housing_manifest_files <- function(paths, table, manifest_file = NULL, census_year = 2011L) {
  census_year <- as.integer(census_year)
  table <- toupper(trimws(plain_chr(table)))
  supported <- switch(
    as.character(census_year),
    `2001` = c("H05", "H08", "H09", "H12", "H13"),
    `2011` = c("HL04", "HL06", "HL07", "HL11", "HL12"),
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


census_room_columns <- function() {
  c("rooms_none", "rooms_one", "rooms_two", "rooms_three", "rooms_four", "rooms_five", "rooms_six_plus")
}

census_room_household_sizes <- function() {
  c("1", "2", "3", "4", "5", "6-8", "9+")
}

census_definite_overcrowding_gt2_count <- function(rows) {
  rows <- safe_df(rows)
  by_size <- setNames(seq_len(nrow(rows)), rows$household_size)
  cell <- function(size, room) num(rows[[room]][by_size[[size]]])[[1L]]
  cell("3", "rooms_one") +
    cell("4", "rooms_one") +
    cell("5", "rooms_one") + cell("5", "rooms_two") +
    cell("6-8", "rooms_one") + cell("6-8", "rooms_two") +
    cell("9+", "rooms_one") + cell("9+", "rooms_two") +
    cell("9+", "rooms_three") + cell("9+", "rooms_four")
}

summarise_census_room_rows <- function(rows, label, ownership_column = NULL) {
  rows <- safe_df(rows)
  keys <- unique(rows[c("state_code", "district_code")])
  safe_bind_rows(lapply(seq_len(nrow(keys)), function(i) {
    part <- rows[
      rows$state_code == keys$state_code[[i]] & rows$district_code == keys$district_code[[i]],
      , drop = FALSE
    ]
    if (!is.null(ownership_column)) {
      total_part <- part[part[[ownership_column]] == "Total", , drop = FALSE]
    } else {
      total_part <- part
    }
    labels <- c("All Households", census_room_household_sizes())
    if (!setequal(total_part$household_size, labels) || anyDuplicated(total_part$household_size)) {
      stop(label, " must contain exactly one total-ownership row for every published household-size category.", call. = FALSE)
    }
    by_size <- setNames(seq_len(nrow(total_part)), total_part$household_size)
    all_row <- total_part[by_size[["All Households"]], , drop = FALSE]
    detail <- total_part[match(census_room_household_sizes(), total_part$household_size), , drop = FALSE]
    rooms <- census_room_columns()
    if (sum(num(detail$households_total)) != num(all_row$households_total) ||
        any(vapply(rooms, function(column) {
          sum(num(detail[[column]])) != num(all_row[[column]])
        }, logical(1)))) {
      stop(label, " household-size rows do not exhaust the all-households room distribution.", call. = FALSE)
    }

    owned <- rented <- other <- NA_real_
    if (!is.null(ownership_column)) {
      all_hh <- part[part$household_size == "All Households", , drop = FALSE]
      ownerships <- c("Total", "Owned", "Rented", "Any Other")
      if (!setequal(all_hh[[ownership_column]], ownerships) || anyDuplicated(all_hh[[ownership_column]])) {
        stop(label, " ownership rows must contain Total, Owned, Rented, and Any Other exactly once.", call. = FALSE)
      }
      by_ownership <- setNames(seq_len(nrow(all_hh)), all_hh[[ownership_column]])
      owner_rows <- all_hh[match(c("Owned", "Rented", "Any Other"), all_hh[[ownership_column]]), , drop = FALSE]
      if (sum(num(owner_rows$households_total)) != num(all_row$households_total) ||
          any(vapply(rooms, function(column) {
            sum(num(owner_rows[[column]])) != num(all_row[[column]])
          }, logical(1)))) {
        stop(label, " ownership categories do not exhaust the total room distribution.", call. = FALSE)
      }
      owned <- num(all_hh$households_total[by_ownership[["Owned"]]])[[1L]]
      rented <- num(all_hh$households_total[by_ownership[["Rented"]]])[[1L]]
      other <- num(all_hh$households_total[by_ownership[["Any Other"]]])[[1L]]
    }

    data.frame(
      state_code = part$state_code[[1L]],
      district_code = part$district_code[[1L]],
      district_name = part$district_name[[1L]],
      households_total = num(all_row$households_total),
      rooms_no_exclusive = num(all_row$rooms_none),
      rooms_one = num(all_row$rooms_one),
      rooms_two = num(all_row$rooms_two),
      overcrowding_gt2_ppr_lower_bound = census_definite_overcrowding_gt2_count(detail),
      households_owned = owned,
      households_rented = rented,
      households_other_tenure = other,
      stringsAsFactors = FALSE
    )
  }))
}

parse_census_h05_2001_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 16L) stop("Census 2001 H05 sheet has fewer than 16 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 2L),
    local_code = trimws(plain_chr(raw[[4L]])),
    district_name = clean_census_district_label(raw[[5L]]),
    residence = trimws(plain_chr(raw[[6L]])),
    household_size = trimws(plain_chr(raw[[7L]])),
    households_total = num(raw[[8L]]),
    rooms_none = num(raw[[9L]]),
    rooms_one = num(raw[[10L]]),
    rooms_two = num(raw[[11L]]),
    rooms_three = num(raw[[12L]]),
    rooms_four = num(raw[[13L]]),
    rooms_five = num(raw[[14L]]),
    rooms_six_plus = num(raw[[15L]]),
    stringsAsFactors = FALSE
  )
  keep <- out$table == "H2205" & !is.na(out$state_code) & !is.na(out$district_code) &
    out$district_code != "00" & out$local_code == "0000" & out$residence == "Total" &
    out$household_size %in% c("All Households", census_room_household_sizes())
  out <- out[keep %in% TRUE, , drop = FALSE]
  validate_household_partition(out$households_total, out[census_room_columns()], "Census 2001 H05 rooms")
  summarise_census_room_rows(out, "Census 2001 H05")
}

parse_census_hl04_2011_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 17L) stop("Census 2011 HL04 sheet has fewer than 17 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 3L),
    subdistrict_code = trimws(plain_chr(raw[[4L]])),
    town_code = trimws(plain_chr(raw[[5L]])),
    district_name = clean_census_district_label(raw[[6L]]),
    residence = trimws(plain_chr(raw[[7L]])),
    ownership = trimws(plain_chr(raw[[8L]])),
    household_size = trimws(plain_chr(raw[[9L]])),
    households_total = num(raw[[10L]]),
    rooms_none = num(raw[[11L]]),
    rooms_one = num(raw[[12L]]),
    rooms_two = num(raw[[13L]]),
    rooms_three = num(raw[[14L]]),
    rooms_four = num(raw[[15L]]),
    rooms_five = num(raw[[16L]]),
    rooms_six_plus = num(raw[[17L]]),
    stringsAsFactors = FALSE
  )
  keep <- out$table == "HH1604" & !is.na(out$state_code) & !is.na(out$district_code) &
    out$district_code != "000" & out$subdistrict_code == "00000" & out$town_code == "000000" &
    out$residence == "Total" & out$ownership %in% c("Total", "Owned", "Rented", "Any Other") &
    out$household_size %in% c("All Households", census_room_household_sizes())
  out <- out[keep %in% TRUE, , drop = FALSE]
  validate_household_partition(out$households_total, out[census_room_columns()], "Census 2011 HL04 rooms")
  summarise_census_room_rows(out, "Census 2011 HL04", "ownership")
}

summarise_census_water_rows <- function(rows, label) {
  rows <- safe_df(rows)
  keys <- unique(rows[c("state_code", "district_code")])
  safe_bind_rows(lapply(seq_len(nrow(keys)), function(i) {
    part <- rows[
      rows$state_code == keys$state_code[[i]] & rows$district_code == keys$district_code[[i]],
      , drop = FALSE
    ]
    locations <- c("Total", "Within", "Near", "Away")
    if (!setequal(part$water_location_group, locations) || anyDuplicated(part$water_location_group)) {
      stop(label, " must contain Total, Within, Near, and Away water-location rows exactly once.", call. = FALSE)
    }
    by_location <- setNames(seq_len(nrow(part)), part$water_location_group)
    total_row <- part[by_location[["Total"]], , drop = FALSE]
    location_rows <- part[match(c("Within", "Near", "Away"), part$water_location_group), , drop = FALSE]
    source_cols <- c("water_tap", "water_well", "water_handpump", "water_tubewell",
                     "water_spring", "water_surface", "water_other")
    if (sum(num(location_rows$households_total)) != num(total_row$households_total) ||
        any(vapply(source_cols, function(column) {
          sum(num(location_rows[[column]])) != num(total_row[[column]])
        }, logical(1)))) {
      stop(label, " water-location rows do not exhaust the total source distribution.", call. = FALSE)
    }
    data.frame(
      state_code = part$state_code[[1L]],
      district_code = part$district_code[[1L]],
      district_name = part$district_name[[1L]],
      households_total = num(total_row$households_total),
      water_tap = num(total_row$water_tap),
      water_well = num(total_row$water_well),
      water_handpump_tubewell = num(total_row$water_handpump) + num(total_row$water_tubewell),
      water_surface = num(total_row$water_surface),
      water_spring = num(total_row$water_spring),
      water_other = num(total_row$water_other),
      water_within_premises = num(part$households_total[by_location[["Within"]]])[[1L]],
      water_away = num(part$households_total[by_location[["Away"]]])[[1L]],
      stringsAsFactors = FALSE
    )
  }))
}

parse_census_h08_2001_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 16L) stop("Census 2001 H08 sheet has fewer than 16 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 2L),
    local_code = trimws(plain_chr(raw[[4L]])),
    district_name = clean_census_district_label(raw[[5L]]),
    residence = trimws(plain_chr(raw[[6L]])),
    water_location = trimws(plain_chr(raw[[7L]])),
    households_total = num(raw[[8L]]),
    water_tap = num(raw[[9L]]),
    water_handpump = num(raw[[10L]]),
    water_tubewell = num(raw[[11L]]),
    water_well = num(raw[[12L]]),
    water_tank = num(raw[[13L]]),
    water_river = num(raw[[14L]]),
    water_spring = num(raw[[15L]]),
    water_other = num(raw[[16L]]),
    stringsAsFactors = FALSE
  )
  keep <- out$table == "H3708" & !is.na(out$state_code) & !is.na(out$district_code) &
    out$district_code != "00" & out$local_code == "0000" & out$residence == "Total" &
    out$water_location %in% c("Total", "Within Premises", "Near Premises", "Away")
  out <- out[keep %in% TRUE, , drop = FALSE]
  out$water_surface <- out$water_tank + out$water_river
  validate_household_partition(
    out$households_total,
    out[c("water_tap", "water_handpump", "water_tubewell", "water_well",
          "water_tank", "water_river", "water_spring", "water_other")],
    "Census 2001 H08 water source"
  )
  out$water_location_group <- c(
    Total = "Total", `Within Premises` = "Within", `Near Premises` = "Near", Away = "Away"
  )[out$water_location]
  summarise_census_water_rows(out, "Census 2001 H08")
}

parse_census_hl06_2011_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 19L) stop("Census 2011 HL06 sheet has fewer than 19 columns.", call. = FALSE)
  out <- data.frame(
    table = trimws(plain_chr(raw[[1L]])),
    state_code = normalize_census_code(raw[[2L]], 2L),
    district_code = normalize_census_code(raw[[3L]], 3L),
    subdistrict_code = trimws(plain_chr(raw[[4L]])),
    town_code = trimws(plain_chr(raw[[5L]])),
    district_name = clean_census_district_label(raw[[6L]]),
    residence = trimws(plain_chr(raw[[7L]])),
    water_location = trimws(plain_chr(raw[[8L]])),
    households_total = num(raw[[9L]]),
    water_tap_treated = num(raw[[10L]]),
    water_tap_untreated = num(raw[[11L]]),
    water_well_covered = num(raw[[12L]]),
    water_well_uncovered = num(raw[[13L]]),
    water_handpump = num(raw[[14L]]),
    water_tubewell = num(raw[[15L]]),
    water_spring = num(raw[[16L]]),
    water_river = num(raw[[17L]]),
    water_tank = num(raw[[18L]]),
    water_other = num(raw[[19L]]),
    stringsAsFactors = FALSE
  )
  keep <- out$table == "HH2206" & !is.na(out$state_code) & !is.na(out$district_code) &
    out$district_code != "000" & out$subdistrict_code == "00000" & out$town_code == "000000" &
    out$residence == "Total" &
    out$water_location %in% c("Total", "Within the premises", "Near the premises", "Away")
  out <- out[keep %in% TRUE, , drop = FALSE]
  validate_household_partition(
    out$households_total,
    out[c("water_tap_treated", "water_tap_untreated", "water_well_covered", "water_well_uncovered",
          "water_handpump", "water_tubewell", "water_spring", "water_river", "water_tank", "water_other")],
    "Census 2011 HL06 water source"
  )
  out$water_tap <- out$water_tap_treated + out$water_tap_untreated
  out$water_well <- out$water_well_covered + out$water_well_uncovered
  out$water_surface <- out$water_river + out$water_tank
  out$water_location_group <- c(
    Total = "Total", `Within the premises` = "Within", `Near the premises` = "Near", Away = "Away"
  )[out$water_location]
  summarise_census_water_rows(out, "Census 2011 HL06")
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

read_census_h05_2001_file <- function(path) parse_census_h05_2001_sheet(read_census_housing_sheet(path, 5L))
read_census_h08_2001_file <- function(path) parse_census_h08_2001_sheet(read_census_housing_sheet(path, 5L))
read_census_h09_2001_file <- function(path) parse_census_h09_2001_sheet(read_census_housing_sheet(path, 5L))
read_census_h12_2001_file <- function(path) parse_census_h12_2001_sheet(read_census_housing_sheet(path, 6L))
read_census_h13_2001_file <- function(path) parse_census_h13_2001_sheet(read_census_housing_sheet(path, 6L))
read_census_hl04_2011_file <- function(path) parse_census_hl04_2011_sheet(read_census_housing_sheet(path, 7L))
read_census_hl06_2011_file <- function(path) parse_census_hl06_2011_sheet(read_census_housing_sheet(path, 7L))
read_census_hl07_2011_file <- function(path) parse_census_hl07_2011_sheet(read_census_housing_sheet(path, 6L))
read_census_hl11_2011_file <- function(path) parse_census_hl11_2011_sheet(read_census_housing_sheet(path, 6L))
read_census_hl12_2011_file <- function(path) parse_census_hl12_2011_sheet(read_census_housing_sheet(path, 6L))

read_census_h05_2001_district <- function(files) read_census_housing_district_files(files, read_census_h05_2001_file, "Census H05")
read_census_h08_2001_district <- function(files) read_census_housing_district_files(files, read_census_h08_2001_file, "Census H08")
read_census_h09_2001_district <- function(files) read_census_housing_district_files(files, read_census_h09_2001_file, "Census H09")
read_census_h12_2001_district <- function(files) read_census_housing_district_files(files, read_census_h12_2001_file, "Census H12")
read_census_h13_2001_district <- function(files) read_census_housing_district_files(files, read_census_h13_2001_file, "Census H13")
read_census_hl04_2011_district <- function(files) read_census_housing_district_files(files, read_census_hl04_2011_file, "Census HL04")
read_census_hl06_2011_district <- function(files) read_census_housing_district_files(files, read_census_hl06_2011_file, "Census HL06")
read_census_hl07_2011_district <- function(files) read_census_housing_district_files(files, read_census_hl07_2011_file, "Census HL07")
read_census_hl11_2011_district <- function(files) read_census_housing_district_files(files, read_census_hl11_2011_file, "Census HL11")
read_census_hl12_2011_district <- function(files) read_census_housing_district_files(files, read_census_hl12_2011_file, "Census HL12")
