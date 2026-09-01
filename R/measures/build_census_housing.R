# Census housing/living-standard measures on the Census-2001 analytical geography.

census_housing_common_count_columns <- function() {
  c(
    "households_total", "rooms_no_exclusive", "rooms_one", "rooms_two",
    "overcrowding_gt2_ppr_lower_bound", "water_tap", "water_well",
    "water_handpump_tubewell", "water_surface", "water_within_premises", "water_away",
    "bathroom_available", "latrine_flush_or_water_closet", "latrine_pit",
    "drainage_closed", "drainage_none", "separate_kitchen_within_house",
    "cooking_solid_fuel", "cooking_clean_fuel",
    "lighting_electricity", "lighting_kerosene", "lighting_solar",
    "lighting_other_oil", "lighting_other", "lighting_none", "latrine_available",
    "banking", "radio", "television", "telephone", "bicycle", "motorcycle", "car"
  )
}

census_housing_common_share_columns <- function() {
  c(
    "no_exclusive_room_share_households", "one_room_share_households",
    "overcrowding_gt2_ppr_lower_bound_share_households",
    "tap_water_share_households", "well_water_share_households",
    "handpump_tubewell_water_share_households", "surface_water_share_households",
    "water_within_premises_share_households", "water_away_share_households",
    "bathroom_share_households", "flush_or_water_closet_latrine_share_households",
    "pit_latrine_share_households", "closed_drainage_share_households",
    "no_drainage_share_households", "separate_kitchen_within_house_share_households",
    "solid_cooking_fuel_share_households", "clean_cooking_fuel_share_households",
    "electricity_share_households", "kerosene_lighting_share_households",
    "solar_lighting_share_households", "no_lighting_share_households",
    "latrine_share_households", "banking_share_households", "radio_share_households",
    "television_share_households", "telephone_share_households", "bicycle_share_households",
    "motorcycle_share_households", "car_share_households"
  )
}

census_housing_validation_row <- function(
    left, right, left_column, right_column, label, check, allow_right_subset = FALSE) {
  if (isTRUE(allow_right_subset)) {
    out <- validate_census_subset_count(left, right, left_column, right_column, label)
  } else {
    strict <- validate_census_matching_count(left, right, left_column, right_column, label)
    out <- data.frame(
      n_reference_districts = strict$n_districts,
      n_source_districts = strict$n_districts,
      n_overlap_districts = strict$n_districts,
      max_abs_difference = strict$max_abs_difference,
      stringsAsFactors = FALSE
    )
  }
  out$check <- check
  out
}

validate_census_housing_sources <- function(
    h09_or_hl07, h12_or_hl11, h13_or_hl12, year,
    h05_or_hl04 = NULL, h08_or_hl06 = NULL,
    h10_or_hl08 = NULL, hl09 = NULL, h11_or_hl10 = NULL, hl13 = NULL) {
  year <- as.integer(year)
  light_label <- if (year == 2001L) "H09" else "HL07"
  utility_label <- if (year == 2001L) "H12" else "HL11"
  asset_label <- if (year == 2001L) "H13" else "HL12"
  room_label <- if (year == 2001L) "H05" else "HL04"
  water_label <- if (year == 2001L) "H08" else "HL06"
  sanitation_label <- if (year == 2001L) "H10" else "HL08"
  bathing_label <- if (year == 2001L) "H10" else "HL09"
  kitchen_label <- if (year == 2001L) "H11" else "HL10"
  structure_label <- if (year == 2001L) NA_character_ else "HL13"
  utility_is_subset <- year == 2001L
  checks <- list(
    census_housing_validation_row(
      h09_or_hl07, h12_or_hl11, "households_total", "households_total",
      paste0("Census ", year, " ", light_label, "/", utility_label, " household-universe"),
      "household_total_lighting_vs_utility", utility_is_subset
    ),
    census_housing_validation_row(
      h09_or_hl07, h12_or_hl11, "lighting_electricity", "electricity_available",
      paste0("Census ", year, " ", light_label, "/", utility_label, " electricity"),
      "electricity_lighting_vs_utility", utility_is_subset
    ),
    census_housing_validation_row(
      h09_or_hl07, h13_or_hl12, "households_total", "households_total",
      paste0("Census ", year, " ", light_label, "/", asset_label, " household-universe"),
      "household_total_lighting_vs_assets", FALSE
    )
  )
  if (!is.null(h05_or_hl04)) {
    checks[[length(checks) + 1L]] <- census_housing_validation_row(
      h09_or_hl07, h05_or_hl04, "households_total", "households_total",
      paste0("Census ", year, " ", light_label, "/", room_label, " household-universe"),
      "household_total_lighting_vs_rooms", year == 2001L
    )
  }
  if (!is.null(h08_or_hl06)) {
    checks[[length(checks) + 1L]] <- census_housing_validation_row(
      h09_or_hl07, h08_or_hl06, "households_total", "households_total",
      paste0("Census ", year, " ", light_label, "/", water_label, " household-universe"),
      "household_total_lighting_vs_water", FALSE
    )
  }
  if (!is.null(h10_or_hl08)) {
    checks[[length(checks) + 1L]] <- census_housing_validation_row(
      h09_or_hl07, h10_or_hl08, "households_total", "households_total",
      paste0("Census ", year, " ", light_label, "/", sanitation_label, " household-universe"),
      "household_total_lighting_vs_sanitation", FALSE
    )
    checks[[length(checks) + 1L]] <- census_housing_validation_row(
      h10_or_hl08, h12_or_hl11, "latrine_available", "latrine_available",
      paste0("Census ", year, " ", sanitation_label, "/", utility_label, " latrine"),
      "latrine_dedicated_vs_utility", utility_is_subset
    )
  }
  if (!is.null(hl09)) {
    checks[[length(checks) + 1L]] <- census_housing_validation_row(
      h09_or_hl07, hl09, "households_total", "households_total",
      paste0("Census ", year, " ", light_label, "/", bathing_label, " household-universe"),
      "household_total_lighting_vs_bathing_drainage", FALSE
    )
  }
  if (!is.null(h11_or_hl10)) {
    checks[[length(checks) + 1L]] <- census_housing_validation_row(
      h09_or_hl07, h11_or_hl10, "households_total", "households_total",
      paste0("Census ", year, " ", light_label, "/", kitchen_label, " household-universe"),
      "household_total_lighting_vs_kitchen_fuel", FALSE
    )
  }
  if (!is.null(hl13)) {
    if (year != 2011L) stop("HL13 validation is only defined for Census 2011.", call. = FALSE)
    checks[[length(checks) + 1L]] <- census_housing_validation_row(
      h09_or_hl07, hl13, "households_total", "households_total",
      paste0("Census ", year, " ", light_label, "/", structure_label, " household-universe"),
      "household_total_lighting_vs_structure", FALSE
    )
  }
  safe_bind_rows(checks)
}

add_census_housing_shares <- function(x) {
  x <- safe_df(x)
  optional_counts <- c(
    "rooms_no_exclusive", "rooms_one", "rooms_two", "overcrowding_gt2_ppr_lower_bound",
    "water_tap", "water_well", "water_handpump_tubewell", "water_surface",
    "water_within_premises", "water_away", "bathroom_available",
    "latrine_flush_or_water_closet", "latrine_pit", "drainage_closed", "drainage_none",
    "separate_kitchen_within_house", "cooking_solid_fuel", "cooking_clean_fuel"
  )
  for (column in setdiff(optional_counts, names(x))) x[[column]] <- NA_real_
  total <- x$households_total
  x$no_exclusive_room_share_households <- safe_count_share(x$rooms_no_exclusive, total)
  x$one_room_share_households <- safe_count_share(x$rooms_one, total)
  x$overcrowding_gt2_ppr_lower_bound_share_households <- safe_count_share(
    x$overcrowding_gt2_ppr_lower_bound, total
  )
  x$tap_water_share_households <- safe_count_share(x$water_tap, total)
  x$well_water_share_households <- safe_count_share(x$water_well, total)
  x$handpump_tubewell_water_share_households <- safe_count_share(x$water_handpump_tubewell, total)
  x$surface_water_share_households <- safe_count_share(x$water_surface, total)
  x$water_within_premises_share_households <- safe_count_share(x$water_within_premises, total)
  x$water_away_share_households <- safe_count_share(x$water_away, total)
  x$bathroom_share_households <- safe_count_share(x$bathroom_available, total)
  x$flush_or_water_closet_latrine_share_households <- safe_count_share(
    x$latrine_flush_or_water_closet, total
  )
  x$pit_latrine_share_households <- safe_count_share(x$latrine_pit, total)
  x$closed_drainage_share_households <- safe_count_share(x$drainage_closed, total)
  x$no_drainage_share_households <- safe_count_share(x$drainage_none, total)
  x$separate_kitchen_within_house_share_households <- safe_count_share(
    x$separate_kitchen_within_house, total
  )
  x$solid_cooking_fuel_share_households <- safe_count_share(x$cooking_solid_fuel, total)
  x$clean_cooking_fuel_share_households <- safe_count_share(x$cooking_clean_fuel, total)
  x$electricity_share_households <- safe_count_share(x$lighting_electricity, total)
  x$kerosene_lighting_share_households <- safe_count_share(x$lighting_kerosene, total)
  x$solar_lighting_share_households <- safe_count_share(x$lighting_solar, total)
  x$no_lighting_share_households <- safe_count_share(x$lighting_none, total)
  x$latrine_share_households <- safe_count_share(x$latrine_available, total)
  x$banking_share_households <- safe_count_share(x$banking, total)
  x$radio_share_households <- safe_count_share(x$radio, total)
  x$television_share_households <- safe_count_share(x$television, total)
  x$telephone_share_households <- safe_count_share(x$telephone, total)
  x$bicycle_share_households <- safe_count_share(x$bicycle, total)
  x$motorcycle_share_households <- safe_count_share(x$motorcycle, total)
  x$car_share_households <- safe_count_share(x$car, total)
  x
}


add_census_housing_followup_shares <- function(x) {
  x <- safe_df(x)
  share_specs <- list(
    owned_share_households = "households_owned",
    rented_share_households = "households_rented",
    computer_share_households = "computer",
    internet_computer_share_households = "computer_internet",
    permanent_structure_share_households = "structure_permanent",
    semi_permanent_structure_share_households = "structure_semi_permanent",
    temporary_structure_share_households = "structure_temporary",
    nonserviceable_temporary_structure_share_households = "structure_temporary_nonserviceable"
  )
  for (share in names(share_specs)) {
    count <- share_specs[[share]]
    x[[share]] <- if (count %in% names(x)) {
      safe_count_share(x[[count]], x$households_total)
    } else {
      rep.int(NA_real_, nrow(x))
    }
  }
  x
}

build_census_2001_housing_measures <- function(
    h09, h12, h13, h05 = NULL, h08 = NULL, h10 = NULL, h11 = NULL) {
  validate_census_housing_sources(h09, h12, h13, 2001L, h05, h08, h10, NULL, h11)
  x <- safe_df(h09)[c(
    "state_code", "district_code", "district_name", "households_total",
    "lighting_electricity", "lighting_kerosene", "lighting_solar", "lighting_other_oil",
    "lighting_other", "lighting_none"
  )]
  utility <- safe_df(h12)[c("state_code", "district_code", "latrine_available")]
  assets <- safe_df(h13)[c(
    "state_code", "district_code", "banking", "radio", "television", "telephone",
    "bicycle", "motorcycle", "car"
  )]
  if (!is.null(h05)) {
    rooms <- safe_df(h05)[c(
      "state_code", "district_code", "rooms_no_exclusive", "rooms_one", "rooms_two",
      "overcrowding_gt2_ppr_lower_bound"
    )]
    x <- left_join_census_district_source(x, rooms, "Census H09", "Census H05", c("district_name", "households_total"))
  }
  if (!is.null(h08)) {
    water <- safe_df(h08)[c(
      "state_code", "district_code", "water_tap", "water_well", "water_handpump_tubewell",
      "water_surface", "water_within_premises", "water_away"
    )]
    x <- merge_census_district_sources(x, water, "Census H09/H05", "Census H08", c("district_name", "households_total"))
  }
  if (!is.null(h10)) {
    sanitation <- safe_df(h10)[c(
      "state_code", "district_code", "bathroom_available", "latrine_available",
      "latrine_flush_or_water_closet", "latrine_pit", "drainage_closed", "drainage_none"
    )]
    x <- merge_census_district_sources(
      x, sanitation, "Census housing baseline", "Census H10", c("district_name", "households_total")
    )
  } else {
    x <- left_join_census_district_source(
      x, utility, "Census housing baseline", "Census H12", c("district_name", "households_total")
    )
  }
  if (!is.null(h11)) {
    kitchen <- safe_df(h11)[c(
      "state_code", "district_code", "separate_kitchen_within_house",
      "cooking_solid_fuel", "cooking_clean_fuel"
    )]
    x <- merge_census_district_sources(
      x, kitchen, "Census housing baseline", "Census H11", c("district_name", "households_total")
    )
  }
  x <- merge_census_district_sources(x, assets, "Census housing baseline", "Census H13", c("district_name", "households_total"))
  x$target_unit_2001 <- paste0("pc2001__", x$state_code, "__", x$district_code)
  x$census_year <- rep.int(2001L, nrow(x))
  add_census_housing_shares(x)
}

build_census_2011_housing_source <- function(
    hl07, hl11, hl12, hl04 = NULL, hl06 = NULL, hl08 = NULL, hl09 = NULL, hl10 = NULL,
    hl13 = NULL) {
  validate_census_housing_sources(
    hl07, hl11, hl12, 2011L, hl04, hl06, hl08, hl09, hl10, hl13
  )
  x <- safe_df(hl07)[c(
    "state_code", "district_code", "district_name", "households_total",
    "lighting_electricity", "lighting_kerosene", "lighting_solar", "lighting_other_oil",
    "lighting_other", "lighting_none"
  )]
  utility <- safe_df(hl11)[c("state_code", "district_code", "latrine_available")]
  assets <- safe_df(hl12)[c(
    "state_code", "district_code", "banking", "radio", "television", "telephone",
    "bicycle", "motorcycle", "car", "computer", "computer_internet"
  )]
  if (!is.null(hl04)) {
    rooms <- safe_df(hl04)[c(
      "state_code", "district_code", "rooms_no_exclusive", "rooms_one", "rooms_two",
      "overcrowding_gt2_ppr_lower_bound", "households_owned", "households_rented"
    )]
    x <- merge_census_district_sources(x, rooms, "Census HL07", "Census HL04", c("district_name", "households_total"))
  }
  if (!is.null(hl06)) {
    water <- safe_df(hl06)[c(
      "state_code", "district_code", "water_tap", "water_well", "water_handpump_tubewell",
      "water_surface", "water_within_premises", "water_away"
    )]
    x <- merge_census_district_sources(x, water, "Census housing follow-up", "Census HL06", c("district_name", "households_total"))
  }
  if (!is.null(hl08)) {
    sanitation <- safe_df(hl08)[c(
      "state_code", "district_code", "latrine_available",
      "latrine_flush_or_water_closet", "latrine_pit"
    )]
    x <- merge_census_district_sources(
      x, sanitation, "Census housing follow-up", "Census HL08", c("district_name", "households_total")
    )
  } else {
    x <- merge_census_district_sources(
      x, utility, "Census housing follow-up", "Census HL11", c("district_name", "households_total")
    )
  }
  if (!is.null(hl09)) {
    bathing <- safe_df(hl09)[c(
      "state_code", "district_code", "bathroom_available", "drainage_closed", "drainage_none"
    )]
    x <- merge_census_district_sources(
      x, bathing, "Census housing follow-up", "Census HL09", c("district_name", "households_total")
    )
  }
  if (!is.null(hl10)) {
    kitchen <- safe_df(hl10)[c(
      "state_code", "district_code", "separate_kitchen_within_house",
      "cooking_solid_fuel", "cooking_clean_fuel"
    )]
    x <- merge_census_district_sources(
      x, kitchen, "Census housing follow-up", "Census HL10", c("district_name", "households_total")
    )
  }
  if (!is.null(hl13)) {
    structure <- safe_df(hl13)[c(
      "state_code", "district_code", "structure_permanent", "structure_semi_permanent",
      "structure_temporary", "structure_temporary_serviceable",
      "structure_temporary_nonserviceable", "structure_unclassifiable"
    )]
    x <- merge_census_district_sources(
      x, structure, "Census housing follow-up", "Census HL13", c("district_name", "households_total")
    )
  }
  merge_census_district_sources(x, assets, "Census housing follow-up", "Census HL12", c("district_name", "households_total"))
}

build_census_2011_housing_measures <- function(
    hl07, hl11, hl12, district_transition_2001_2011,
    hl04 = NULL, hl06 = NULL, hl08 = NULL, hl09 = NULL, hl10 = NULL, hl13 = NULL) {
  source <- build_census_2011_housing_source(
    hl07, hl11, hl12, hl04, hl06, hl08, hl09, hl10, hl13
  )
  count_cols <- c(
    intersect(census_housing_common_count_columns(), names(source)),
    "computer", "computer_internet"
  )
  if (!is.null(hl04)) count_cols <- c(count_cols, "households_owned", "households_rented")
  if (!is.null(hl13)) {
    count_cols <- c(
      count_cols, "structure_permanent", "structure_semi_permanent", "structure_temporary",
      "structure_temporary_serviceable", "structure_temporary_nonserviceable",
      "structure_unclassifiable"
    )
  }
  pooled <- harmonize_census_2011_counts_to_2001(source, district_transition_2001_2011, count_cols)
  pooled$census_year <- rep.int(2011L, nrow(pooled))
  pooled <- add_census_housing_shares(pooled)
  add_census_housing_followup_shares(pooled)
}

build_census_housing_change_measures <- function(housing_2001, housing_2011) {
  baseline <- safe_df(housing_2001)
  followup <- safe_df(housing_2011)
  common <- census_housing_common_share_columns()
  baseline <- baseline[c("target_unit_2001", common)]
  names(baseline)[-1L] <- paste0(common, "_2001")
  followup <- followup[c(
    "target_unit_2001", "census_2011_source_district_count",
    "census_2011_parent_reconstruction_complete", "households_total", common,
    "computer_share_households", "internet_computer_share_households"
  )]
  names(followup)[names(followup) == "households_total"] <- "households_total_2011"
  names(followup)[names(followup) %in% common] <- paste0(
    names(followup)[names(followup) %in% common], "_2011"
  )
  out <- merge(followup, baseline, by = "target_unit_2001", all.x = TRUE, sort = FALSE)
  if (nrow(out) != nrow(followup) || anyDuplicated(out$target_unit_2001)) {
    stop("Census housing changes require one Census-2001 baseline row per harmonized 2011 parent.", call. = FALSE)
  }
  for (variable in common) {
    out[[paste0(variable, "_change_2011_2001")]] <-
      num(out[[paste0(variable, "_2011")]]) - num(out[[paste0(variable, "_2001")]])
  }
  rownames(out) <- NULL
  out
}
