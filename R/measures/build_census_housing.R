# Census housing/living-standard measures on the Census-2001 analytical geography.

census_housing_common_count_columns <- function() {
  c(
    "households_total", "lighting_electricity", "lighting_kerosene", "lighting_solar",
    "lighting_other_oil", "lighting_other", "lighting_none", "latrine_available",
    "banking", "radio", "television", "telephone", "bicycle", "motorcycle", "car"
  )
}

census_housing_common_share_columns <- function() {
  c(
    "electricity_share_households", "kerosene_lighting_share_households",
    "solar_lighting_share_households", "no_lighting_share_households",
    "latrine_share_households", "banking_share_households", "radio_share_households",
    "television_share_households", "telephone_share_households", "bicycle_share_households",
    "motorcycle_share_households", "car_share_households"
  )
}

validate_census_housing_subset_count <- function(
    left, right, left_column, right_column, label) {
  left <- safe_df(left)
  right <- safe_df(right)
  keys <- c("state_code", "district_code")
  needed_left <- c(keys, left_column)
  needed_right <- c(keys, right_column)
  if (length(setdiff(needed_left, names(left))) || length(setdiff(needed_right, names(right)))) {
    stop(label, " validation is missing required source columns.", call. = FALSE)
  }
  if (anyDuplicated(left[keys]) || anyDuplicated(right[keys])) {
    stop(label, " validation requires unique source districts.", call. = FALSE)
  }
  left_key <- paste(left$state_code, left$district_code, sep = "/")
  right_key <- paste(right$state_code, right$district_code, sep = "/")
  if (!all(right_key %in% left_key)) {
    stop(label, " contains districts outside the reference table.", call. = FALSE)
  }
  joined <- merge(left[needed_left], right[needed_right], by = keys, all = FALSE, sort = TRUE)
  left_value <- num(joined[[left_column]])
  right_value <- num(joined[[right_column]])
  same <- is.finite(left_value) & is.finite(right_value) & left_value == right_value
  if (!nrow(joined) || any(!same)) {
    bad <- joined[!same, , drop = FALSE]
    detail <- if (nrow(bad)) paste0(bad$state_code[[1L]], "/", bad$district_code[[1L]]) else "no shared districts"
    stop(label, " counts disagree on overlapping districts; first mismatch: ", detail, ".", call. = FALSE)
  }
  data.frame(
    n_reference_districts = length(left_key),
    n_source_districts = length(right_key),
    n_overlap_districts = nrow(joined),
    max_abs_difference = max(abs(left_value - right_value)),
    stringsAsFactors = FALSE
  )
}

census_housing_validation_row <- function(
    left, right, left_column, right_column, label, check, allow_right_subset = FALSE) {
  if (isTRUE(allow_right_subset)) {
    out <- validate_census_housing_subset_count(left, right, left_column, right_column, label)
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

validate_census_housing_sources <- function(h09_or_hl07, h12_or_hl11, h13_or_hl12, year) {
  year <- as.integer(year)
  light_label <- if (year == 2001L) "H09" else "HL07"
  utility_label <- if (year == 2001L) "H12" else "HL11"
  asset_label <- if (year == 2001L) "H13" else "HL12"
  utility_is_subset <- year == 2001L
  safe_bind_rows(list(
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
  ))
}

add_census_housing_shares <- function(x) {
  x <- safe_df(x)
  total <- x$households_total
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

build_census_2001_housing_measures <- function(h09, h12, h13) {
  validate_census_housing_sources(h09, h12, h13, 2001L)
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
  x <- left_join_census_district_source(x, utility, "Census H09", "Census H12", c("district_name", "households_total"))
  x <- merge_census_district_sources(x, assets, "Census H09/H12", "Census H13", c("district_name", "households_total"))
  x$target_unit_2001 <- paste0("pc2001__", x$state_code, "__", x$district_code)
  x$census_year <- rep.int(2001L, nrow(x))
  add_census_housing_shares(x)
}

build_census_2011_housing_source <- function(hl07, hl11, hl12) {
  validate_census_housing_sources(hl07, hl11, hl12, 2011L)
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
  x <- merge_census_district_sources(x, utility, "Census HL07", "Census HL11", c("district_name", "households_total"))
  merge_census_district_sources(x, assets, "Census HL07/HL11", "Census HL12", c("district_name", "households_total"))
}

build_census_2011_housing_measures <- function(hl07, hl11, hl12, district_transition_2001_2011) {
  source <- build_census_2011_housing_source(hl07, hl11, hl12)
  count_cols <- c(census_housing_common_count_columns(), "computer", "computer_internet")
  pooled <- harmonize_census_2011_counts_to_2001(source, district_transition_2001_2011, count_cols)
  pooled$census_year <- rep.int(2011L, nrow(pooled))
  pooled <- add_census_housing_shares(pooled)
  pooled$computer_share_households <- safe_count_share(pooled$computer, pooled$households_total)
  pooled$internet_computer_share_households <- safe_count_share(
    pooled$computer_internet, pooled$households_total
  )
  pooled
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
