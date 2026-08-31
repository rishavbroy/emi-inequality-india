# District migration constructs from Census D-series tables.

census_d02_count_columns <- function() {
  c(
    "migrants_total", "migrants_recent_0_9", "migrants_within_state_outside_place",
    "migrants_within_district", "migrants_other_district_same_state",
    "migrants_interstate", "migrants_outside_india"
  )
}

census_d03_reason_count_columns <- function() {
  census_migration_reason_columns()
}

census_d03_count_columns <- function() {
  c(
    census_d03_reason_count_columns(),
    "recent_0_9_work_employment_within_india_classified_origin"
  )
}

census_d04_count_columns <- function() {
  c(
    "migrants_total", "migrants_illiterate", "migrants_literate",
    "migrants_literate_below_matric", "migrants_matric_below_graduate",
    "migrants_technical_diploma_below_degree", "migrants_graduate_nontechnical",
    "migrants_technical_degree", "migrants_literate_education_not_classified"
  )
}

census_d05_count_columns <- function() {
  c(
    census_d03_reason_count_columns(),
    "working_age_migrants_15_64", "work_migrants_age_20_49",
    "education_migrants_age_15_24"
  )
}

census_d06_count_columns <- function() {
  c(
    "migrants_total", "main_workers", "marginal_workers",
    "marginal_workers_seeking_work", "non_workers", "non_workers_seeking_work",
    "working_age_migrants_15_64", "working_age_main_workers",
    "working_age_marginal_workers", "working_age_marginal_workers_seeking_work",
    "working_age_non_workers", "working_age_non_workers_seeking_work"
  )
}

census_d07_count_columns <- function() {
  c(
    "recent_work_migrants_total", "recent_work_migrants_within_state",
    "recent_work_migrants_outside_state", "recent_work_migrants_rural_origin",
    "recent_work_migrants_urban_origin", "recent_work_migrants_illiterate",
    "recent_work_migrants_literate", "recent_work_migrants_literate_below_matric",
    "recent_work_migrants_matric_below_graduate",
    "recent_work_migrants_technical_diploma_below_degree",
    "recent_work_migrants_graduate_nontechnical",
    "recent_work_migrants_technical_degree",
    "recent_work_migrants_literate_education_not_classified"
  )
}

add_census_d02_migration_shares <- function(x) {
  x <- safe_df(x)
  x$recent_0_9_share_among_migrants <- safe_count_share(x$migrants_recent_0_9, x$migrants_total)
  x$within_district_share_among_migrants <- safe_count_share(x$migrants_within_district, x$migrants_total)
  x$other_district_same_state_share_among_migrants <- safe_count_share(
    x$migrants_other_district_same_state, x$migrants_total
  )
  x$interstate_share_among_migrants <- safe_count_share(x$migrants_interstate, x$migrants_total)
  x$outside_india_share_among_migrants <- safe_count_share(x$migrants_outside_india, x$migrants_total)
  x
}

add_census_d03_reason_shares <- function(x) {
  x <- safe_df(x)
  for (column in setdiff(census_d03_reason_count_columns(), "migrants_total")) {
    x[[paste0(column, "_share_among_migrants")]] <- safe_count_share(
      x[[column]], x$migrants_total
    )
  }
  x
}

add_census_migrant_education_shares <- function(
    x, total, literate, technical_diploma, graduate_nontechnical,
    technical_degree, suffix) {
  x <- safe_df(x)
  graduate_or_technical_degree <- num(x[[graduate_nontechnical]]) + num(x[[technical_degree]])
  technical_credential <- num(x[[technical_diploma]]) + num(x[[technical_degree]])
  x[[paste0("literate_share_", suffix)]] <- safe_count_share(x[[literate]], x[[total]])
  x[[paste0("graduate_or_technical_degree_share_", suffix)]] <-
    safe_count_share(graduate_or_technical_degree, x[[total]])
  x[[paste0("technical_credential_share_", suffix)]] <-
    safe_count_share(technical_credential, x[[total]])
  x
}

add_census_d04_education_shares <- function(x) {
  add_census_migrant_education_shares(
    x,
    total = "migrants_total",
    literate = "migrants_literate",
    technical_diploma = "migrants_technical_diploma_below_degree",
    graduate_nontechnical = "migrants_graduate_nontechnical",
    technical_degree = "migrants_technical_degree",
    suffix = "among_migrants"
  )
}

add_census_d05_age_reason_shares <- function(x) {
  x <- safe_df(x)
  x$working_age_15_64_share_among_migrants <- safe_count_share(
    x$working_age_migrants_15_64, x$migrants_total
  )
  x$age_20_49_share_among_work_migrants <- safe_count_share(
    x$work_migrants_age_20_49, x$work_employment
  )
  x$age_15_24_share_among_education_migrants <- safe_count_share(
    x$education_migrants_age_15_24, x$education
  )
  x
}

add_census_d06_activity_shares <- function(x) {
  x <- safe_df(x)
  x$main_worker_share_among_working_age_migrants <- safe_count_share(
    x$working_age_main_workers, x$working_age_migrants_15_64
  )
  x$marginal_worker_share_among_working_age_migrants <- safe_count_share(
    x$working_age_marginal_workers, x$working_age_migrants_15_64
  )
  x$non_worker_share_among_working_age_migrants <- safe_count_share(
    x$working_age_non_workers, x$working_age_migrants_15_64
  )
  seeking <- num(x$working_age_marginal_workers_seeking_work) +
    num(x$working_age_non_workers_seeking_work)
  non_main <- num(x$working_age_marginal_workers) + num(x$working_age_non_workers)
  x$seeking_work_share_among_working_age_migrants <- safe_count_share(
    seeking, x$working_age_migrants_15_64
  )
  x$seeking_work_share_among_working_age_non_main <- safe_count_share(
    seeking, non_main
  )
  x
}

add_census_d07_work_migrant_shares <- function(x) {
  x <- safe_df(x)
  x$outside_state_share_among_recent_work_migrants <- safe_count_share(
    x$recent_work_migrants_outside_state, x$recent_work_migrants_total
  )
  x$rural_origin_share_among_recent_work_migrants <- safe_count_share(
    x$recent_work_migrants_rural_origin, x$recent_work_migrants_total
  )
  add_census_migrant_education_shares(
    x,
    total = "recent_work_migrants_total",
    literate = "recent_work_migrants_literate",
    technical_diploma = "recent_work_migrants_technical_diploma_below_degree",
    graduate_nontechnical = "recent_work_migrants_graduate_nontechnical",
    technical_degree = "recent_work_migrants_technical_degree",
    suffix = "among_recent_work_migrants"
  )
}

validate_census_2011_migration_totals <- function(d02_2011, d03_2011) {
  out <- validate_census_matching_count(
    d02_2011, d03_2011, "migrants_total", "migrants_total",
    "Census 2011 D02/D03 all-duration migrant"
  )
  names(out)[names(out) == "max_abs_difference"] <- "max_abs_total_difference"
  out
}


validate_census_2011_d02_d04_totals <- function(d02_2011, d04_2011) {
  validate_census_matching_count(
    d02_2011, d04_2011, "migrants_total", "migrants_total",
    "Census 2011 D02/D04 all-migrant"
  )
}

validate_census_2011_d03_d05_reasons <- function(d03_2011, d05_2011) {
  columns <- census_d03_reason_count_columns()
  safe_bind_rows(lapply(columns, function(column) {
    out <- validate_census_matching_count(
      d03_2011, d05_2011, column, column,
      paste0("Census 2011 D03/D05 ", column)
    )
    out$measure <- column
    out
  }))
}

validate_census_2011_d02_d06_totals <- function(d02_2011, d06_2011) {
  validate_census_matching_count(
    d02_2011, d06_2011, "migrants_total", "migrants_total",
    "Census 2011 D02/D06 all-migrant"
  )
}

validate_census_2011_d03_d07_recent_work <- function(d03_2011, d07_2011) {
  validate_census_matching_count(
    d03_2011, d07_2011,
    "recent_0_9_work_employment_within_india_classified_origin",
    "recent_work_migrants_total",
    "Census 2011 D03/D07 recent work-migrant"
  )
}

census_d02_population_rate_columns <- function() {
  c(
    "migrant_stock_share_population",
    "recent_0_9_migrant_share_population",
    "interdistrict_migrant_share_population",
    "interstate_migrant_share_population"
  )
}

add_census_d02_population_rates <- function(x, population_column) {
  x <- safe_df(x)
  if (!population_column %in% names(x)) {
    stop("Census D02 population denominator column is missing: ", population_column, call. = FALSE)
  }
  population <- num(x[[population_column]])
  interdistrict <- num(x$migrants_other_district_same_state) + num(x$migrants_interstate)
  x$migrant_stock_share_population <- safe_count_share(x$migrants_total, population)
  x$recent_0_9_migrant_share_population <- safe_count_share(x$migrants_recent_0_9, population)
  x$interdistrict_migrant_share_population <- safe_count_share(interdistrict, population)
  x$interstate_migrant_share_population <- safe_count_share(x$migrants_interstate, population)
  rate_cols <- census_d02_population_rate_columns()
  if (any(!is.finite(as.matrix(x[rate_cols])))) {
    stop("Census D02 counts are incompatible with district population denominators.", call. = FALSE)
  }
  x
}

build_census_2011_population_measures <- function(
    population_2011, district_transition_2001_2011) {
  pooled <- harmonize_census_2011_counts_to_2001(
    population_2011, district_transition_2001_2011, "population_total_2011"
  )
  if (nrow(pooled) && any(num(pooled$population_total_2011) <= 0)) {
    stop("Harmonized Census-2011 population denominators must be positive.", call. = FALSE)
  }
  pooled$census_year <- rep.int(2011L, nrow(pooled))
  pooled
}

validate_census_2011_migration_population <- function(d02_2011, population_2011) {
  joined <- merge_census_district_sources(
    d02_2011, population_2011,
    "Census 2011 D02", "SHRUG Census-2011 PCA"
  )
  migrants <- num(joined$migrants_total)
  population <- num(joined$population_total_2011)
  valid <- is.finite(migrants) & migrants >= 0 & is.finite(population) & population > 0 &
    migrants <= population
  if (any(!valid)) {
    bad <- joined[!valid, , drop = FALSE]
    stop(
      "Census 2011 D02 migrants exceed or lack district population; first mismatch: ",
      bad$state_code[[1L]], "/", bad$district_code[[1L]], ".",
      call. = FALSE
    )
  }
  data.frame(
    n_districts = nrow(joined),
    max_migrant_stock_share_population = max(migrants / population),
    stringsAsFactors = FALSE
  )
}

build_census_d02_2001_measures <- function(d02_2001, census_2001_district_totals) {
  x <- safe_df(d02_2001)
  if (anyDuplicated(x[c("state_code", "district_code")])) {
    stop("Census-2001 D02 measures require unique district rows.", call. = FALSE)
  }
  population <- safe_df(census_2001_district_totals)
  required_population <- c("state_code_2001", "district_code_2001", "population_total")
  missing <- setdiff(required_population, names(population))
  if (length(missing)) {
    stop("Census-2001 population denominator lacks columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  population <- population[required_population]
  names(population)[1:2] <- c("state_code", "district_code")
  population$state_code <- normalize_census_code(population$state_code, 2L)
  population$district_code <- normalize_census_code(population$district_code, 2L)
  if (anyDuplicated(population[c("state_code", "district_code")])) {
    stop("Census-2001 population denominator is not unique by district.", call. = FALSE)
  }
  district_key <- function(frame) paste(frame$state_code, frame$district_code, sep = "/")
  if (!setequal(district_key(x), district_key(population))) {
    stop("Census-2001 D02 and population denominator district coverage differ.", call. = FALSE)
  }
  x <- merge(x, population, by = c("state_code", "district_code"), all.x = TRUE, sort = FALSE)
  if (any(!is.finite(num(x$population_total)) | num(x$population_total) <= 0)) {
    stop("Census-2001 D02 migration rows lack positive district population denominators.", call. = FALSE)
  }
  x$target_unit_2001 <- paste0(
    "pc2001__", normalize_census_code(x$state_code, 2L), "__",
    normalize_census_code(x$district_code, 2L)
  )
  x <- add_census_d02_population_rates(x, "population_total")
  add_census_d02_migration_shares(x)
}

build_census_d02_2011_measures <- function(
    d02_2011, district_transition_2001_2011, population_2011) {
  pooled <- harmonize_census_2011_counts_to_2001(
    d02_2011, district_transition_2001_2011, census_d02_count_columns()
  )
  population <- safe_df(population_2011)
  needed_population <- c("target_unit_2001", "population_total_2011")
  missing <- setdiff(needed_population, names(population))
  if (length(missing)) {
    stop(
      "Harmonized Census-2011 population denominator lacks columns: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  if (anyDuplicated(population$target_unit_2001) ||
      !setequal(pooled$target_unit_2001, population$target_unit_2001)) {
    stop("Census 2011 D02 and population denominator target coverage differ.", call. = FALSE)
  }
  pooled_order <- pooled$target_unit_2001
  pooled <- merge(
    pooled, population[needed_population],
    by = "target_unit_2001", all.x = TRUE, sort = FALSE
  )
  pooled <- pooled[match(pooled_order, pooled$target_unit_2001), , drop = FALSE]
  pooled$census_year <- rep.int(2011L, nrow(pooled))
  pooled <- add_census_d02_population_rates(pooled, "population_total_2011")
  add_census_d02_migration_shares(pooled)
}

build_census_d02_population_change_measures <- function(d02_2001, d02_2011) {
  baseline <- safe_df(d02_2001)
  followup <- safe_df(d02_2011)
  rates <- census_d02_population_rate_columns()
  required_baseline <- c("target_unit_2001", rates)
  required_followup <- c("target_unit_2001", rates)
  missing <- unique(c(
    setdiff(required_baseline, names(baseline)),
    setdiff(required_followup, names(followup))
  ))
  if (length(missing)) {
    stop("Census D02 population-change measures lack columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(baseline$target_unit_2001) || anyDuplicated(followup$target_unit_2001)) {
    stop("Census D02 population-change measures require unique target districts.", call. = FALSE)
  }
  if (!all(followup$target_unit_2001 %in% baseline$target_unit_2001)) {
    stop("Census 2011 D02 contains targets absent from the Census-2001 baseline.", call. = FALSE)
  }
  left <- baseline[c("target_unit_2001", rates)]
  right <- followup[c("target_unit_2001", rates)]
  names(left)[-1L] <- paste0(rates, "_2001")
  names(right)[-1L] <- paste0(rates, "_2011")
  out <- merge(right, left, by = "target_unit_2001", all.x = TRUE, sort = FALSE)
  out <- out[match(followup$target_unit_2001, out$target_unit_2001), , drop = FALSE]
  for (rate in rates) {
    out[[paste0(rate, "_change_2011_2001")]] <-
      num(out[[paste0(rate, "_2011")]]) - num(out[[paste0(rate, "_2001")]])
  }
  rownames(out) <- NULL
  out
}

build_census_d03_2011_measures <- function(d03_2011, district_transition_2001_2011) {
  pooled <- harmonize_census_2011_counts_to_2001(
    d03_2011, district_transition_2001_2011, census_d03_count_columns()
  )
  pooled$census_year <- rep.int(2011L, nrow(pooled))
  add_census_d03_reason_shares(pooled)
}

build_census_d04_2011_measures <- function(d04_2011, district_transition_2001_2011) {
  pooled <- harmonize_census_2011_counts_to_2001(
    d04_2011, district_transition_2001_2011, census_d04_count_columns()
  )
  pooled$census_year <- rep.int(2011L, nrow(pooled))
  add_census_d04_education_shares(pooled)
}

build_census_d05_2011_measures <- function(d05_2011, district_transition_2001_2011) {
  pooled <- harmonize_census_2011_counts_to_2001(
    d05_2011, district_transition_2001_2011, census_d05_count_columns()
  )
  pooled$census_year <- rep.int(2011L, nrow(pooled))
  add_census_d05_age_reason_shares(pooled)
}

build_census_d06_2011_measures <- function(d06_2011, district_transition_2001_2011) {
  pooled <- harmonize_census_2011_counts_to_2001(
    d06_2011, district_transition_2001_2011, census_d06_count_columns()
  )
  pooled$census_year <- rep.int(2011L, nrow(pooled))
  add_census_d06_activity_shares(pooled)
}

build_census_d07_2011_measures <- function(d07_2011, district_transition_2001_2011) {
  pooled <- harmonize_census_2011_counts_to_2001(
    d07_2011, district_transition_2001_2011, census_d07_count_columns()
  )
  pooled$census_year <- rep.int(2011L, nrow(pooled))
  add_census_d07_work_migrant_shares(pooled)
}

summarise_census_migration_coverage <- function(frames) {
  if (!is.list(frames) || is.null(names(frames)) || any(!nzchar(names(frames)))) {
    stop("Census migration coverage requires a named list of datasets.", call. = FALSE)
  }
  safe_bind_rows(lapply(names(frames), function(name) {
    x <- safe_df(frames[[name]])
    denominator <- if ("migrants_total" %in% names(x)) {
      "migrants_total"
    } else if ("recent_work_migrants_total" %in% names(x)) {
      "recent_work_migrants_total"
    } else {
      NA_character_
    }
    data.frame(
      dataset = name,
      n_districts = nrow(x),
      n_positive_migrant_denominators = if (!is.na(denominator)) {
        sum(is.finite(num(x[[denominator]])) & num(x[[denominator]]) > 0)
      } else {
        NA_integer_
      },
      stringsAsFactors = FALSE
    )
  }))
}
