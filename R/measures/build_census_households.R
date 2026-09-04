# Census 2011 household human-capital and worker-intensity measures.

census_household_count_columns <- function() {
  c(
    "households_total", "households_no_literate", "households_1_literate",
    "households_2_literates", "households_3_literates", "households_4_plus_literates",
    "households_age15_plus", "households_with_matriculate", "households_with_female_matriculate",
    "households_with_graduate", "households_with_female_graduate",
    "households_no_workers", "households_1_worker", "households_2_workers",
    "households_3_workers", "households_4_plus_workers", "workers_total", "main_workers",
    "marginal_workers_3_6_months", "marginal_workers_lt3_months"
  )
}

validate_census_2011_household_sources <- function(hh08, hh10, hh11) {
  hh08 <- safe_df(hh08); hh10 <- safe_df(hh10); hh11 <- safe_df(hh11)
  hh10$households_matriculation_partition <- hh10$households_no_matriculate + hh10$households_with_matriculate
  safe_bind_rows(list(
    transform(
      validate_census_matching_count(hh08, hh11, "households_total", "households_total", "Census HH08/HH11 household universe"),
      check = "household_total_hh08_vs_hh11"
    ),
    transform(
      validate_census_matching_count(hh08, hh10, "households_total", "households_matriculation_partition", "Census HH08/HH10 matriculation partition"),
      check = "household_total_vs_hh10_matriculation_partition"
    ),
    transform(
      validate_census_matching_count(hh08, hh10, "households_with_literate_member", "households_with_literate_member", "Census HH08/HH10 literate-household count"),
      check = "literate_households_hh08_vs_hh10"
    )
  ))
}

build_census_2011_household_source <- function(hh08, hh10, hh11) {
  validate_census_2011_household_sources(hh08, hh10, hh11)
  x <- safe_df(hh08)[c(
    "state_code", "district_code", "district_name", "households_total", "households_no_literate",
    "households_1_literate", "households_2_literates", "households_3_literates", "households_4_plus_literates"
  )]
  education <- safe_df(hh10)[c(
    "state_code", "district_code", "households_age15_plus", "households_with_matriculate", "households_with_female_matriculate",
    "households_with_graduate", "households_with_female_graduate"
  )]
  workers <- safe_df(hh11)[c(
    "state_code", "district_code", "households_no_workers", "households_1_worker", "households_2_workers",
    "households_3_workers", "households_4_plus_workers", "workers_total", "main_workers",
    "marginal_workers_3_6_months", "marginal_workers_lt3_months"
  )]
  x <- merge_census_district_sources(x, education, "Census HH08", "Census HH10", c("district_name", "households_total"))
  merge_census_district_sources(x, workers, "Census HH08/HH10", "Census HH11", c("district_name", "households_total"))
}

add_census_household_measures <- function(x) {
  x <- safe_df(x)
  H <- x$households_total
  x$no_literate_share_households <- safe_count_share(x$households_no_literate, H)
  x$two_plus_literate_share_households <- safe_count_share(
    x$households_2_literates + x$households_3_literates + x$households_4_plus_literates, H
  )
  x$four_plus_literate_share_households <- safe_count_share(x$households_4_plus_literates, H)
  H15 <- x$households_age15_plus
  x$matriculate_access_share_households_age15_plus <- safe_count_share(x$households_with_matriculate, H15)
  x$female_matriculate_access_share_households_age15_plus <- safe_count_share(x$households_with_female_matriculate, H15)
  x$graduate_access_share_households_age15_plus <- safe_count_share(x$households_with_graduate, H15)
  x$female_graduate_access_share_households_age15_plus <- safe_count_share(x$households_with_female_graduate, H15)
  x$workerless_share_households <- safe_count_share(x$households_no_workers, H)
  x$two_plus_worker_share_households <- safe_count_share(
    x$households_2_workers + x$households_3_workers + x$households_4_plus_workers, H
  )
  x$four_plus_worker_share_households <- safe_count_share(x$households_4_plus_workers, H)
  if (all(c("workers_total", "marginal_workers_3_6_months", "marginal_workers_lt3_months") %in% names(x))) {
    x$workers_per_household <- ifelse(is.finite(num(H)) & num(H) > 0, num(x$workers_total) / num(H), NA_real_)
    marginal <- x$marginal_workers_3_6_months + x$marginal_workers_lt3_months
    x$marginal_worker_share_workers <- safe_count_share(marginal, x$workers_total)
    x$short_marginal_share_marginal_workers <- safe_count_share(x$marginal_workers_lt3_months, marginal)
  }
  x
}

build_census_2011_household_measures <- function(hh08, hh10, hh11, district_transition_2001_2011) {
  source <- build_census_2011_household_source(hh08, hh10, hh11)
  pooled <- harmonize_census_2011_counts_to_2001(source, district_transition_2001_2011, census_household_count_columns())
  pooled$census_year <- rep.int(2011L, nrow(pooled))
  add_census_household_measures(pooled)
}

census_household_longitudinal_share_columns <- function() {
  c(
    "no_literate_share_households",
    "two_plus_literate_share_households",
    "four_plus_literate_share_households",
    "matriculate_access_share_households_age15_plus",
    "female_matriculate_access_share_households_age15_plus",
    "graduate_access_share_households_age15_plus",
    "female_graduate_access_share_households_age15_plus",
    "workerless_share_households",
    "two_plus_worker_share_households",
    "four_plus_worker_share_households"
  )
}

validate_census_2001_household_sources <- function(hh09, hh13, hh15, hh15a) {
  hh09 <- safe_df(hh09); hh13 <- safe_df(hh13); hh15 <- safe_df(hh15); hh15a <- safe_df(hh15a)
  hh13$households_total <- hh13$households_age15_plus + hh13$households_without_age15_plus
  checks <- list(
    transform(
      validate_census_matching_count(hh09, hh13, "households_total", "households_total", "Census 2001 HH09/HH13 household universe"),
      check = "household_total_hh09_vs_hh13"
    ),
    transform(
      validate_census_matching_count(hh09, hh15, "households_total", "households_total", "Census 2001 HH09/HH15 household universe"),
      check = "household_total_hh09_vs_hh15"
    ),
    transform(
      validate_census_matching_count(hh15, hh15a, "households_total", "households_total", "Census 2001 HH15/HH15 Appendix household universe"),
      check = "household_total_hh15_vs_hh15a"
    )
  )
  for (column in c(
    "households_no_workers", "households_1_worker", "households_2_workers",
    "households_3_workers", "households_4_plus_workers"
  )) {
    checks[[length(checks) + 1L]] <- transform(
      validate_census_matching_count(hh15, hh15a, column, column, paste("Census 2001 HH15/HH15 Appendix", column)),
      check = paste0("worker_category_hh15_vs_hh15a__", column)
    )
  }
  safe_bind_rows(checks)
}

build_census_2001_household_measures <- function(hh09, hh13, hh15, hh15a) {
  validate_census_2001_household_sources(hh09, hh13, hh15, hh15a)
  x <- safe_df(hh09)[c(
    "state_code", "district_code", "district_name", "households_total", "households_no_literate",
    "households_1_literate", "households_2_literates", "households_3_literates", "households_4_plus_literates"
  )]
  education <- safe_df(hh13)[c(
    "state_code", "district_code", "households_age15_plus", "households_with_matriculate",
    "households_with_female_matriculate", "households_with_graduate", "households_with_female_graduate"
  )]
  workers <- safe_df(hh15)[c(
    "state_code", "district_code", "households_no_workers", "households_1_worker",
    "households_2_workers", "households_3_workers", "households_4_plus_workers"
  )]
  x <- merge_census_district_sources(x, education, "Census 2001 HH09", "Census 2001 HH13", c("district_name", "households_total"))
  x <- merge_census_district_sources(x, workers, "Census 2001 HH09/HH13", "Census 2001 HH15", c("district_name", "households_total"))
  x$target_unit_2001 <- paste0("pc2001__", x$state_code, "__", x$district_code)
  x$census_year <- rep.int(2001L, nrow(x))
  add_census_household_measures(x)
}

build_census_household_change_measures <- function(household_2001, household_2011) {
  baseline <- safe_df(household_2001)
  followup <- safe_df(household_2011)
  common <- census_household_longitudinal_share_columns()
  baseline <- baseline[c("target_unit_2001", "households_total", common)]
  names(baseline)[names(baseline) == "households_total"] <- "households_total_2001"
  names(baseline)[names(baseline) %in% common] <- paste0(
    names(baseline)[names(baseline) %in% common], "_2001"
  )
  followup <- followup[c(
    "target_unit_2001", "census_2011_source_district_count",
    "census_2011_parent_reconstruction_complete", "households_total", common
  )]
  names(followup)[names(followup) == "households_total"] <- "households_total_2011"
  names(followup)[names(followup) %in% common] <- paste0(
    names(followup)[names(followup) %in% common], "_2011"
  )
  out <- merge(followup, baseline, by = "target_unit_2001", all.x = TRUE, sort = FALSE)
  if (nrow(out) != nrow(followup) || anyDuplicated(out$target_unit_2001)) {
    stop("Census household changes require one Census-2001 baseline row per harmonized 2011 parent.", call. = FALSE)
  }
  for (variable in common) {
    out[[paste0(variable, "_change_2011_2001")]] <-
      num(out[[paste0(variable, "_2011")]]) - num(out[[paste0(variable, "_2001")]])
  }
  rownames(out) <- NULL
  out
}
