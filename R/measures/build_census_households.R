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
  x$workers_per_household <- ifelse(is.finite(num(H)) & num(H) > 0, num(x$workers_total) / num(H), NA_real_)
  marginal <- x$marginal_workers_3_6_months + x$marginal_workers_lt3_months
  x$marginal_worker_share_workers <- safe_count_share(marginal, x$workers_total)
  x$short_marginal_share_marginal_workers <- safe_count_share(x$marginal_workers_lt3_months, marginal)
  x
}

build_census_2011_household_measures <- function(hh08, hh10, hh11, district_transition_2001_2011) {
  source <- build_census_2011_household_source(hh08, hh10, hh11)
  pooled <- harmonize_census_2011_counts_to_2001(source, district_transition_2001_2011, census_household_count_columns())
  pooled$census_year <- rep.int(2011L, nrow(pooled))
  add_census_household_measures(pooled)
}
