# Harmonized Census 2011 local economic-structure measures.

census_worker_keys <- function() c("state_code", "district_code")

census_industry_combined_count_columns <- function() {
  c(
    "workers_total", "main_workers_total", "marginal_workers_total",
    paste0("industry_", unname(census_industry_groups()))
  )
}

build_census_2011_industry_source <- function(b04, b06) {
  keys <- census_worker_keys()
  b04 <- safe_df(b04)[c(keys, "district_name", census_industry_count_columns("main"))]
  b06 <- safe_df(b06)[c(keys, "district_name", census_industry_count_columns("marginal"))]
  x <- merge_census_district_sources(b04, b06, "Census B04", "Census B06")
  x$workers_total <- num(x$main_workers_total) + num(x$marginal_workers_total)
  x$industry_agriculture <-
    num(x$main_cultivators) + num(x$main_agricultural_labourers) + num(x$main_agriculture_other) +
    num(x$marginal_cultivators) + num(x$marginal_agricultural_labourers) +
    num(x$marginal_agriculture_other)
  for (group in setdiff(unname(census_industry_groups()), "agriculture")) {
    x[[paste0("industry_", group)]] <- num(x[[paste0("main_", group)]]) +
      num(x[[paste0("marginal_", group)]])
  }
  parts <- paste0("industry_", unname(census_industry_groups()))
  if (any(rowSums(as.matrix(data.frame(lapply(x[parts], num), check.names = FALSE))) != x$workers_total)) {
    stop("Combined Census B04/B06 industry categories do not sum exactly to workers.", call. = FALSE)
  }
  x
}

census_occupation_combined_count_columns <- function() {
  c(
    "workers_excl_cultivators_aglab_total",
    paste0("occupation_division_", c(as.character(1:9), "x"))
  )
}

build_census_2011_occupation_source <- function(b25a, b25b) {
  keys <- census_worker_keys()
  main_columns <- c(
    "main_workers_excl_cultivators_aglab_total",
    paste0("main_occupation_division_", c(as.character(1:9), "x"))
  )
  marginal_columns <- c(
    "marginal_workers_excl_cultivators_aglab_total",
    paste0("marginal_occupation_division_", c(as.character(1:9), "x"))
  )
  b25a <- safe_df(b25a)[c(keys, "district_name", main_columns)]
  b25b <- safe_df(b25b)[c(keys, "district_name", marginal_columns)]
  x <- merge_census_district_sources(b25a, b25b, "Census B25A", "Census B25B")
  x$workers_excl_cultivators_aglab_total <-
    num(x$main_workers_excl_cultivators_aglab_total) +
    num(x$marginal_workers_excl_cultivators_aglab_total)
  for (division in c(as.character(1:9), "x")) {
    x[[paste0("occupation_division_", division)]] <-
      num(x[[paste0("main_occupation_division_", division)]]) +
      num(x[[paste0("marginal_occupation_division_", division)]])
  }
  parts <- paste0("occupation_division_", c(as.character(1:9), "x"))
  if (any(rowSums(as.matrix(data.frame(lapply(x[parts], num), check.names = FALSE))) !=
          x$workers_excl_cultivators_aglab_total)) {
    stop("Combined Census B25A/B occupation divisions do not sum exactly to workers.", call. = FALSE)
  }
  x
}

validate_census_2011_b04_b25a_universe <- function(b04, b25a) {
  left <- safe_df(b04)
  left$expected_b25a_total <- num(left$main_workers_total) - num(left$main_cultivators) -
    num(left$main_agricultural_labourers)
  validate_census_matching_count(
    left, b25a,
    "expected_b25a_total", "main_workers_excl_cultivators_aglab_total",
    "Census 2011 B04/B25A worker-universe"
  )
}

validate_census_2011_b06_b25b_universe <- function(b06, b25b) {
  left <- safe_df(b06)
  left$expected_b25b_total <- num(left$marginal_workers_total) - num(left$marginal_cultivators) -
    num(left$marginal_agricultural_labourers)
  validate_census_matching_count(
    left, b25b,
    "expected_b25b_total", "marginal_workers_excl_cultivators_aglab_total",
    "Census 2011 B06/B25B worker-universe"
  )
}

add_census_industry_shares <- function(x) {
  x <- safe_df(x)
  x$main_worker_share_among_workers <- safe_count_share(x$main_workers_total, x$workers_total)
  x$marginal_worker_share_among_workers <- safe_count_share(x$marginal_workers_total, x$workers_total)
  for (group in unname(census_industry_groups())) {
    x[[paste0(group, "_share_among_workers")]] <- safe_count_share(
      x[[paste0("industry_", group)]], x$workers_total
    )
  }
  x
}

add_census_occupation_shares <- function(x) {
  x <- safe_df(x)
  total <- x$workers_excl_cultivators_aglab_total
  x$manager_professional_technical_share <- safe_count_share(
    x$occupation_division_1 + x$occupation_division_2 + x$occupation_division_3, total
  )
  x$clerical_service_sales_share <- safe_count_share(
    x$occupation_division_4 + x$occupation_division_5, total
  )
  x$skilled_agriculture_fishery_share <- safe_count_share(x$occupation_division_6, total)
  x$craft_machine_operator_share <- safe_count_share(
    x$occupation_division_7 + x$occupation_division_8, total
  )
  x$elementary_occupation_share <- safe_count_share(x$occupation_division_9, total)
  x$occupation_not_classified_share <- safe_count_share(x$occupation_division_x, total)
  x
}

build_census_2011_industry_measures <- function(b04, b06, district_transition_2001_2011) {
  source <- build_census_2011_industry_source(b04, b06)
  pooled <- harmonize_census_2011_counts_to_2001(
    source, district_transition_2001_2011, census_industry_combined_count_columns()
  )
  pooled$census_year <- rep.int(2011L, nrow(pooled))
  add_census_industry_shares(pooled)
}

build_census_2011_occupation_measures <- function(b25a, b25b, district_transition_2001_2011) {
  source <- build_census_2011_occupation_source(b25a, b25b)
  pooled <- harmonize_census_2011_counts_to_2001(
    source, district_transition_2001_2011, census_occupation_combined_count_columns()
  )
  pooled$census_year <- rep.int(2011L, nrow(pooled))
  add_census_occupation_shares(pooled)
}


build_census_2001_industry_measures <- function(b04) {
  x <- safe_df(b04)
  required <- c(
    "state_code", "district_code", "district_name",
    census_2001_industry_count_columns()
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Census 2001 B04 measures are missing required columns.", call. = FALSE)
  }
  if (anyDuplicated(x[c("state_code", "district_code")])) {
    stop("Census 2001 B04 measures are not unique by district.", call. = FALSE)
  }
  total <- num(x$main_workers_total)
  for (group in unname(census_2001_industry_groups())) {
    x[[paste0(group, "_share_among_main_workers")]] <- safe_count_share(
      x[[paste0("main_", group)]], total
    )
  }
  x$census_year <- rep.int(2001L, nrow(x))
  x
}

build_census_2001_occupation_measures <- function(b26) {
  x <- safe_df(b26)
  required <- c(
    "state_code", "district_code", "district_name",
    "main_workers_excl_cultivators_aglab_total",
    "marginal_workers_excl_cultivators_aglab_total",
    paste0("main_occupation_division_", c(as.character(1:9), "x")),
    paste0("marginal_occupation_division_", c(as.character(1:9), "x"))
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Census 2001 B26 measures are missing required columns.", call. = FALSE)
  }
  if (anyDuplicated(x[c("state_code", "district_code")])) {
    stop("Census 2001 B26 measures are not unique by district.", call. = FALSE)
  }
  x$workers_excl_cultivators_aglab_total <-
    num(x$main_workers_excl_cultivators_aglab_total) +
    num(x$marginal_workers_excl_cultivators_aglab_total)
  for (division in c(as.character(1:9), "x")) {
    x[[paste0("occupation_division_", division)]] <-
      num(x[[paste0("main_occupation_division_", division)]]) +
      num(x[[paste0("marginal_occupation_division_", division)]])
  }
  parts <- paste0("occupation_division_", c(as.character(1:9), "x"))
  if (any(rowSums(as.matrix(data.frame(lapply(x[parts], num), check.names = FALSE))) !=
          x$workers_excl_cultivators_aglab_total)) {
    stop("Census 2001 B26 combined occupation divisions do not sum exactly to workers.", call. = FALSE)
  }
  x$census_year <- rep.int(2001L, nrow(x))
  add_census_occupation_shares(x)
}

validate_census_2001_b25_b26_main_occupation <- function(b25, b26) {
  columns <- c(
    "main_workers_excl_cultivators_aglab_total",
    paste0("main_occupation_division_", c(as.character(1:9), "x"))
  )
  safe_bind_rows(lapply(columns, function(column) {
    out <- validate_census_matching_count(
      b25, b26, column, column,
      paste0("Census 2001 B25/B26 ", column)
    )
    out$count_column <- column
    out
  }))
}
