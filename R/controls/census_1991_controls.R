# Predetermined Census-1991 district attributes from SHRUG district products.

census_1991_keys <- function() c("state_code_1991", "district_code_1991")

historical_baseline_1991_metadata <- function() {
  data.frame(
    variable = c(
      "log_population_1991", "female_share_1991", "child_share_0_6_1991",
      "sc_share_1991", "st_share_1991", "literacy_share_7plus_1991",
      "main_worker_share_1991", "marginal_worker_share_1991",
      "cultivator_share_main_workers_1991", "agricultural_labourer_share_main_workers_1991",
      "rural_primary_schools_per_100k_1991", "rural_high_schools_per_100k_1991",
      "rural_hospitals_per_100k_1991", "rural_phc_per_100k_1991",
      "urban_literacy_share_7plus_1991", "urban_primary_schools_per_100k_7plus_1991",
      "urban_hospitals_per_100k_7plus_1991", "urban_banks_per_100k_7plus_1991"
    ),
    domain = c(
      rep("demography", 5L), "human_capital",
      rep("economic_structure", 4L),
      rep("rural_development", 4L),
      rep("urban_development", 4L)
    ),
    source = c(
      rep("PCA91", 10L), rep("VD91", 4L), rep("TD91", 4L)
    ),
    label = c(
      "Log population", "Female population share", "Population age 0-6 share",
      "Scheduled Caste share", "Scheduled Tribe share", "Literacy share, age 7+",
      "Main-worker share", "Marginal-worker share",
      "Cultivator share of main workers", "Agricultural-labourer share of main workers",
      "Rural primary schools per 100,000 district residents",
      "Rural high schools per 100,000 district residents",
      "Rural hospitals per 100,000 district residents",
      "Rural primary health centres per 100,000 district residents",
      "Urban literacy share, age 7+",
      "Urban primary schools per 100,000 urban residents age 7+",
      "Urban hospitals per 100,000 urban residents age 7+",
      "Urban banks per 100,000 urban residents age 7+"
    ),
    stringsAsFactors = FALSE
  )
}

historical_baseline_1991_variables <- function() historical_baseline_1991_metadata()$variable

historical_baseline_1991_control_sets <- function() {
  metadata <- historical_baseline_1991_metadata()
  list(
    pca = metadata$variable[metadata$source == "PCA91"],
    all = metadata$variable
  )
}

validate_census_1991_district_keys <- function(x, source) {
  x <- safe_df(x)
  keys <- census_1991_keys()
  missing <- setdiff(keys, names(x))
  if (length(missing)) stop(source, " lacks standardized 1991 district keys.", call. = FALSE)
  if (any(!stats::complete.cases(x[keys]))) stop(source, " contains missing 1991 district keys.", call. = FALSE)
  if (anyDuplicated(x[keys])) stop(source, " contains duplicate 1991 district keys.", call. = FALSE)
  x
}


census_1991_district_key <- function(x) {
  x <- validate_census_1991_district_keys(x, "Census-1991 district source")
  paste(x$state_code_1991, x$district_code_1991, sep = "__")
}

require_census_1991_subset <- function(source, reference, label) {
  extra <- setdiff(census_1991_district_key(source), census_1991_district_key(reference))
  if (length(extra)) stop(label, " contains district keys absent from PCA91.", call. = FALSE)
  invisible(TRUE)
}

historical_baseline_1991_pca_count_fields <- function() {
  c(
    "population_1991_count",
    "female_population_1991_count",
    "child_0_6_1991_count",
    "sc_population_1991_count",
    "st_population_1991_count",
    "literate_1991_count",
    "population_7plus_1991_count",
    "main_workers_1991_count",
    "marginal_workers_1991_count",
    "cultivators_main_1991_count",
    "agricultural_labourers_main_1991_count"
  )
}

historical_baseline_1991_pca_variables <- function() {
  metadata <- historical_baseline_1991_metadata()
  metadata$variable[metadata$source == "PCA91"]
}

production_historical_baseline_1991_controls <- function(
    g2_sensitivity, coverage_threshold = .99) {
  controls <- safe_df(g2_sensitivity$controls)
  required <- c(
    census_2001_keys(), "geography_spec_id", "source_coverage_threshold",
    historical_baseline_1991_pca_variables()
  )
  missing <- setdiff(required, names(controls))
  if (length(missing)) {
    stop(
      "Production 1991 baseline controls are missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  threshold <- as.numeric(coverage_threshold)
  if (length(threshold) != 1L || !is.finite(threshold) || threshold < 0 || threshold > 1) {
    stop("Production 1991 baseline coverage threshold must lie in [0, 1].", call. = FALSE)
  }
  keep <- controls$geography_spec_id == "G2_population_interpolated" &
    abs(num(controls$source_coverage_threshold) - threshold) < 1e-12
  out <- controls[keep, c(census_2001_keys(), historical_baseline_1991_pca_variables()), drop = FALSE]
  if (!nrow(out)) {
    stop(
      "Production 1991 baseline controls have no G2 population-interpolated rows at coverage threshold ",
      threshold, ".",
      call. = FALSE
    )
  }
  out$state_code_2001 <- pad_admin_code(out$state_code_2001, 2L)
  out$district_code_2001 <- pad_admin_code(out$district_code_2001, 2L)
  if (anyDuplicated(out[census_2001_keys()])) {
    stop("Production 1991 baseline controls contain duplicate Census-2001 district keys.", call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

build_shrug_1991_pca_sufficient_statistics <- function(x) {
  x <- safe_df(x)
  required <- c(
    "pc91_state_id", "pc91_district_id", "pc91_pca_tot_p",
    "pc91_pca_tot_f", "pc91_pca_p_06", "pc91_pca_p_sc",
    "pc91_pca_p_st", "pc91_pca_p_lit", "pc91_pca_mainwork_p",
    "pc91_pca_margwork_p", "pc91_pca_main_cl_p",
    "pc91_pca_main_al_p"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "SHRUG PCA91 sufficient statistics are missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  out <- data.frame(
    state_code_1991 = pad_admin_code(x$pc91_state_id, 2L),
    district_code_1991 = pad_admin_code(x$pc91_district_id, 2L),
    population_1991_count = num(x$pc91_pca_tot_p),
    female_population_1991_count = num(x$pc91_pca_tot_f),
    child_0_6_1991_count = num(x$pc91_pca_p_06),
    sc_population_1991_count = num(x$pc91_pca_p_sc),
    st_population_1991_count = num(x$pc91_pca_p_st),
    literate_1991_count = num(x$pc91_pca_p_lit),
    main_workers_1991_count = num(x$pc91_pca_mainwork_p),
    marginal_workers_1991_count = num(x$pc91_pca_margwork_p),
    cultivators_main_1991_count = num(x$pc91_pca_main_cl_p),
    agricultural_labourers_main_1991_count =
      num(x$pc91_pca_main_al_p),
    stringsAsFactors = FALSE
  )
  out$population_7plus_1991_count <-
    out$population_1991_count - out$child_0_6_1991_count
  out$source_unit_id <- geography_transition_unit_id(
    1991L, out$state_code_1991, out$district_code_1991
  )

  out <- validate_census_1991_district_keys(
    out, "SHRUG PCA91 sufficient statistics"
  )
  counts <- historical_baseline_1991_pca_count_fields()
  invalid_negative <- vapply(
    counts,
    function(field) {
      value <- num(out[[field]])
      any(is.finite(value) & value < 0)
    },
    logical(1)
  )
  if (any(invalid_negative)) {
    stop(
      "SHRUG PCA91 contains negative sufficient statistics: ",
      paste(counts[invalid_negative], collapse = ", "),
      call. = FALSE
    )
  }

  invalid <- (
    is.finite(out$female_population_1991_count) &
      is.finite(out$population_1991_count) &
      out$female_population_1991_count > out$population_1991_count
  ) | (
    is.finite(out$child_0_6_1991_count) &
      is.finite(out$population_1991_count) &
      out$child_0_6_1991_count > out$population_1991_count
  ) | (
    is.finite(out$literate_1991_count) &
      is.finite(out$population_7plus_1991_count) &
      out$literate_1991_count > out$population_7plus_1991_count
  ) | (
    is.finite(out$cultivators_main_1991_count) &
      is.finite(out$main_workers_1991_count) &
      out$cultivators_main_1991_count > out$main_workers_1991_count
  ) | (
    is.finite(out$agricultural_labourers_main_1991_count) &
      is.finite(out$main_workers_1991_count) &
      out$agricultural_labourers_main_1991_count >
        out$main_workers_1991_count
  )
  invalid[is.na(invalid)] <- FALSE
  if (any(invalid)) {
    stop(
      "SHRUG PCA91 sufficient statistics violate population accounting.",
      call. = FALSE
    )
  }

  out[c(
    census_1991_keys(), "source_unit_id",
    historical_baseline_1991_pca_count_fields()
  )]
}

historical_baseline_1991_pca_measures_from_counts <- function(x) {
  out <- safe_df(x)
  required <- historical_baseline_1991_pca_count_fields()
  missing <- setdiff(required, names(out))
  if (length(missing)) {
    stop(
      "Interpolated PCA91 counts lack: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  pop <- num(out$population_1991_count)
  pop7 <- num(out$population_7plus_1991_count)
  main <- num(out$main_workers_1991_count)
  out$population_1991 <- pop
  out$log_population_1991 <- ifelse(
    positive_finite(pop), log(pop), NA_real_
  )
  out$female_share_1991 <- safe_share(
    out$female_population_1991_count, pop
  )
  out$child_share_0_6_1991 <- safe_share(
    out$child_0_6_1991_count, pop
  )
  out$sc_share_1991 <- safe_share(
    out$sc_population_1991_count, pop
  )
  out$st_share_1991 <- safe_share(
    out$st_population_1991_count, pop
  )
  out$literacy_share_7plus_1991 <- safe_share(
    out$literate_1991_count, pop7
  )
  out$main_worker_share_1991 <- safe_share(
    out$main_workers_1991_count, pop
  )
  out$marginal_worker_share_1991 <- safe_share(
    out$marginal_workers_1991_count, pop
  )
  out$cultivator_share_main_workers_1991 <- safe_share(
    out$cultivators_main_1991_count, main
  )
  out$agricultural_labourer_share_main_workers_1991 <- safe_share(
    out$agricultural_labourers_main_1991_count, main
  )
  out
}

build_population_interpolated_pca_baseline_1991 <- function(
    pca, crosswalk, coverage_threshold = .99) {
  threshold <- as.numeric(coverage_threshold)
  if (length(threshold) != 1L || !is.finite(threshold) ||
      threshold < 0 || threshold > 1) {
    stop(
      "PCA91 population-interpolation coverage threshold must lie in [0, 1].",
      call. = FALSE
    )
  }

  sufficient <- build_shrug_1991_pca_sufficient_statistics(pca)
  map <- safe_df(crosswalk)
  required_map <- c(
    "source_vintage", "source_unit_id", "target_vintage",
    "target_state_code", "target_district_code", "target_unit_id",
    "allocation_weight", "source_population_coverage"
  )
  missing <- setdiff(required_map, names(map))
  if (length(missing)) {
    stop(
      "PCA91 population interpolation crosswalk lacks: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  coverage <- unique(map[
    map$source_vintage == 1991L &
      map$transition_id != "target_identity",
    c("source_unit_id", "source_population_coverage")
  ])
  if (anyDuplicated(coverage$source_unit_id)) {
    stop(
      "PCA91 population interpolation coverage is not unique by source district.",
      call. = FALSE
    )
  }
  eligible <- coverage$source_unit_id[
    is.finite(num(coverage$source_population_coverage)) &
      num(coverage$source_population_coverage) >= threshold
  ]
  input <- sufficient[
    sufficient$source_unit_id %in% eligible,
    ,
    drop = FALSE
  ]
  if (!nrow(input)) {
    stop(
      "PCA91 population interpolation has no source districts at coverage threshold ",
      threshold, ".",
      call. = FALSE
    )
  }

  allocated <- allocate_population_sufficient_statistics(
    input,
    map,
    source_vintage = 1991L,
    unit_field = "source_unit_id",
    statistic_fields = historical_baseline_1991_pca_count_fields(),
    measure_family = "census_extensive_counts"
  )
  counts <- historical_baseline_1991_pca_count_fields()
  groups <- split(
    seq_len(nrow(allocated)),
    allocated$target_unit_id
  )
  aggregated <- safe_bind_rows(lapply(groups, function(i) {
    part <- allocated[i, , drop = FALSE]
    states <- unique(plain_chr(part$target_state_code))
    districts <- unique(plain_chr(part$target_district_code))
    if (length(states) != 1L || length(districts) != 1L) {
      stop(
        "One interpolated Census-2001 district has inconsistent codes.",
        call. = FALSE
      )
    }
    totals <- vapply(
      counts,
      function(field) {
        value <- num(part[[field]])
        if (!length(value) || any(!is.finite(value))) {
          NA_real_
        } else {
          sum(value)
        }
      },
      numeric(1)
    )
    data.frame(
      state_code_2001 = states[[1L]],
      district_code_2001 = districts[[1L]],
      target_unit_id = part$target_unit_id[[1L]],
      as.list(totals),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }))
  measures <- historical_baseline_1991_pca_measures_from_counts(
    aggregated
  )
  measures$geography_spec_id <- "G2_population_interpolated"
  measures$source_coverage_threshold <- threshold

  input_population <- sum(
    num(input$population_1991_count), na.rm = TRUE
  )
  allocated_population <- sum(
    num(allocated$population_1991_count), na.rm = TRUE
  )
  coverage_summary <- data.frame(
    geography_spec_id = "G2_population_interpolated",
    source_coverage_threshold = threshold,
    n_source_districts = nrow(input),
    n_target_districts = nrow(measures),
    source_population_1991 = input_population,
    allocated_population_1991 = allocated_population,
    allocated_population_share = if (input_population > 0) {
      allocated_population / input_population
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )

  keep <- c(
    "state_code_2001", "district_code_2001", "target_unit_id",
    "geography_spec_id", "source_coverage_threshold",
    "population_1991", historical_baseline_1991_pca_variables()
  )
  list(
    controls = measures[keep],
    coverage = coverage_summary
  )
}

clean_shrug_pca_1991_district <- function(x) {
  x <- safe_df(x)
  required <- c(
    "pc91_state_id", "pc91_district_id", "pc91_pca_tot_p", "pc91_pca_tot_f",
    "pc91_pca_p_06", "pc91_pca_p_sc", "pc91_pca_p_st", "pc91_pca_p_lit",
    "pc91_pca_mainwork_p", "pc91_pca_margwork_p", "pc91_pca_main_cl_p", "pc91_pca_main_al_p"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("SHRUG PCA91 is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  out <- data.frame(
    state_code_1991 = pad_admin_code(x$pc91_state_id, 2L),
    district_code_1991 = pad_admin_code(x$pc91_district_id, 2L),
    population_1991 = num(x$pc91_pca_tot_p),
    stringsAsFactors = FALSE
  )
  population_7plus <- out$population_1991 - num(x$pc91_pca_p_06)
  out$log_population_1991 <- ifelse(positive_finite(out$population_1991), log(out$population_1991), NA_real_)
  out$female_share_1991 <- safe_share(x$pc91_pca_tot_f, out$population_1991)
  out$child_share_0_6_1991 <- safe_share(x$pc91_pca_p_06, out$population_1991)
  out$sc_share_1991 <- safe_share(x$pc91_pca_p_sc, out$population_1991)
  out$st_share_1991 <- safe_share(x$pc91_pca_p_st, out$population_1991)
  out$literacy_share_7plus_1991 <- safe_share(x$pc91_pca_p_lit, population_7plus)
  out$main_worker_share_1991 <- safe_share(x$pc91_pca_mainwork_p, out$population_1991)
  out$marginal_worker_share_1991 <- safe_share(x$pc91_pca_margwork_p, out$population_1991)
  out$cultivator_share_main_workers_1991 <- safe_share(x$pc91_pca_main_cl_p, x$pc91_pca_mainwork_p)
  out$agricultural_labourer_share_main_workers_1991 <- safe_share(x$pc91_pca_main_al_p, x$pc91_pca_mainwork_p)
  validate_census_1991_district_keys(out, "SHRUG PCA91")
}

clean_shrug_vd_1991_district <- function(x, pca) {
  x <- safe_df(x)
  required <- c(
    "pc91_state_id", "pc91_district_id", "pc91_vd_p_sch", "pc91_vd_s_sch",
    "pc91_vd_hosp", "pc91_vd_ph_cntr"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("SHRUG VD91 is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  out <- data.frame(
    state_code_1991 = pad_admin_code(x$pc91_state_id, 2L),
    district_code_1991 = pad_admin_code(x$pc91_district_id, 2L),
    pc91_vd_p_sch = num(x$pc91_vd_p_sch), pc91_vd_s_sch = num(x$pc91_vd_s_sch),
    pc91_vd_hosp = num(x$pc91_vd_hosp), pc91_vd_ph_cntr = num(x$pc91_vd_ph_cntr),
    stringsAsFactors = FALSE
  )
  out <- validate_census_1991_district_keys(out, "SHRUG VD91")
  pca_keep <- validate_census_1991_district_keys(pca, "SHRUG PCA91")[c(census_1991_keys(), "population_1991")]
  out <- merge(out, pca_keep, by = census_1991_keys(), all.x = TRUE, sort = FALSE)
  denom <- out$population_1991
  out$rural_primary_schools_per_100k_1991 <- safe_share(out$pc91_vd_p_sch, denom, scale = 1e5)
  out$rural_high_schools_per_100k_1991 <- safe_share(out$pc91_vd_s_sch, denom, scale = 1e5)
  out$rural_hospitals_per_100k_1991 <- safe_share(out$pc91_vd_hosp, denom, scale = 1e5)
  out$rural_phc_per_100k_1991 <- safe_share(out$pc91_vd_ph_cntr, denom, scale = 1e5)
  out[c(census_1991_keys(), historical_baseline_1991_metadata()$variable[historical_baseline_1991_metadata()$source == "VD91"])]
}

clean_shrug_td_1991_district <- function(x) {
  x <- safe_df(x)
  required <- c(
    "pc91_state_id", "pc91_district_id", "pc91_td_p_7andup", "pc91_td_p_lit_7andup",
    "pc91_td_primary", "pc91_td_hospitals", "pc91_td_banks"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("SHRUG TD91 is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  out <- data.frame(
    state_code_1991 = pad_admin_code(x$pc91_state_id, 2L),
    district_code_1991 = pad_admin_code(x$pc91_district_id, 2L),
    population_urban_7plus_1991 = num(x$pc91_td_p_7andup),
    stringsAsFactors = FALSE
  )
  out$urban_literacy_share_7plus_1991 <- safe_share(x$pc91_td_p_lit_7andup, out$population_urban_7plus_1991)
  out$urban_primary_schools_per_100k_7plus_1991 <- safe_share(x$pc91_td_primary, out$population_urban_7plus_1991, scale = 1e5)
  out$urban_hospitals_per_100k_7plus_1991 <- safe_share(x$pc91_td_hospitals, out$population_urban_7plus_1991, scale = 1e5)
  out$urban_banks_per_100k_7plus_1991 <- safe_share(x$pc91_td_banks, out$population_urban_7plus_1991, scale = 1e5)
  out <- validate_census_1991_district_keys(out, "SHRUG TD91")
  out[c(census_1991_keys(), historical_baseline_1991_metadata()$variable[historical_baseline_1991_metadata()$source == "TD91"])]
}

build_shrug_1991_baseline_controls <- function(sources) {
  required <- c("pca", "vd", "td")
  missing <- setdiff(required, names(sources))
  if (length(missing)) stop("SHRUG 1991 baseline source bundle lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  pca <- clean_shrug_pca_1991_district(sources$pca)
  vd <- clean_shrug_vd_1991_district(sources$vd, pca)
  td <- clean_shrug_td_1991_district(sources$td)
  require_census_1991_subset(vd, pca, "SHRUG VD91")
  require_census_1991_subset(td, pca, "SHRUG TD91")
  out <- Reduce(function(a, b) merge(a, b, by = census_1991_keys(), all.x = TRUE, sort = FALSE), list(pca, vd, td))
  variables <- historical_baseline_1991_variables()
  bounded <- c(
    "female_share_1991", "child_share_0_6_1991", "sc_share_1991", "st_share_1991",
    "literacy_share_7plus_1991", "main_worker_share_1991", "marginal_worker_share_1991",
    "cultivator_share_main_workers_1991", "agricultural_labourer_share_main_workers_1991",
    "urban_literacy_share_7plus_1991"
  )
  invalid <- vapply(bounded, function(variable) {
    value <- num(out[[variable]])
    any(is.finite(value) & (value < 0 | value > 100))
  }, logical(1))
  if (any(invalid)) stop("SHRUG 1991 baseline contains bounded shares outside [0, 100]: ", paste(bounded[invalid], collapse = ", "), call. = FALSE)
  rates <- setdiff(variables, c("log_population_1991", bounded))
  if (any(vapply(rates, function(variable) any(is.finite(num(out[[variable]])) & num(out[[variable]]) < 0), logical(1)))) {
    stop("SHRUG 1991 baseline contains negative facility rates.", call. = FALSE)
  }
  validate_census_1991_district_keys(out, "SHRUG 1991 baseline")
}

summarize_historical_baseline_1991_coverage <- function(baseline) {
  x <- safe_df(baseline)
  metadata <- historical_baseline_1991_metadata()
  safe_bind_rows(lapply(seq_len(nrow(metadata)), function(i) {
    variable <- metadata$variable[[i]]
    value <- num(x[[variable]])
    data.frame(
      metadata[i, , drop = FALSE], n_districts = nrow(x),
      n_nonmissing = sum(is.finite(value)), missing_share = mean(!is.finite(value)),
      stringsAsFactors = FALSE
    )
  }))
}
