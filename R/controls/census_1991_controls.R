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
