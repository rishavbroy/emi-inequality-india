test_that("Tendulkar relatives preserve a common price reference", {
  x <- data.frame(
    state_code = c("00", "00", "10"),
    sector = c("rural", "urban", "rural"),
    poverty_line_rupees = c(816, 1000, 778)
  )
  out <- build_tendulkar_spatial_relatives(x)
  expect_equal(out$spatial_price_relative, c(1, 1000 / 816, 778 / 816))
})

test_that("price links use the median overlap ratio", {
  expect_equal(price_link_factor(c(100, 110, 120), c(200, 220, 360)), 2)
})


test_that("price-sector parsing accepts haven-labelled NSS fields", {
  labelled_sector <- structure(
    c(1, 2),
    labels = c(Rural = 1, Urban = 2),
    class = c("haven_labelled", "vctrs_vctr", "double")
  )

  expect_equal(price_sector(labelled_sector), c("rural", "urban"))
})

test_that("Census control ratios are built from totals", {
  x <- data.frame(
    state_code_2001 = "01",
    district_code_2001 = "01",
    population_total = 1000,
    population_urban = 200,
    population_age_7_plus = 800,
    adult_secondary_plus = 160,
    literate_population = 640,
    sc_population = 100,
    st_population = 50,
    religion_population_total = 1000,
    muslim_population = 150,
    workers_total = 400,
    cultivators = 100,
    agricultural_labourers = 100,
    population_age_0_14 = 300,
    population_age_15_64 = 600,
    population_age_65_plus = 100,
    households_total = 200,
    households_electricity = 120,
    area_sq_km = 10
  )
  out <- build_census_2001_controls(x)
  expect_equal(out$urban_share_2001, 20)
  expect_equal(out$adult_secondary_plus_share_2001, 20)
  expect_equal(out$muslim_share_2001, 15)
  expect_equal(out$agricultural_worker_share_2001, 50)
  expect_equal(out$literacy_share_2001, 80)
  expect_equal(out$worker_share_2001, 40)
  expect_equal(out$cultivator_share_workers_2001, 25)
  expect_equal(out$agricultural_labourer_share_workers_2001, 25)
  expect_equal(out$dependency_ratio_2001, 100 * 400 / 600)
  expect_equal(out$electricity_access_share_2001, 60)
})



test_that("religion shares use the matching C-01 universe", {
  x <- data.frame(
    state_code_2001 = "01", district_code_2001 = "04",
    population_total = 950, population_urban = 100,
    population_age_7_plus = 700, adult_secondary_plus = 140, literate_population = 560,
    sc_population = 50, st_population = 20,
    religion_population_total = 1000, muslim_population = 990,
    workers_total = 400, cultivators = 100, agricultural_labourers = 100,
    population_age_0_14 = 300, population_age_15_64 = 600,
    population_age_65_plus = 100, households_total = 200,
    households_electricity = 120, area_sq_km = 10
  )

  out <- build_census_2001_controls(x)
  expect_equal(out$muslim_share_2001, 99)
})

test_that("bounded Census shares cannot exceed their universes", {
  x <- data.frame(
    state_code_2001 = "01", district_code_2001 = "04",
    population_total = 1000, population_urban = 100,
    population_age_7_plus = 700, adult_secondary_plus = 140, literate_population = 560,
    sc_population = 50, st_population = 20,
    religion_population_total = 900, muslim_population = 950,
    workers_total = 400, cultivators = 100, agricultural_labourers = 100,
    population_age_0_14 = 300, population_age_15_64 = 600,
    population_age_65_plus = 100, households_total = 200,
    households_electricity = 120, area_sq_km = 10
  )

  expect_error(
    build_census_2001_controls(x),
    "muslim_share_2001"
  )
})

test_that("revised formulas use state fixed effects and predetermined controls", {
  f <- build_revised_iv_formulas()
  text <- paste(deparse(f$consumption), collapse = " ")
  expect_match(text, "real_log_consumption_change")
  expect_match(text, "state_code_2001")
  expect_match(text, "urban_share_2001")
  expect_match(text, "emi_exposure_all_children_0708", fixed = TRUE)
  expect_match(text, "ling_distance_nonzero_mean", fixed = TRUE)
  expect_false(grepl("EMIE", text, fixed = TRUE))
  expect_false(grepl("wavg_ling_degrees", text, fixed = TRUE))
  expect_false(grepl("gini_cons_0708", text, fixed = TRUE))
})

test_that("RBI state rural and urban CPI extracts use the general index only", {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    DATAFLOW = "RBI:CPI_RUC_ST_RN(1.0)", BASE_PER = "BY_2012",
    COMD_ITEM = c("C_GIAG", "C_GIAG", "C_GIAG", "C_GIAG_FBT"),
    COVERAGE_GEO_RN = c("RUR", "URB", "ALL_INDIA", "RUR"),
    STATE_CODE = "ANP", TIME_PERIOD = "2013-01-31", OBS_VALUE = c(101, 102, 103, 104)
  ), path, row.names = FALSE)
  out <- read_cpi_ruc_state(path, expected_base = 2012)
  expect_equal(out$sector, c("rural", "urban"))
  expect_equal(out$index, c(101, 102))
  expect_equal(out$period, as.Date(c("2013-01-01", "2013-01-01")))
})

test_that("RBI CPI-AL and CPI-RL rows are distinguished explicitly", {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    DATAFLOW = "RBI:CPI_ALRL_ST_RN(1.0)",
    ELEMENT = c("Consumer Price Index - Agricultural Labourers", "Consumer Price Index - Rural Labourers"),
    STATE_CODE = "ANP", TIME_PERIOD = "2007-07-31", OBS_VALUE = c(410, 415)
  ), path, row.names = FALSE)
  out <- read_cpi_alrl_state(path)
  expect_equal(out$labour_series, c("agricultural_labour", "rural_labour"))
  expect_equal(out$sector, c("rural", "rural"))
})


test_that("RBI CPI-AL/RL reader uses the export's labour-type dimension", {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    DATAFLOW = "RBI:CPI_ALRL_ST_RN(1.0)",
    TYPE_LABOU_RN = c("AL", "RL"),
    STATE_CODE = "ANP", TIME_PERIOD = "2007-07-31", OBS_VALUE = c(410, 415)
  ), path, row.names = FALSE)

  out <- read_cpi_alrl_state(path)
  expect_equal(out$labour_series, c("agricultural_labour", "rural_labour"))
  expect_equal(out$index, c(410, 415))
  expect_false(anyDuplicated(out[c("state_code", "labour_series", "year", "month")]) > 0L)
})


test_that("generic ALRL dataflow labels do not classify rows without a labour type", {
  raw <- data.frame(DATAFLOW = rep("RBI:CPI_ALRL_ST_RN(1.0)", 2))
  expect_true(all(is.na(classify_alrl_series(raw))))
})


test_that("CPI-AL/RL reader rejects unrecognized labour-type values", {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    TYPE_LABOU_RN = c("AL", "OTHER"), STATE_CODE = "ANP",
    TIME_PERIOD = "2007-07-31", OBS_VALUE = c(410, 415)
  ), path, row.names = FALSE)
  expect_error(read_cpi_alrl_state(path), "for every row")
})


test_that("CPI-IW reader retains the published All-India series for explicit fallbacks", {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    BASE_PER = "BY_2001", COMD_ITEM = "GENERAL INDEX",
    CENTRE = c("All India", "Guntur"),
    TIME_PERIOD = c("2007-07-31", "2007-07-31"),
    OBS_VALUE = c(125, 130)
  ), path, row.names = FALSE)

  out <- read_cpi_iw_all_india(path)
  expect_equal(out$state_code, "ALL_INDIA")
  expect_equal(out$sector, "urban")
  expect_equal(out$index, 125)
})



test_that("CPI-IW readers use CENTER_RN rather than the state code in RBI exports", {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    DATAFLOW = "RBI:CPI_IW_AISC_RN(1.0)",
    BASE_PER = "BY_2001",
    CENTER_RN = c("AL_INDIA", "GUNTUR", "VIJAYAWADA"),
    STATE_CODE = c("N_A", "ANP", "ANP"),
    TIME_PERIOD = "2007-07-31",
    OBS_VALUE = c(125, 100, 200)
  ), path, row.names = FALSE)

  all_india <- read_cpi_iw_all_india(path)
  centres <- read_cpi_iw_centres(path)
  expect_equal(all_india$index, 125)
  expect_equal(centres$centre_key, c("GUNTUR", "VIJAYWADA"))
  expect_false(any(centres$centre_key == "ANP"))
})

test_that("CPI-IW reader collapses identical rows but rejects conflicting values", {
  path <- tempfile(fileext = ".csv")
  rows <- data.frame(
    BASE_PER = "BY_2001", CENTER_RN = "GUNTUR", STATE_CODE = "ANP",
    TIME_PERIOD = "2007-07-31", OBS_VALUE = c(100, 100)
  )
  utils::write.csv(rows, path, row.names = FALSE)
  expect_equal(nrow(read_cpi_iw_centres(path)), 1L)

  rows$OBS_VALUE[[2]] <- 101
  utils::write.csv(rows, path, row.names = FALSE)
  expect_error(read_cpi_iw_centres(path), "conflicting duplicate")
})

test_that("CPI-IW aliases match the names used by the RBI extract", {
  source_names <- c(
    "ALWAYE_ERNAKULAM", "COONOR", "DM_TINSUKIA", "HALDI",
    "MONGHYR_JAMALPUR", "MONGHYR", "OTHERS-GOA", "OTHERS_HIMACHAL_PRADESH",
    "OTHERS-TRIPURA", "PONDICHERRY", "QUILLON", "SHOLAPUR",
    "TEZPUR_RANGAPARA", "TRICHIRAPALLY", "TRIVANDRUM"
  )
  metadata_names <- c(
    "Ernakulam", "Coonoor", "D.D.Tinsukia", "Haldia",
    "Monger-Jamalpur", "Monger-Jamalpur", "Goa", "Himachal Pradesh", "Tripura",
    "Puducherry", "Quilon", "Solapur", "Rangapara-Tezpur",
    "Tiruchirapally", "Thiruvananthapuram"
  )
  expect_equal(normalise_cpi_iw_centre(source_names), normalise_cpi_iw_centre(metadata_names))
})

test_that("CPI-IW state aggregation covers historical estimation and link windows", {
  periods <- cpi_iw_state_periods()
  expect_equal(min(periods), as.Date("2004-07-01"))
  expect_equal(max(periods), as.Date("2014-12-01"))
  expect_true(all(seq(as.Date("2004-07-01"), as.Date("2012-12-01"), by = "month") %in% periods))
  expect_true(all(seq(as.Date("2013-01-01"), as.Date("2014-12-01"), by = "month") %in% periods))
  expect_equal(length(periods), length(unique(periods)))
})

test_that("CPI-IW state aggregation windows must be ordered", {
  expect_error(
    cpi_iw_state_periods(as.Date("2010-01-01"), as.Date("2009-01-01")),
    "must be ordered"
  )
})

test_that("CPI-IW state indices use official centre weights", {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    BASE_PER = "BY_2001", COMD_ITEM = "GENERAL INDEX",
    CENTRE = rep(c("Guntur", "Vijayawada"), each = 2),
    TIME_PERIOD = rep(c("2007-07-31", "2007-08-31"), 2),
    OBS_VALUE = c(100, 110, 200, 220)
  ), path, row.names = FALSE)
  centre <- read_cpi_iw_centres(path)
  weights <- data.frame(
    centre_key = normalise_cpi_iw_centre(c("Guntur", "Vijaywada")),
    state_code = "ANP", weight = c(25, 75)
  )
  out <- aggregate_cpi_iw_to_state(centre, weights)
  expect_equal(out$index, c(175, 192.5))
  expect_equal(out$centre_count, c(2L, 2L))
})

test_that("CPI-IW aggregation rejects incomplete centre coverage", {
  centre <- data.frame(
    centre_key = normalise_cpi_iw_centre(c("Guntur", "Vijaywada", "Guntur")),
    period = as.Date(c("2007-07-01", "2007-07-01", "2007-08-01")),
    index = c(100, 200, 110)
  )
  weights <- data.frame(
    centre_key = normalise_cpi_iw_centre(c("Guntur", "Vijaywada")),
    state_code = "ANP", weight = c(25, 75)
  )
  expect_error(aggregate_cpi_iw_to_state(centre, weights), "incomplete centre coverage")
})


test_that("CPI-IW aggregation ignores source centres outside the official weighting system", {
  centre <- data.frame(
    centre_key = normalise_cpi_iw_centre(c("Guntur", "Vijaywada", "Kothagudem")),
    period = as.Date("2005-01-01"),
    index = c(100, 200, 999)
  )
  weights <- data.frame(
    centre_key = normalise_cpi_iw_centre(c("Guntur", "Vijaywada")),
    state_code = "ANP", weight = c(25, 75)
  )
  out <- aggregate_cpi_iw_to_state(centre, weights)
  expect_equal(out$index, 175)
  expect_equal(out$centre_count, 2L)
})

test_that("CPI-IW aggregation can drop globally incomplete source months without renormalizing weights", {
  centre <- data.frame(
    centre_key = normalise_cpi_iw_centre(c(
      "Guntur", "Vijaywada",
      "Guntur",
      "Guntur", "Vijaywada"
    )),
    period = as.Date(c(
      "2012-05-01", "2012-05-01",
      "2012-06-01",
      "2012-07-01", "2012-07-01"
    )),
    index = c(100, 200, 110, 120, 220)
  )
  weights <- data.frame(
    centre_key = normalise_cpi_iw_centre(c("Guntur", "Vijaywada")),
    state_code = "ANP", weight = c(25, 75)
  )
  out <- aggregate_cpi_iw_to_state(
    centre, weights, incomplete_periods = "drop"
  )
  expect_equal(out$period, as.Date(c("2012-05-01", "2012-07-01")))
  expect_false(as.Date("2012-06-01") %in% out$period)
})

test_that("official CPI-Urban fills only missing CPI-IW months after overlap calibration", {
  iw <- data.frame(
    state_code = "A", sector = "urban",
    period = as.Date(c("2011-01-01", "2011-02-01", "2011-04-01", "2011-05-01")),
    index = c(100, 110, 130, 140),
    centre_count = 2L
  )
  donor <- data.frame(
    state_code = "A", sector = "urban",
    period = as.Date(c("2011-01-01", "2011-02-01", "2011-03-01", "2011-04-01", "2011-05-01")),
    index = c(50, 55, 60, 65, 70)
  )
  out <- complete_cpi_iw_state_months(
    iw, donor,
    required_periods = as.Date(c(
      "2011-01-01", "2011-02-01", "2011-03-01", "2011-04-01", "2011-05-01"
    )),
    minimum_overlap_months = 4L
  )
  march <- out[out$period == as.Date("2011-03-01"), ]
  expect_equal(march$index, 120)
  expect_equal(march$cpi_iw_completion, "scaled_cpi_u_2010_gap_fill")
  expect_true(all(out$cpi_iw_completion[out$period != as.Date("2011-03-01")] == "direct_cpi_iw"))
})

test_that("1982-base CPI-IW metadata reproduce the published weighted system and linking contract", {
  weights <- read_cpi_iw_weights("data/metadata/cpi_iw_centres_1982.csv", tolerance = 0.02)
  expect_equal(nrow(weights), 73L)
  expect_equal(sum(weights$all_india_member), 70L)
  expect_equal(sum(weights$weight[weights$all_india_member]), 100.01, tolerance = 1e-10)
  links <- cpi_iw_state_link_factors(weights)
  expect_true(all(positive_finite(links$link_factor_2001)))
  expect_true(all(links$link_weight_coverage > 0 & links$link_weight_coverage <= 1))
  expect_equal(links$link_factor_2001[links$state_code == "DEL"], 5.60)
})

test_that("1982-base CPI-IW state linking changes units but preserves observations", {
  index <- data.frame(
    state_code = "A", sector = "urban", period = as.Date("2005-01-01"),
    index = 460, centre_count = 2L
  )
  links <- data.frame(
    state_code = "A", link_factor_2001 = 4.6,
    link_weight_coverage = 0.8, link_centres = 2L, total_centres = 3L
  )
  out <- link_cpi_iw_state_series(index, links)
  expect_equal(out$index, 100)
  expect_equal(out$link_weight_coverage, 0.8)
  expect_error(
    link_cpi_iw_state_series(index, rbind(links, links)),
    "one row per state"
  )
})

test_that("tracked CPI-IW weights reproduce the 78-centre system", {
  weights <- read_cpi_iw_weights("data/metadata/cpi_iw_centres_2001.csv")
  expect_equal(nrow(weights), 78L)
  expect_equal(sum(weights$weight), 100, tolerance = 1e-10)
  expect_equal(weights$state_code[weights$centre == "Hyderabad"], "ANP")
})

test_that("CPI-IW centre normalization is case and punctuation invariant", {
  expect_equal(
    normalise_cpi_iw_centre(c("Centre One", "CENTRE-ONE", "centre_one")),
    rep("CENTREONE", 3)
  )
  expect_equal(
    normalise_cpi_iw_centre(c("Vijayawada", "Vijaywada")),
    rep("VIJAYWADA", 2)
  )
})

test_that("CPI-IW validation permits equal weights for different centres", {
  weights <- data.frame(
    state_code = c("A", "A"),
    state_name = c("State A", "State A"),
    centre = c("Centre One", "Centre Two"),
    weight = c(50, 50)
  )
  out <- validate_cpi_iw_weights(weights)
  expect_equal(out$weight, c(50, 50))
  expect_equal(out$centre_key, c("CENTREONE", "CENTRETWO"))
})

test_that("CPI-IW validation rejects duplicate normalized centre identities", {
  weights <- data.frame(
    state_code = c("A", "A"),
    state_name = c("State A", "State A"),
    centre = c("Vijayawada", "Vijaywada"),
    weight = c(50, 50)
  )
  expect_error(
    validate_cpi_iw_weights(weights),
    "identities must be unique after name normalization"
  )
})

test_that("temporal price series uses CPI-RL and CPI-IW before 2013", {
  months <- as.Date(c("2012-11-01", "2012-12-01", "2013-01-01", "2013-02-01"))
  sources <- list(
    cpi_alrl = data.frame(
      state_code = "A", labour_series = "rural_labour", sector = "rural",
      period = months, index = c(100, 102, 104, 106)
    ),
    cpi_iw_states = data.frame(
      state_code = "A", sector = "urban", period = months,
      index = c(200, 204, 208, 212)
    ),
    cpi_ruc_2012 = rbind(
      data.frame(state_code = "A", sector = "rural", period = months[3:4], index = c(52, 53)),
      data.frame(state_code = "A", sector = "urban", period = months[3:4], index = c(104, 106))
    )
  )

  out <- build_temporal_price_series(
    sources,
    overlap_start = as.Date("2013-01-01"),
    overlap_end = as.Date("2013-02-01"),
    minimum_link_months = 2
  )$index

  expect_equal(out$price_source[out$period < as.Date("2013-01-01")], c("cpi_rl_state", "cpi_rl_state", "cpi_iw_state", "cpi_iw_state"))
  expect_true(all(out$price_source[out$period >= as.Date("2013-01-01")] %in% c("cpi_rural_2012", "cpi_urban_2012")))
  expect_equal(out$index[out$sector == "rural"], c(50, 51, 52, 53))
  expect_equal(out$index[out$sector == "urban"], c(100, 102, 104, 106))
})

test_that("temporal price links use state-sector overlap medians", {
  months <- as.Date(c("2013-01-01", "2013-02-01", "2013-03-01"))
  old <- rbind(
    data.frame(state_code = "A", sector = "rural", period = months, index = c(100, 110, 120)),
    data.frame(state_code = "A", sector = "urban", period = months, index = c(200, 220, 240))
  )
  new <- rbind(
    data.frame(state_code = "A", sector = "rural", period = months, index = c(200, 220, 360)),
    data.frame(state_code = "A", sector = "urban", period = months, index = c(100, 110, 120))
  )
  links <- summarise_price_links(old, new, months[1], months[3])
  expect_equal(links$link_factor, c(2, 0.5))
  expect_equal(links$link_months, c(3L, 3L))
})

test_that("temporal price construction rejects weak or missing direct links", {
  months <- as.Date(c("2012-12-01", "2013-01-01"))
  sources <- list(
    cpi_alrl = data.frame(
      state_code = "A", labour_series = "rural_labour", sector = "rural",
      period = months, index = c(100, 101)
    ),
    cpi_iw_states = data.frame(
      state_code = "A", sector = "urban", period = months, index = c(100, 101)
    ),
    cpi_ruc_2012 = rbind(
      data.frame(state_code = "A", sector = "rural", period = months[2], index = 100),
      data.frame(state_code = "A", sector = "urban", period = months[2], index = 100)
    )
  )
  expect_error(
    build_temporal_price_series(
      sources,
      overlap_start = months[2], overlap_end = months[2], minimum_link_months = 2
    ),
    "sufficient direct link"
  )
})

test_that("base-2010 and base-2012 CPI-R/U overlap remains a validation result", {
  months <- as.Date(c("2013-01-01", "2013-02-01"))
  sources <- list(
    cpi_ruc_2010 = data.frame(
      state_code = "A", sector = c("rural", "rural"), period = months, index = c(100, 110)
    ),
    cpi_ruc_2012 = data.frame(
      state_code = "A", sector = c("rural", "rural"), period = months, index = c(200, 220)
    )
  )
  out <- summarise_ruc_base_overlap(sources)
  expect_equal(out$link_factor, 2)
  expect_equal(out$link_months, 2L)
})

test_that("official price metadata covers every current state and sector", {
  poverty <- read_tendulkar_poverty_lines()
  rules <- read_price_state_crosswalk()

  expect_equal(nrow(poverty), 72L)
  expect_equal(sort(unique(poverty$sector)), c("rural", "urban"))
  expect_equal(length(unique(poverty$state_code)), 36L)
  expect_equal(nrow(rules), 29L)
  expect_true(all(rules$target_state_code %in% poverty$state_code))

  chandigarh <- poverty[poverty$state_code == "CHD", ]
  expect_equal(chandigarh$poverty_line_rupees, c(1155, 1155))
  expect_true(all(chandigarh$source_state_code == "PUN"))

  telangana <- poverty[poverty$state_code == "TEL", ]
  expect_equal(telangana$poverty_line_rupees, c(860, 1009))
  expect_true(all(telangana$source_state_code == "ANP"))
})

test_that("state rules prefer direct observations and use documented donors only when needed", {
  months <- as.Date(c("2012-12-01", "2013-01-01"))
  temporal <- rbind(
    data.frame(state_code = "ANP", sector = "rural", period = months, index = c(100, 101), price_source = "source"),
    data.frame(state_code = "ANP", sector = "urban", period = months, index = c(200, 202), price_source = "source"),
    data.frame(state_code = "TEL", sector = "rural", period = months[2], index = 111, price_source = "source"),
    data.frame(state_code = "TEL", sector = "urban", period = months[2], index = 222, price_source = "source")
  )
  rules <- data.frame(
    target_state_code = "TEL", source_state_code = "ANP", sector = c("rural", "urban"),
    valid_from = as.Date("1900-01-01"), valid_to = as.Date("2012-12-01"),
    rule_type = "inheritance", reason = "undivided state"
  )

  out <- apply_price_state_rules(temporal, rules)
  tel <- out[out$state_code == "TEL", ]
  expect_equal(tel$index, c(100, 111, 200, 222))
  expect_equal(tel$temporal_state_source, c("ANP", "TEL", "ANP", "TEL"))
  expect_equal(tel$state_rule, c("inheritance", "direct", "inheritance", "direct"))
})

test_that("state rules resolve documented fallback chains", {
  period <- as.Date("2007-07-01")
  temporal <- rbind(
    data.frame(state_code = "ALL_INDIA", sector = "rural", period = period, index = 100, price_source = "all_india"),
    data.frame(state_code = "GOA", sector = "urban", period = period, index = 110, price_source = "direct")
  )
  rules <- data.frame(
    target_state_code = c("GOA", "DADI"),
    source_state_code = c("ALL_INDIA", "GOA"),
    sector = c("rural", "rural"),
    valid_from = as.Date(c("1900-01-01", "1900-01-01")),
    valid_to = as.Date(c(NA_character_, NA_character_)),
    rule_type = c("fallback", "fallback"),
    reason = c("Goa rural uses All India", "Daman and Diu uses Goa")
  )

  out <- apply_price_state_rules(temporal, rules, period, period)
  dadi <- out[out$state_code == "DADI" & out$sector == "rural", ]
  expect_equal(dadi$index, 100)
  expect_equal(dadi$temporal_state_source, "ALL_INDIA")
  expect_equal(dadi$state_rule, "fallback")
  expect_match(dadi$fallback_reason, "Daman and Diu uses Goa -> Goa rural uses All India", fixed = TRUE)
})

test_that("Arunachal urban fallback remains available when later state CPI is missing", {
  rules <- read_price_state_crosswalk()
  rule <- rules[rules$target_state_code == "ARP" & rules$sector == "urban", ]
  expect_equal(nrow(rule), 1L)
  expect_true(is.na(rule$valid_to))
  expect_equal(rule$source_state_code, "ALL_INDIA")
})

test_that("state rules fail rather than inventing an undocumented temporal fallback", {
  temporal <- rbind(
    data.frame(state_code = "A", sector = "rural", period = as.Date("2012-01-01"), index = 100),
    data.frame(state_code = "A", sector = "urban", period = as.Date("2012-01-01"), index = 100)
  )
  rules <- data.frame(
    target_state_code = "B", source_state_code = "C", sector = c("rural", "urban"),
    valid_from = as.Date("1900-01-01"), valid_to = as.Date(NA_character_),
    rule_type = "fallback", reason = "documented donor"
  )
  expect_error(apply_price_state_rules(temporal, rules), "No direct or documented fallback")
})

test_that("state-sector deflators combine temporal change with a common spatial anchor", {
  months <- as.Date(c("2011-07-01", "2011-08-01", "2012-07-01"))
  temporal <- rbind(
    data.frame(state_code = "A", sector = "rural", period = months, index = c(100, 100, 120)),
    data.frame(state_code = "A", sector = "urban", period = months, index = c(200, 200, 220))
  )
  spatial <- build_tendulkar_spatial_relatives(data.frame(
    state_code = c("A", "A"), sector = c("rural", "urban"),
    poverty_line_rupees = c(816, 1000)
  ))
  out <- build_state_sector_deflator(temporal, spatial, months[1:2])

  expect_equal(out$price_deflator[out$sector == "rural"], c(1, 1, 1.2))
  expect_equal(
    out$price_deflator[out$sector == "urban"],
    c(1000 / 816, 1000 / 816, (1000 / 816) * 1.1)
  )
})

test_that("household attachment preserves input row order and fallback provenance", {
  households <- data.frame(
    household_id = c("second", "first"), state = c("B", "A"),
    sector = c("urban", "rural"), period = as.Date(c("2012-01-01", "2012-01-01"))
  )
  deflators <- data.frame(
    state_code = c("A", "B"), sector = c("rural", "urban"),
    period = as.Date(c("2012-01-01", "2012-01-01")), price_deflator = c(1, 2),
    price_source = c("direct", "donor"), temporal_state_source = c("A", "C"),
    state_rule = c("direct", "fallback"), fallback_reason = c(NA, "documented")
  )
  out <- attach_household_deflator(households, deflators, "state", "sector", "period")
  expect_equal(out$household_id, households$household_id)
  expect_equal(out$price_deflator, c(2, 1))
  expect_equal(out$state_rule, c("fallback", "direct"))
})



test_that("price month normalization is vectorized while boundaries stay scalar", {
  expect_equal(
    price_month_start(as.Date(c("2007-07-31", "2007-08-15"))),
    as.Date(c("2007-07-01", "2007-08-01"))
  )
  expect_error(price_month_start(c("2007-07-01", NA)), "valid dates")
  expect_error(price_boundary(as.Date(c("2007-07-01", "2007-08-01"))), "one valid date")
})

test_that("NSS sub-rounds map to consecutive survey-quarter price months", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  spec <- consumption_survey_spec(registry, "nss_2007_08_education")
  periods <- as.Date(c("2007-07-01", "2007-09-01", "2007-10-01", "2008-06-01"))

  expect_equal(survey_subround_for_month(periods, spec), c(1L, 1L, 2L, 4L))
  expect_equal(nss_subround_for_month(periods, 2007, registry), survey_subround_for_month(periods, spec))
  expect_equal(
    nss_subround_for_month(as.Date(c("2017-07-01", "2018-01-01", "2018-06-01")), 2017, registry),
    c(1L, 3L, 4L)
  )
  modern <- consumption_survey_spec(registry, "hces_2022_23")
  expect_error(
    survey_subround_for_month(as.Date("2022-08-01"), modern),
    "does not use quarterly NSS sub-round price timing"
  )
})

test_that("HCES panels map to overlapping three-month survey windows", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  spec22 <- consumption_survey_spec(registry, "hces_2022_23")
  spec23 <- consumption_survey_spec(registry, "hces_2023_24")

  expect_equal(
    survey_panel_months(1, spec22),
    as.Date(c("2022-08-01", "2022-09-01", "2022-10-01"))
  )
  expect_equal(
    survey_panel_months(10, spec22),
    as.Date(c("2023-05-01", "2023-06-01", "2023-07-01"))
  )
  expect_equal(
    survey_panel_months(10, spec23),
    as.Date(c("2024-05-01", "2024-06-01", "2024-07-01"))
  )
  expect_equal(normalise_survey_panel(c("1", "panel 10", "11"), spec22), c(1L, 10L, NA_integer_))
})

test_that("HCES panel deflators average exactly the panel's three monthly indices", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  spec <- consumption_survey_spec(registry, "hces_2022_23")
  months <- seq(as.Date("2022-08-01"), as.Date("2023-07-01"), by = "month")
  d <- data.frame(
    state_code = "BIH",
    sector = "rural",
    period = months,
    price_deflator = seq_along(months),
    spatial_price_relative = 1,
    price_source = "cpi_rural_2012",
    temporal_state_source = "BIH",
    state_rule = "direct",
    fallback_reason = NA_character_,
    stringsAsFactors = FALSE
  )

  out <- build_survey_panel_deflators(d, spec)
  expect_equal(out$panel, 1:10)
  expect_equal(out$price_deflator, 2:11)
  expect_equal(out$period_start[c(1, 10)], as.Date(c("2022-08-01", "2023-05-01")))
  expect_equal(out$period_end[c(1, 10)], as.Date(c("2022-10-01", "2023-07-01")))
})

test_that("registered real-consumption deflation dispatches HCES to panel timing", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  spec <- consumption_survey_spec(registry, "hces_2023_24")
  months <- seq(as.Date("2023-08-01"), as.Date("2024-07-01"), by = "month")
  deflators <- data.frame(
    state_code = "BIH",
    sector = "rural",
    period = months,
    price_deflator = rep(2, length(months)),
    spatial_price_relative = 1,
    price_source = "cpi_rural_2012",
    temporal_state_source = "BIH",
    state_rule = "direct",
    fallback_reason = NA_character_,
    stringsAsFactors = FALSE
  )
  households <- data.frame(
    source_state_code = "10",
    sector = "1",
    subround = "4",
    nominal_mpce = 100,
    nominal_household_consumption = 400,
    household_size = 4,
    survey_weight = 2,
    stringsAsFactors = FALSE
  )

  out <- deflate_detailed_consumption_households(households, deflators, spec)
  expect_equal(out$.price_panel, 4L)
  expect_equal(out$period_start, as.Date("2023-11-01"))
  expect_equal(out$period_end, as.Date("2024-01-01"))
  expect_equal(out$real_mpce, 50)
  expect_equal(out$real_household_consumption, 200)
})

test_that("NSS sub-round deflators average exactly three monthly indices", {
  months <- seq(as.Date("2007-07-01"), as.Date("2007-09-01"), by = "month")
  d <- data.frame(
    state_code = "BIH", sector = "rural", period = months,
    price_deflator = c(1, 2, 3), spatial_price_relative = 1,
    price_source = "cpi_rl_state", temporal_state_source = "BIH",
    state_rule = "direct", fallback_reason = NA_character_
  )
  out <- build_nss_subround_deflators(d, 2007)
  expect_equal(out$price_deflator, 2)
  expect_equal(out$period_start, as.Date("2007-07-01"))
  expect_equal(out$period_end, as.Date("2007-09-01"))
})


test_that("canonical detailed households are deflated before downstream aggregation", {
  registry <- read_consumption_survey_registry()
  spec <- consumption_survey_spec(registry, "nss_2004_05")
  months <- seq(as.Date("2004-07-01"), as.Date("2004-09-01"), by = "month")
  deflators <- data.frame(
    state_code = rep("ANP", 3),
    sector = rep("rural", 3),
    period = months,
    price_deflator = rep(2, 3),
    spatial_price_relative = rep(1, 3),
    price_source = rep("fixture", 3),
    temporal_state_source = rep("ANP", 3),
    state_rule = rep("direct", 3),
    fallback_reason = rep(NA_character_, 3),
    stringsAsFactors = FALSE
  )
  households <- data.frame(
    source_state_code = "28", sector = "Rural", subround = "1",
    nominal_mpce = 100, nominal_household_consumption = 400,
    household_size = 4, survey_weight = 2,
    source_lineage_eligible = TRUE, stringsAsFactors = FALSE
  )
  out <- deflate_detailed_consumption_households(households, deflators, spec)
  expect_equal(out$real_mpce, 50)
  expect_equal(out$real_household_consumption, 200)
  expect_true(out$source_lineage_eligible)
})

test_that("NSS state resolution accepts names, survey codes, and price codes", {
  poverty <- data.frame(
    state_code = c("BIH", "TEL"), state_name = c("Bihar", "Telangana"),
    sector = "rural", poverty_line_rupees = 1
  )
  expect_equal(
    resolve_nss_price_state(c("Bihar", "10", "TEL", "36"), poverty),
    c("BIH", "BIH", "TEL", "TEL")
  )
})

test_that("household deflation occurs before district aggregation", {
  months <- seq(as.Date("2017-07-01"), as.Date("2017-09-01"), by = "month")
  deflators <- data.frame(
    state_code = "BIH", sector = "rural", period = months,
    price_deflator = c(2, 2, 2), spatial_price_relative = 1,
    price_source = "cpi_rural_2012", temporal_state_source = "BIH",
    state_rule = "direct", fallback_reason = NA_character_
  )
  households <- data.frame(
    State = c("Bihar", "Bihar"), Sector = 1, Sub_Round = 1,
    District = c("Patna", "Patna"), HHID = c("a", "b"),
    HH_Con_exp_rs = c(200, 600), Household_size = c(2, 3),
    MULT_Combined = c(1, 1)
  )
  prepared <- prepare_2017_consumption_households(list(block = households), deflators)
  expect_equal(prepared$consumption_real_pc, c(50, 100))

  out <- build_2017_measures(list(block = households), list(), prepared)
  expect_equal(out$consumption_1718, 160)
  expect_equal(out$real_consumption_1718, 80)
  expect_equal(out$price_fallback_household_share_1718, 0)
})

test_that("real district means use person rather than household weights", {
  households <- data.frame(
    district_code_0708 = c("1001", "1001"),
    consumption_nominal_total = c(100, 900),
    consumption_real_total = c(50, 450),
    household_size_price = c(1, 3), survey_weight_price = c(1, 1),
    price_deflator = c(2, 2), state_rule = c("direct", "fallback")
  )
  out <- aggregate_consumption_households(households, "district_code_0708", "0708")
  expect_equal(out$consumption_0708, 250)
  expect_equal(out$real_consumption_0708, 125)
  expect_equal(out$consumption_0708_household_weighted, 200)
  expect_equal(out$price_fallback_household_share_0708, 50)
})

test_that("price manifest exposes the four production CPI inputs", {
  manifest <- read_manifest(build_paths(Sys.getenv("EMI_PROJECT_ROOT")))
  ids <- c("price_cpi_alrl_state", "price_cpi_iw_centres", "price_cpi_ruc_2010", "price_cpi_ruc_2012")
  rows <- manifest[manifest$file_id %in% ids, , drop = FALSE]
  expect_equal(nrow(rows), 4L)
  expect_true(all(tolower(rows$required_for_current_pipeline) == "true"))
})


test_that("price-source adapters return one named path per production series", {
  paths <- c(
    cpi_alrl = "alrl.csv",
    cpi_iw = "iw.csv",
    cpi_ruc_2010 = "ruc2010.csv",
    cpi_ruc_2012 = "ruc2012.csv"
  )
  out <- validate_price_source_paths(paths)
  expect_type(out, "list")
  expect_named(out, names(paths))
  expect_equal(unlist(out, use.names = FALSE), unname(paths))
  expect_error(validate_price_source_paths(unname(paths)), "must be named")
})


test_that("pre-2013 selector binds state and All-India CPI-IW schemas by contract", {
  sources <- list(
    cpi_alrl = data.frame(
      state_code = "A", labour_series = "rural_labour", sector = "rural",
      period = as.Date("2007-07-01"), index = 90, source_file = "alrl.csv"
    ),
    cpi_iw_states = data.frame(
      state_code = "A", sector = "urban", period = as.Date("2007-07-01"),
      index = 100, centre_count = 2L
    ),
    cpi_iw_all_india = data.frame(
      state_code = "ALL_INDIA", sector = "urban", period = as.Date("2007-07-01"),
      index = 105, year = 2007L, month = 7L, source_file = "iw.csv"
    )
  )

  out <- select_pre_2013_price_series(sources)

  expect_named(out, c("state_code", "sector", "period", "index", "price_source"))
  expect_equal(out$state_code, c("A", "A", "ALL_INDIA"))
  expect_equal(out$price_source, c("cpi_rl_state", "cpi_iw_state", "cpi_iw_state"))
})

test_that("production temporal series retains only the requested pre-switch window", {
  months <- seq(as.Date("2007-07-01"), as.Date("2013-02-01"), by = "month")
  old <- data.frame(state_code = "A", period = months, index = seq_along(months))
  sources <- list(
    cpi_alrl = transform(old, labour_series = "rural_labour", sector = "rural"),
    cpi_iw_states = transform(old, sector = "urban"),
    cpi_ruc_2012 = rbind(
      data.frame(state_code = "A", sector = "rural", period = months[months >= as.Date("2013-01-01")], index = c(100, 101)),
      data.frame(state_code = "A", sector = "urban", period = months[months >= as.Date("2013-01-01")], index = c(100, 101))
    )
  )
  out <- build_temporal_price_series(
    sources,
    overlap_start = as.Date("2013-01-01"), overlap_end = as.Date("2013-02-01"),
    minimum_link_months = 2L,
    pre_switch_start = as.Date("2007-07-01"), pre_switch_end = as.Date("2008-06-01")
  )$index
  pre <- out[out$period < as.Date("2013-01-01"), ]
  expect_equal(range(pre$period), as.Date(c("2007-07-01", "2008-06-01")))
})

test_that("R/U reference index uses all requested months and the overlap link", {
  reference <- seq(as.Date("2011-07-01"), as.Date("2012-06-01"), by = "month")
  overlap <- as.Date(c("2013-01-01", "2013-02-01"))
  old <- rbind(
    data.frame(state_code = "A", sector = "rural", period = c(reference, overlap), index = c(rep(50, 12), 50, 50)),
    data.frame(state_code = "A", sector = "urban", period = c(reference, overlap), index = c(rep(80, 12), 80, 80))
  )
  new <- rbind(
    data.frame(state_code = "A", sector = "rural", period = overlap, index = c(100, 100)),
    data.frame(state_code = "A", sector = "urban", period = overlap, index = c(120, 120))
  )
  poverty <- data.frame(state_code = "A", sector = c("rural", "urban"))
  rules <- data.frame(
    target_state_code = character(), source_state_code = character(), sector = character(),
    valid_from = as.Date(character()), valid_to = as.Date(character()),
    rule_type = character(), reason = character()
  )
  out <- build_ruc_reference_index(
    list(cpi_ruc_2010 = old, cpi_ruc_2012 = new), reference,
    state_rules = rules, poverty_lines = poverty
  )
  expect_equal(out$reference_index, c(100, 120))
  expect_equal(out$reference_months, c(12L, 12L))
})

test_that("reference prices inherit the documented poverty-line source when direct history is absent", {
  index <- data.frame(
    state_code = "TEL", sector = "urban", period = as.Date("2017-07-01"), index = 150
  )
  spatial <- data.frame(
    state_code = "TEL", sector = "urban", spatial_price_relative = 1.2,
    source_state_code = "ANP"
  )
  reference <- data.frame(state_code = "ANP", sector = "urban", reference_index = 100)
  out <- build_state_sector_deflator(index, spatial, reference_index = reference)
  expect_equal(out$temporal_price_relative, 1.5)
  expect_equal(out$price_deflator, 1.8)
})


test_that("R/U reference index uses a documented donor for incomplete state history", {
  reference <- seq(as.Date("2011-07-01"), as.Date("2012-06-01"), by = "month")
  overlap <- as.Date(c("2013-01-01", "2013-02-01"))
  old <- data.frame(
    state_code = "ALL_INDIA", sector = "urban", period = c(reference, overlap),
    index = c(rep(80, 12), 80, 80)
  )
  new <- data.frame(
    state_code = "ALL_INDIA", sector = "urban", period = overlap, index = c(120, 120)
  )
  poverty <- data.frame(state_code = c("ALL_INDIA", "ARP"), sector = "urban")
  rules <- data.frame(
    target_state_code = "ARP", source_state_code = "ALL_INDIA", sector = "urban",
    valid_from = as.Date("1900-01-01"), valid_to = as.Date("2012-12-01"),
    rule_type = "fallback", reason = "No complete state reference series"
  )
  out <- build_ruc_reference_index(
    list(cpi_ruc_2010 = old, cpi_ruc_2012 = new), reference,
    state_rules = rules, poverty_lines = poverty
  )
  arunachal <- out[out$state_code == "ARP", ]
  expect_equal(arunachal$reference_index, 120)
  expect_equal(arunachal$reference_state_source, "ALL_INDIA")
  expect_equal(arunachal$reference_rule, "fallback")
})

test_that("consumption comparison formulas define the planned fixed-sample outcomes", {
  formulas <- build_consumption_outcome_comparison_formulas()
  expect_equal(names(formulas), c(
    "nominal_log_change", "real_log_change_preferred", "real_ancova"
  ))
  expect_match(paste(deparse(formulas$nominal_log_change), collapse = " "), "log_consumption_difference")
  expect_match(paste(deparse(formulas$real_log_change_preferred), collapse = " "), "real_log_consumption_change")
  ancova <- paste(deparse(formulas$real_ancova), collapse = " ")
  expect_match(ancova, "log_real_consumption_1718")
  expect_match(ancova, "log_real_consumption_0708")
  expect_false(any(vapply(formulas, function(x) "consumption_0708" %in% all.vars(x), logical(1))))
  expect_equal(
    sum(all.vars(formulas$real_ancova) == "log_real_consumption_0708"),
    1L
  )
  expect_true(all(vapply(formulas, function(x) "state_code_2001" %in% all.vars(x), logical(1))))
})

test_that("consumption outcome comparison excludes the legacy baseline level control", {
  controls <- consumption_outcome_comparison_controls()
  expect_false("consumption_0708" %in% controls)
  expect_true(all(setdiff(legacy_2007_iv_controls(), "consumption_0708") %in% controls))
})

test_that("consumption comparison sample is common across all specifications", {
  formulas <- list(
    nominal = y1 ~ x | z,
    real = y2 ~ x | z,
    ancova = y3 ~ x + y0 | z + y0
  )
  panel <- data.frame(
    y0 = c(1, 1, 1), y1 = c(1, 2, 3), y2 = c(1, NA, 3),
    y3 = c(2, 3, 4), x = 1:3, z = 2:4
  )
  out <- consumption_outcome_common_sample(panel, formulas)
  expect_equal(nrow(out), 2L)
  expect_equal(out$x, c(1, 3))
})

test_that("household price diagnostics report direct and fallback weighted shares", {
  households <- data.frame(
    .price_state_code = c("A", "A"), .price_sector = c("rural", "rural"),
    .price_subround = c(1L, 1L), survey_weight_price = c(1, 3),
    price_deflator = c(1, 2), state_rule = c("direct", "fallback"),
    temporal_state_source = c("A", "B")
  )
  out <- summarise_household_price_assignments(households, "nss_2007_08")
  expect_equal(sum(out$survey_weight_share_pct), 100)
  expect_equal(out$survey_weight_share_pct[out$assignment_type == "fallback_or_inheritance"], 75)
  expect_equal(unique(out$period_type), "subround")
  expect_equal(unique(out$period_group), 1L)
  expect_equal(unique(out$round_id), "nss_2007_08")
})

test_that("household price diagnostics use modern HCES panel timing and canonical weights", {
  households <- data.frame(
    .price_state_code = c("JNK", "JNK"),
    .price_sector = c("rural", "rural"),
    .price_panel = c(4L, 4L),
    survey_weight = c(2, 6),
    price_deflator = c(2, 2),
    state_rule = c("direct", "direct"),
    temporal_state_source = c("JNK", "JNK"),
    stringsAsFactors = FALSE
  )

  out <- summarise_household_price_assignments(households, "hces_2023_24")
  expect_equal(nrow(out), 1L)
  expect_equal(out$period_type, "panel")
  expect_equal(out$period_group, 4L)
  expect_equal(out$survey_weight, 8)
  expect_equal(out$survey_weight_share_pct, 100)
  expect_equal(out$round_id, "hces_2023_24")
})

test_that("household price diagnostics reject ambiguous price-period contracts", {
  households <- data.frame(
    .price_state_code = "A", .price_sector = "rural",
    .price_subround = 1L, .price_panel = 1L,
    survey_weight = 1,
    price_deflator = 1,
    state_rule = "direct",
    temporal_state_source = "A"
  )
  expect_error(
    summarise_household_price_assignments(households, "bad_round"),
    "exactly one registered price-period field"
  )
})

test_that("Census source cleaners use standardized state-district keys", {
  pca <- data.frame(
    pc01_state_id = 1, pc01_district_id = 2, pc01_pca_no_hh = 10,
    pc01_pca_tot_p = 100, pc01_pca_p_06 = 20, pc01_pca_p_sc = 5,
    pc01_pca_p_st = 7, pc01_pca_p_lit = 60, pc01_pca_tot_work_p = 40,
    pc01_pca_main_cl_p = 5, pc01_pca_main_al_p = 6,
    pc01_pca_marg_cl_p = 1, pc01_pca_marg_al_p = 2
  )
  out <- clean_shrug_pca_2001_district(pca)
  expect_equal(out$state_code_2001, "01")
  expect_equal(out$district_code_2001, "02")
  expect_equal(out$cultivators, 6)
  expect_equal(out$agricultural_labourers, 8)
})

test_that("Census controls attach without changing panel rows", {
  totals <- data.frame(
    state_code_2001 = c("01", "01"), district_code_2001 = c("01", "02"),
    population_total = c(100, 200), population_urban = c(20, 50),
    population_age_7_plus = c(80, 150), adult_secondary_plus = c(10, 30),
    literate_population = c(60, 120),
    sc_population = c(5, 10), st_population = c(2, 4),
    religion_population_total = c(100, 200), muslim_population = c(8, 20),
    workers_total = c(40, 80), cultivators = c(10, 20), agricultural_labourers = c(5, 10),
    population_age_0_14 = c(30, 60), population_age_15_64 = c(60, 120),
    population_age_65_plus = c(10, 20), households_total = c(20, 40),
    households_electricity = c(15, 30), area_sq_km = c(10, 20)
  )
  controls <- build_census_2001_controls(totals)
  panel <- data.frame(state_code_2001 = c("01", "01"), district_code_2001 = c("02", "01"), y = 1:2)
  out <- attach_census_2001_controls(panel, controls)
  expect_equal(out$district_code_2001, panel$district_code_2001)
  expect_equal(nrow(out), nrow(panel))
  expect_true(all(census_2001_main_controls() %in% names(out)))
})


test_that("Census controls preserve sf geometry and CRS", {
  testthat::skip_if_not_installed("sf")

  totals <- data.frame(
    state_code_2001 = c("01", "01"), district_code_2001 = c("01", "02"),
    population_total = c(100, 200), population_urban = c(20, 50),
    population_age_7_plus = c(80, 150), adult_secondary_plus = c(10, 30),
    literate_population = c(60, 120),
    sc_population = c(5, 10), st_population = c(2, 4),
    religion_population_total = c(100, 200), muslim_population = c(8, 20),
    workers_total = c(40, 80), cultivators = c(10, 20), agricultural_labourers = c(5, 10),
    population_age_0_14 = c(30, 60), population_age_15_64 = c(60, 120),
    population_age_65_plus = c(10, 20), households_total = c(20, 40),
    households_electricity = c(15, 30), area_sq_km = c(10, 20)
  )
  controls <- build_census_2001_controls(totals)
  geometry <- sf::st_sfc(sf::st_point(c(1, 1)), sf::st_point(c(2, 2)), crs = 4326)
  panel <- sf::st_sf(
    state_code_2001 = c("01", "01"),
    district_code_2001 = c("02", "01"),
    y = 1:2,
    geometry = geometry
  )

  out <- attach_census_2001_controls(panel, controls)

  expect_s3_class(out, "sf")
  expect_identical(sf::st_crs(out), sf::st_crs(panel))
  expect_identical(sf::st_geometry(out), sf::st_geometry(panel))
  expect_equal(out$district_code_2001, panel$district_code_2001)
  expect_equal(nrow(out), nrow(panel))
})

test_that("Census count sources must share the same district universe", {
  a <- data.frame(
    state_code_2001 = c("01", "01"),
    district_code_2001 = c("01", "02"),
    population_total = c(10, 20)
  )
  b <- data.frame(
    state_code_2001 = "01",
    district_code_2001 = "01",
    muslim_population = 2
  )
  expect_error(
    combine_census_2001_count_sources(list(a, b), require_same_keys = TRUE),
    "same state-district universe"
  )
})


test_that("Census district validation enforces the declared universe size", {
  x <- data.frame(
    state_code_2001 = c("01", "01"),
    district_code_2001 = c("01", "02")
  )
  expect_error(
    validate_census_district_rows(x, "test source", expected_n = 3L),
    "2 districts; expected 3"
  )
})


test_that("Census geometry derives standardized keys from canonical unit IDs", {
  geometry <- data.frame(
    unit_id = c("pc2001__01__02", "pc2001__20__16"),
    stringsAsFactors = FALSE
  )
  out <- census_geometry_keys(geometry)
  expect_equal(out$state_code_2001, c("01", "20"))
  expect_equal(out$district_code_2001, c("02", "16"))
})


test_that("Census geometry rejects malformed or duplicate canonical unit IDs", {
  expect_error(
    census_geometry_keys(data.frame(unit_id = "pc2001__1__02")),
    "malformed"
  )
  expect_error(
    census_geometry_keys(data.frame(unit_id = rep("pc2001__01__02", 2L))),
    "not unique"
  )
})


test_that("preferred public outcome is real log consumption growth", {
  panel <- data.frame(
    EMIE = c(10, 20),
    wavg_ling_degrees = c(1, 2),
    real_log_consumption_change = c(0.1, 0.2),
    real_consumption_0708 = c(100, 110),
    real_consumption_1718 = c(111, 134),
    stringsAsFactors = FALSE
  )
  figures <- make_figures(panel, character(), list(mode = "draft"))
  expect_identical(figures$map_consumption_growth$variable, "real_log_consumption_change")
})

test_that("real-consumption deflation prefers an explicit historical price-state key", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  spec <- consumption_survey_spec(registry, "hces_2023_24")
  months <- seq(as.Date("2023-08-01"), as.Date("2024-07-01"), by = "month")

  deflators <- data.frame(
    state_code = "JNK",
    sector = "rural",
    period = months,
    price_deflator = rep(2, length(months)),
    spatial_price_relative = 1,
    price_source = "test",
    temporal_state_source = "JNK",
    state_rule = "direct",
    fallback_reason = NA_character_,
    stringsAsFactors = FALSE
  )
  households <- data.frame(
    source_state_code = "37",
    price_state_code = "JNK",
    sector = "1",
    subround = "1",
    nominal_mpce = 100,
    nominal_household_consumption = 400,
    household_size = 4,
    survey_weight = 1,
    stringsAsFactors = FALSE
  )

  out <- deflate_detailed_consumption_households(households, deflators, spec)
  expect_equal(out$.price_state_code, "JNK")
  expect_equal(out$real_mpce, 50)
  expect_equal(out$real_household_consumption, 200)
})

test_that("unresolved price-key errors identify the failing key and raw value", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  spec <- consumption_survey_spec(registry, "hces_2023_24")
  months <- seq(as.Date("2023-08-01"), as.Date("2024-07-01"), by = "month")
  deflators <- data.frame(
    state_code = "JNK",
    sector = "rural",
    period = months,
    price_deflator = 1,
    spatial_price_relative = 1,
    price_source = "test",
    temporal_state_source = "JNK",
    state_rule = "direct",
    fallback_reason = NA_character_,
    stringsAsFactors = FALSE
  )
  households <- data.frame(
    source_state_code = "37",
    sector = "1",
    subround = "1",
    nominal_mpce = 100,
    nominal_household_consumption = 400,
    household_size = 4,
    survey_weight = 1,
    stringsAsFactors = FALSE
  )

  expect_error(
    deflate_detailed_consumption_households(households, deflators, spec),
    "state=1 \\[37\\]"
  )
})

test_that("consumption price-window validation catches truncated production tables early", {
  window <- data.frame(
    start_period = as.Date("2023-08-01"),
    end_period = as.Date("2023-10-01"),
    stringsAsFactors = FALSE
  )
  d <- data.frame(
    state_code = "JNK",
    sector = "rural",
    period = as.Date(c("2023-08-01", "2023-09-01")),
    price_deflator = c(1, 1.1),
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_consumption_price_window(d, window),
    "2023-10-01"
  )
})

test_that("consumption price-window validation accepts complete monthly coverage", {
  window <- data.frame(
    start_period = as.Date("2023-08-01"),
    end_period = as.Date("2023-10-01"),
    stringsAsFactors = FALSE
  )
  d <- data.frame(
    state_code = "JNK",
    sector = "rural",
    period = seq(as.Date("2023-08-01"), as.Date("2023-10-01"), by = "month"),
    price_deflator = c(1, 1.1, 1.2),
    stringsAsFactors = FALSE
  )

  out <- validate_consumption_price_window(d, window)
  expect_equal(out, d)
})
