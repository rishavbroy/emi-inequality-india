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

test_that("Census control ratios are built from totals", {
  x <- data.frame(
    district_code_2001 = "001",
    population_total = 1000,
    population_urban = 200,
    population_age_7_plus = 800,
    adult_secondary_plus = 160,
    sc_population = 100,
    st_population = 50,
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
  expect_equal(out$agricultural_worker_share_2001, 50)
  expect_equal(out$dependency_ratio_2001, 100 * 400 / 600)
  expect_equal(out$electricity_access_share_2001, 60)
})

test_that("revised formulas use state fixed effects and predetermined controls", {
  f <- build_revised_iv_formulas()
  text <- paste(deparse(f$consumption), collapse = " ")
  expect_match(text, "real_log_consumption_change")
  expect_match(text, "state_2001")
  expect_match(text, "urban_share_2001")
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
