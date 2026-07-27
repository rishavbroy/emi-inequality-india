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
