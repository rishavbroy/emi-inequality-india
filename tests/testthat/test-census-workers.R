test_that("Census B04 industrial categories partition main workers exactly", {
  raw <- data.frame(matrix("", nrow = 1, ncol = 64), stringsAsFactors = FALSE)
  raw[1, 1:6] <- c("B0104", "09", "132", "District - Alpha (132)", "Total", "Total")
  raw[1, c(10, 13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46, 49, 52, 55, 58, 61, 64)] <-
    c(10, 20, 5, 3, 4, 6, 2, 8, 4, 6, 7, 5, 2, 3, 9, 4, 3, 4, 5)
  raw[1, 7] <- sum(as.numeric(unlist(raw[1, c(10, 13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46, 49, 52, 55, 58, 61, 64)], use.names = FALSE)))

  out <- validate_census_industry_partition(parse_census_b04_2011_sheet(raw), "main")
  expect_equal(out$main_workers_total, 110)
  expect_equal(out$main_manufacturing, 10)
  expect_equal(out$main_accommodation_food, 7)
  expect_equal(out$main_transport, 5)
  expect_equal(out$main_information_communication, 5)

  raw[1, 7] <- 109
  expect_error(
    validate_census_industry_partition(parse_census_b04_2011_sheet(raw), "main"),
    "do not sum exactly"
  )
})

test_that("Census B06 combines marginal-work durations before industry shares", {
  raw <- data.frame(matrix("", nrow = 1, ncol = 67), stringsAsFactors = FALSE)
  raw[1, 1:6] <- c("B0706", "09", "132", "Alpha", "Total", "Total")
  raw[1, 7] <- 70
  raw[1, 10] <- 30
  raw[1, c(13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46, 49, 52, 55, 58, 61, 64, 67)] <-
    c(10, 20, 5, 3, 4, 6, 2, 8, 4, 6, 7, 5, 2, 3, 4, 3, 3, 2, 3)

  out <- validate_census_industry_partition(parse_census_b06_2011_sheet(raw), "marginal")
  expect_equal(out$marginal_workers_total, 100)
  expect_equal(out$marginal_manufacturing, 10)
  expect_equal(out$marginal_accommodation_food, 7)
  expect_equal(out$marginal_transport, 5)
  expect_equal(out$marginal_workers_3_6_months + out$marginal_workers_less_than_3_months, 100)
})

test_that("Census B25 top-level occupation divisions exhaust the table universe", {
  divisions <- c("0", census_occupation_divisions())
  raw <- data.frame(matrix("", nrow = length(divisions), ncol = 10), stringsAsFactors = FALSE)
  raw[, 1] <- "B0425A"
  raw[, 2] <- "09"
  raw[, 3] <- "132"
  raw[, 4] <- "Alpha"
  raw[, 5] <- divisions
  raw[, 6] <- "00"
  raw[, 7] <- "000"
  raw[, 8] <- "0000"
  raw[, 9] <- c("TOTAL", paste("Division", census_occupation_divisions()))
  raw[, 10] <- c(100, 5, 10, 15, 10, 10, 0, 10, 10, 25, 5)

  out <- summarise_census_b25_2011_district(parse_census_b25_2011_sheet(raw, "main"), "main")
  expect_equal(out$main_workers_excl_cultivators_aglab_total, 100)
  expect_equal(out$main_occupation_division_2, 10)
  expect_equal(out$main_occupation_division_x, 5)

  omitted_zero <- raw[raw[, 5] != "6", , drop = FALSE]
  out_zero <- summarise_census_b25_2011_district(
    parse_census_b25_2011_sheet(omitted_zero, "main"), "main"
  )
  expect_equal(out_zero$main_occupation_division_6, 0)

  raw[raw[, 5] == "9", 10] <- 24
  expect_error(
    summarise_census_b25_2011_district(parse_census_b25_2011_sheet(raw, "main"), "main"),
    "do not sum exactly"
  )
})

test_that("B04/B25A and B06/B25B enforce their published worker universes", {
  b04 <- data.frame(
    state_code = "09", district_code = "132", main_workers_total = 100,
    main_cultivators = 20, main_agricultural_labourers = 30,
    stringsAsFactors = FALSE
  )
  b25a <- data.frame(
    state_code = "09", district_code = "132",
    main_workers_excl_cultivators_aglab_total = 50,
    stringsAsFactors = FALSE
  )
  expect_equal(validate_census_2011_b04_b25a_universe(b04, b25a)$max_abs_difference, 0)

  b06 <- data.frame(
    state_code = "09", district_code = "132", marginal_workers_total = 80,
    marginal_cultivators = 10, marginal_agricultural_labourers = 40,
    stringsAsFactors = FALSE
  )
  b25b <- data.frame(
    state_code = "09", district_code = "132",
    marginal_workers_excl_cultivators_aglab_total = 30,
    stringsAsFactors = FALSE
  )
  expect_equal(validate_census_2011_b06_b25b_universe(b06, b25b)$max_abs_difference, 0)

  b25b$marginal_workers_excl_cultivators_aglab_total <- 29
  expect_error(validate_census_2011_b06_b25b_universe(b06, b25b), "counts disagree")
})

test_that("Census worker counts are pooled before economic-structure shares", {
  b04 <- data.frame(
    state_code = c("09", "09"), district_code = c("132", "133"), district_name = c("A", "B"),
    main_workers_total = c(80, 120),
    stringsAsFactors = FALSE
  )
  b06 <- data.frame(
    state_code = c("09", "09"), district_code = c("132", "133"), district_name = c("A", "B"),
    marginal_workers_total = c(20, 80),
    stringsAsFactors = FALSE
  )
  for (column in setdiff(census_industry_count_columns("main"), "main_workers_total")) {
    b04[[column]] <- 0
  }
  for (column in setdiff(census_industry_count_columns("marginal"), "marginal_workers_total")) {
    b06[[column]] <- 0
  }
  b04$main_cultivators <- c(40, 30)
  b04$main_information_communication <- c(40, 90)
  b06$marginal_cultivators <- c(10, 40)
  b06$marginal_information_communication <- c(10, 40)
  transition <- data.frame(
    state_code_2011 = c("09", "09"), district_code_2011 = c("132", "133"),
    state_code_2001 = c("09", "09"), district_code_2001 = c("01", "01"),
    population_share_to_2001 = 1, area_share_to_2001 = 1, shrid_coverage = 1,
    mapping_class = "deterministic_containment", stringsAsFactors = FALSE
  )

  out <- build_census_2011_industry_measures(b04, b06, transition)
  expect_equal(nrow(out), 1L)
  expect_equal(out$workers_total, 300)
  expect_equal(out$industry_information_communication, 180)
  expect_equal(out$information_communication_share_among_workers, 0.6)
  expect_equal(out$census_2011_source_district_count, 2L)
})

test_that("Census occupation measures retain not-classified workers in the denominator", {
  a <- data.frame(
    state_code = "09", district_code = "132", district_name = "Alpha",
    main_workers_excl_cultivators_aglab_total = 80, stringsAsFactors = FALSE
  )
  b <- data.frame(
    state_code = "09", district_code = "132", district_name = "Alpha",
    marginal_workers_excl_cultivators_aglab_total = 20, stringsAsFactors = FALSE
  )
  for (division in c(as.character(1:9), "x")) {
    a[[paste0("main_occupation_division_", division)]] <- 0
    b[[paste0("marginal_occupation_division_", division)]] <- 0
  }
  a$main_occupation_division_1 <- 10
  a$main_occupation_division_2 <- 20
  a$main_occupation_division_3 <- 10
  a$main_occupation_division_9 <- 30
  a$main_occupation_division_x <- 10
  b$marginal_occupation_division_2 <- 5
  b$marginal_occupation_division_9 <- 10
  b$marginal_occupation_division_x <- 5

  source <- build_census_2011_occupation_source(a, b)
  out <- add_census_occupation_shares(source)
  expect_equal(out$workers_excl_cultivators_aglab_total, 100)
  expect_equal(out$manager_professional_technical_share, 0.45)
  expect_equal(out$elementary_occupation_share, 0.4)
  expect_equal(out$occupation_not_classified_share, 0.15)
})


test_that("Census 2001 B04 industrial categories partition main workers exactly", {
  raw <- data.frame(matrix("", nrow = 1, ncol = 49), stringsAsFactors = FALSE)
  raw[1, 1:7] <- c("B0104", "09", "01", "0000", "District - Alpha 01", "Total", "Total")
  raw[1, c(11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47)] <-
    c(20, 10, 5, 5, 10, 5, 5, 10, 10, 5, 5, 5, 5)
  raw[1, 8] <- sum(as.numeric(unlist(
    raw[1, c(11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47)],
    use.names = FALSE
  )))

  out <- parse_census_b04_2001_sheet(raw)
  expect_equal(out$main_workers_total, 100)
  expect_equal(out$main_agriculture, 35)
  expect_equal(out$main_manufacturing, 15)

  raw[1, 8] <- 99
  expect_error(parse_census_b04_2001_sheet(raw), "do not sum exactly")
})

test_that("Census 2001 B25 and B26 retain and exhaust unclassified occupations", {
  divisions <- c("0", as.character(1:9), "X")

  raw25 <- data.frame(matrix("", nrow = length(divisions), ncol = 8), stringsAsFactors = FALSE)
  raw25[, 1] <- "B0425"
  raw25[, 2] <- "District - Alpha * 01"
  raw25[, 3] <- divisions
  raw25[, 4] <- ifelse(divisions == "X", "X9", "00")
  raw25[, 5] <- "000"
  raw25[, 6] <- "0000"
  raw25[, 7] <- c("TOTAL", paste("Division", divisions[-1]))
  raw25[, 8] <- c(100, 5, 10, 15, 10, 10, 5, 10, 10, 20, 5)

  b25 <- summarise_census_b25_2001_district(
    parse_census_b25_2001_sheet(raw25, "09")
  )
  expect_equal(b25$main_workers_excl_cultivators_aglab_total, 100)
  expect_equal(b25$main_occupation_division_x, 5)

  raw26 <- data.frame(matrix("", nrow = length(divisions), ncol = 10), stringsAsFactors = FALSE)
  raw26[, 1] <- "B26"
  raw26[, 2] <- "District - Alpha * 01"
  raw26[, 3] <- divisions
  raw26[, 4] <- ifelse(divisions == "X", "X9", "00")
  raw26[, 5] <- "Total"
  raw26[, 6] <- "Total"
  raw26[, 7] <- raw25[, 8]
  raw26[, 10] <- c(50, 2, 3, 5, 5, 5, 5, 5, 5, 10, 5)

  b26 <- summarise_census_b26_2001_district(
    parse_census_b26_2001_sheet(raw26, "09")
  )
  expect_equal(b26$main_workers_excl_cultivators_aglab_total, 100)
  expect_equal(b26$marginal_workers_excl_cultivators_aglab_total, 50)
  expect_equal(b26$marginal_occupation_division_x, 5)

  validation <- validate_census_2001_b25_b26_main_occupation(b25, b26)
  expect_true(all(validation$max_abs_difference == 0))
})

test_that("Census 2001 occupation shares use combined main and marginal counts", {
  x <- data.frame(
    state_code = "09", district_code = "01", district_name = "Alpha",
    main_workers_excl_cultivators_aglab_total = 80,
    marginal_workers_excl_cultivators_aglab_total = 20,
    stringsAsFactors = FALSE
  )
  for (division in c(as.character(1:9), "x")) {
    x[[paste0("main_occupation_division_", division)]] <- 0
    x[[paste0("marginal_occupation_division_", division)]] <- 0
  }
  x$main_occupation_division_1 <- 20
  x$main_occupation_division_2 <- 20
  x$main_occupation_division_9 <- 30
  x$main_occupation_division_x <- 10
  x$marginal_occupation_division_2 <- 5
  x$marginal_occupation_division_9 <- 10
  x$marginal_occupation_division_x <- 5

  out <- build_census_2001_occupation_measures(x)
  expect_equal(out$workers_excl_cultivators_aglab_total, 100)
  expect_equal(out$manager_professional_technical_share, 0.45)
  expect_equal(out$elementary_occupation_share, 0.4)
  expect_equal(out$occupation_not_classified_share, 0.15)
})

test_that("Census 2001 worker validity requires full district coverage", {
  panel <- data.frame(
    state_code_2001 = c("09", "09"),
    district_code_2001 = c("01", "02"),
    stringsAsFactors = FALSE
  )
  variables <- c("manufacturing_share_among_main_workers", "manager_professional_technical_share")
  measures <- data.frame(
    state_code = "09", district_code = "01",
    manufacturing_share_among_main_workers = 0.2,
    manager_professional_technical_share = 0.1,
    stringsAsFactors = FALSE
  )
  expect_error(
    prepare_census_2001_balance_panel(panel, measures, variables, "Worker"),
    "do not cover the full IV panel"
  )
})
