test_that("2007 measures compute weighted EMIE by district", {
  edu <- list(block = data.frame(
    State = c("Bihar", "Bihar", "Bihar"),
    District = c("Patna", "Patna", "Gaya"),
    EMI = c(1, 0, 1),
    weight = c(1, 3, 2)
  ))

  out <- build_2007_measures(edu, list(), list())

  expect_equal(out$EMIE[out$district_std == "patna"], 25)
  expect_true(all(!duplicated(out$district_panel_id)))
})

test_that("2017 measures compute weighted consumption by district", {
  edu <- list(block = data.frame(
    State = c("Bihar", "Bihar"),
    District = c("Patna", "Patna"),
    MPCE = c(100, 200),
    weight = c(1, 3)
  ))

  out <- build_2017_measures(edu, list())

  expect_equal(out$consumption_1718, 175)
})

test_that("2017 district lookup recovers headerless Tabula CSV rows", {
  districts <- data.frame(
    check.names = FALSE,
    `1.` = c("", "", "", "", "", "2."),
    `Andaman &` = c("Nicobar Islands", "(35)", "", "", "", "Andhra Pradesh"),
    `351` = c("", "", "", "", "", "281"),
    `Andaman &.1` = c("Nicobar", "Islands", "", "", "", "Coastal"),
    `1..1` = c("2.", "3.", "", "", "", "4."),
    `Nicobars` = c("North & Middle Andaman", "South Andaman", "", "", "", "Srikakulam"),
    `(01)` = c("(02)", "(03)", "", "", "", "(01)")
  )
  states <- data.frame(
    `State/UT name` = c("A & N Islands", "Andhra Pradesh"),
    code = c("35", "28"),
    check.names = FALSE
  )

  out <- parse_2017_district_lookup(list(nss1718_districts = districts, nss1718_state_codes = states))

  expect_equal(nrow(out), 4L)
  expect_equal(out$district_code_1718[1], "35101")
  expect_equal(out$state_1718[1], "Andaman & Nicobar Islands")
  expect_equal(out$district_1718[out$district_code_1718 == "28101"], "Srikakulam")
})

test_that("2007 household aggregation computes a weighted Gini through the canonical path", {
  df <- data.frame(
    district_code = c("01001", "01001"),
    HHID = c("h1", "h2"),
    MPCE = c(100, 200),
    HH_SIZE = c(2, 2),
    weight = c(1, 1)
  )

  out <- compute_education_household_measures_2007(df)

  expect_gt(out$gini_cons_0708, 0)
  expect_lt(out$gini_cons_0708, 1)
})

test_that("linguistic distance IV uses real columns when present", {
  census <- data.frame(
    State = c("Bihar", "Bihar"),
    District = c("Patna", "Patna"),
    ling_degrees = c(0, 5),
    spkr_tot = c(3, 1)
  )

  out <- build_linguistic_distance_iv(census, list())

  expect_equal(out$wavg_ling_degrees, 1.25)
  expect_equal(out$district_panel_id, "2001__bihar__patna")
})

test_that("Census 2001 state codes map to tracker state names", {
  expect_equal(census_2001_state_name(c("01", "05", "21", "35")), c(
    "Jammu & Kashmir",
    "Uttaranchal",
    "Orissa",
    "Andaman & Nicobar Islands"
  ))
})

test_that("Census 2001 cleaner parses district language rows", {
  raw <- data.frame(
    `C-16 POPULATION BY MOTHER TONGUE` = rep("C0116", 5),
    ...2 = rep("01", 5),
    ...3 = rep("02", 5),
    ...4 = rep("0000", 5),
    ...5 = rep("District - Baramula  02", 5),
    ...6 = c("001000", "006000", "016000", "004000", "001999"),
    ...7 = c("1 ASSAMESE", "2 HINDI", "3 PUNJABI", "4 DOGRI", "1 Others"),
    ...8 = c(100, 200, 50, 10, 500),
    check.names = FALSE
  )

  out <- clean_census_2001_languages(list(raw))

  expect_equal(nrow(out), 3)
  expect_setequal(out$mother_tongue, c("Assamese", "Hindi", "Punjabi"))
  expect_equal(unique(out$state_std), "01")
  expect_equal(unique(out$district_std), "02")
  expect_setequal(out$ling_degrees, c(0, 1, 3))
})

test_that("linguistic distance IV does not invent placeholder values", {
  census <- data.frame(State = "Bihar", District = "Patna", spkr_tot = 10)

  out <- build_linguistic_distance_iv(census, list())

  expect_equal(out$status, "out_of_active_pipeline")
  expect_match(out$reason, "No real linguistic-distance column")
})

test_that("district panel preserves IDs and avoids duplicate generated units", {
  measures_2007 <- data.frame(
    state_std = c("bihar", "bihar"),
    district_std = c("patna", "gaya"),
    district_panel_id = c("id1", "id2"),
    EMIE = c(0.2, 0.4)
  )
  measures_2017 <- data.frame(
    state_std = "bihar",
    district_std = "patna",
    consumption_1718 = 100
  )

  out <- build_district_panel(data.frame(), measures_2007, measures_2017, data.frame(), data.frame(), list())

  expect_setequal(out$district_panel_id, c("id1", "id2"))
  expect_false(anyDuplicated(out$district_panel_id) > 0L)
})

test_that("district panel preserves sf geometry when boundary keys match", {
  skip_if_not_installed("sf")
  poly <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
  ))), crs = 4326)
  boundaries <- sf::st_sf(
    state_std = "bihar",
    district_std = "patna",
    geometry = poly
  )
  measures_2007 <- data.frame(
    state_std = "bihar",
    district_std = "patna",
    district_panel_id = "id1",
    EMIE = 0.2
  )

  out <- build_district_panel(data.frame(), measures_2007, data.frame(), data.frame(), boundaries, list())

  expect_s3_class(out, "sf")
  expect_true("geometry" %in% names(out))
})

test_that("district panel validation records duplicate and range issues", {
  panel <- data.frame(
    district_panel_id = c("a", "a"),
    EMIE = c(10, 120),
    wavg_ling_degrees = c(1, 2)
  )

  out <- validate_district_panel(panel)
  issues <- attr(out, "district_panel_validation")

  expect_true(any(issues$check == "unique_district_units"))
  expect_true(any(issues$check == "panel_variable_ranges"))
  expect_error(validate_district_panel(panel, strict = TRUE), "district_panel_id is not unique")
})


test_that("analysis district-panel validation inspects join-map many-to-many flags", {
  panel <- data.frame(
    district_panel_id = "a",
    EMIE = 10,
    wavg_ling_degrees = 1
  )
  join_map <- data.frame(
    many_to_many = TRUE,
    many_to_many_allowed = FALSE
  )

  out <- validate_analysis_district_panel(panel, cfg = list(strict_district_panel_validation = FALSE), join_map = join_map)
  issues <- attr(out, "district_panel_validation")

  expect_true(any(issues$message == "join_map contains unintended many-to-many matches."))
  expect_error(
    validate_analysis_district_panel(panel, cfg = list(strict_district_panel_validation = TRUE), join_map = join_map),
    "join_map contains unintended many-to-many matches"
  )
})

test_that("linguistic-distance range validation is part of the active builder", {
  census <- data.frame(
    State = "Bihar",
    District = "Patna",
    ling_degrees = 6,
    spkr_tot = 1
  )

  expect_error(
    build_linguistic_distance_iv(census, list()),
    "0-5 range",
    fixed = TRUE
  )
})

test_that("district panel attaches 2020 geometry without merge coercion", {
  skip_if_not_installed("sf")
  polygons <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    sf::st_polygon(list(rbind(c(2, 0), c(3, 0), c(3, 1), c(2, 1), c(2, 0)))),
    crs = 4326
  )
  boundaries <- sf::st_sf(
    state_20 = c("Bihar", "Bihar"),
    district_20 = c("Patna", "Gaya"),
    geometry = polygons
  )
  panel <- data.frame(
    district_panel_id = c("gaya", "patna", "missing"),
    state_20 = c("Bihar", "Bihar", "Bihar"),
    district_20 = c("Gaya", "Patna", "Nalanda"),
    stringsAsFactors = FALSE
  )

  out <- attach_panel_geometry(panel, boundaries)

  expect_s3_class(out, "sf")
  expect_identical(out$district_panel_id, panel$district_panel_id)
  expect_equal(sf::st_coordinates(sf::st_geometry(out)[1])[, 1], c(2, 3, 3, 2, 2))
  expect_identical(sf::st_is_empty(sf::st_geometry(out)), c(FALSE, FALSE, TRUE))
})

test_that("district geometry attachment keeps an sf contract when no keys match", {
  skip_if_not_installed("sf")
  boundaries <- sf::st_sf(
    state_20 = "Bihar",
    district_20 = "Patna",
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  )
  panel <- data.frame(
    district_panel_id = c("gaya", "nalanda"),
    state_20 = c("Bihar", "Bihar"),
    district_20 = c("Gaya", "Nalanda"),
    stringsAsFactors = FALSE
  )

  out <- attach_panel_geometry(panel, boundaries)

  expect_s3_class(out, "sf")
  expect_identical(out$district_panel_id, panel$district_panel_id)
  expect_true(all(sf::st_is_empty(sf::st_geometry(out))))
  expect_identical(sf::st_crs(out), sf::st_crs(boundaries))
})

test_that("district geometry matching uses the first unique boundary key", {
  skip_if_not_installed("sf")
  polygons <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(9, 9)),
    crs = 4326
  )
  boundaries <- sf::st_sf(
    state_20 = c("Bihar", "Bihar"),
    district_20 = c("Patna", "Patna"),
    geometry = polygons
  )
  panel <- data.frame(state_20 = "Bihar", district_20 = "Patna")

  out <- attach_panel_geometry(panel, boundaries)

  expect_equal(as.numeric(sf::st_coordinates(out)[1, ]), c(0, 0))
})
