test_that("2007 measures compute weighted EMIE by district", {
  edu <- list(block = data.frame(
    State = c("Bihar", "Bihar", "Bihar"),
    District = c("Patna", "Patna", "Gaya"),
    EMI = c(1, 0, 1),
    weight = c(1, 3, 2)
  ))

  out <- build_2007_measures(edu, list(), data.frame(), data.frame(), list())

  expect_equal(out$emie_2007[out$district_std == "patna"], 25)
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

  expect_equal(out$consumption_2017, 175)
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

test_that("weighted Gini helper is available for measure construction", {
  df <- data.frame(
    State = c("Bihar", "Bihar"),
    District = c("Patna", "Patna"),
    MPCE = c(100, 200),
    weight = c(1, 1)
  )

  out <- compute_gini_consumption_2007(df)

  expect_gt(out$gini_consumption_2007, 0)
  expect_lt(out$gini_consumption_2007, 1)
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
    emie_2007 = c(0.2, 0.4)
  )
  measures_2017 <- data.frame(
    state_std = "bihar",
    district_std = "patna",
    consumption_2017 = 100
  )

  out <- build_district_panel(data.frame(), data.frame(), measures_2007, measures_2017, data.frame(), data.frame(), list())

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
    emie_2007 = 0.2
  )

  out <- build_district_panel(data.frame(), data.frame(), measures_2007, data.frame(), data.frame(), boundaries, list())

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


test_that("legacy district-panel validation inspects join-map many-to-many flags", {
  panel <- data.frame(
    district_panel_id = "a",
    EMIE = 10,
    wavg_ling_degrees = 1
  )
  join_map <- data.frame(
    many_to_many = TRUE,
    many_to_many_allowed = FALSE
  )

  out <- validate_legacy_district_panel(panel, cfg = list(strict_district_panel_validation = FALSE), join_map = join_map)
  issues <- attr(out, "district_panel_validation")

  expect_true(any(issues$message == "join_map contains unintended many-to-many matches."))
  expect_error(
    validate_legacy_district_panel(panel, cfg = list(strict_district_panel_validation = TRUE), join_map = join_map),
    "join_map contains unintended many-to-many matches"
  )
})
