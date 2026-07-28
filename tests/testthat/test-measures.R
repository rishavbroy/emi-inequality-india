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
  expect_equal(out$consumption_1718_household_weighted, 175)
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
    state_std = c("bihar", "bihar"), district_std = c("patna", "patna"),
    canonical_language = c("Hindi", "Tamil"),
    ling_degrees = c(0, 5), spkr_tot = c(3, 1),
    mother_tongue_code = c("006001", "020001")
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

test_that("Census 2001 cleaner parses mutually exclusive district language rows", {
  raw <- data.frame(
    `C-16 POPULATION BY MOTHER TONGUE` = rep("C0116", 6),
    ...2 = rep("01", 6), ...3 = rep("02", 6), ...4 = rep("0000", 6),
    ...5 = rep("District - Baramula  02", 6),
    ...6 = c("001000", "001002", "006000", "006001", "016000", "016001"),
    ...7 = c("1 ASSAMESE", "1 ASSAMESE", "6 HINDI", "1 HINDI", "16 PUNJABI", "1 PUNJABI"),
    ...8 = c(100, 100, 200, 200, 50, 50), check.names = FALSE
  )

  out <- clean_census_2001_languages(list(raw))

  expect_equal(nrow(out), 3L)
  expect_setequal(out$canonical_language, c("Assamese", "Hindi", "Punjabi"))
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
  expect_true(sf::st_crs(out) == sf::st_crs(boundaries))
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

test_that("2007 district identifiers accept haven labelled vectors", {
  skip_if_not_installed("haven")
  labelled_codes <- haven::labelled(c(101, 102), labels = c(first = 101, second = 102))
  input <- data.frame(value = c(1, 2))
  input$district_code_0708 <- labelled_codes

  standardized <- standardize_nss_2007_district_code(input)

  expect_identical(standardized$district_code_0708, c("101", "102"))
  expect_identical(district_group_vars_2007(standardized), "district_code_0708")
})

test_that("person-weighted consumption differs from the mean household MPCE", {
  total <- c(2000, 5000)
  size <- c(1, 5)
  weight <- c(1, 1)
  expect_equal(mean_household_mpce(total, size, weight), 1500)
  expect_equal(mean_expenditure_per_person(total, size, weight), 7000 / 6)
})

test_that("household deflation requires positive price relatives", {
  expect_equal(deflate_household_expenditure(c(100, 200), c(2, 4)), c(50, 50))
  expect_true(is.na(deflate_household_expenditure(100, 0)))
})

test_that("district panel constructs real log change from deflated district means", {
  panel <- data.frame(
    state_std = "bihar", district_std = "patna", district_panel_id = "id1",
    EMIE = 10, consumption_0708 = 100, consumption_1718 = 200,
    real_consumption_0708 = 80, real_consumption_1718 = 120,
    npeople_0708 = 1, gini_cons_0708 = 0.1, gini_cons_1718 = 0.2,
    wavg_ling_degrees = 1
  )
  out <- build_district_panel(data.frame(), panel, data.frame(), data.frame(), data.frame(), list())
  expect_equal(out$real_log_consumption_change, log(120) - log(80))
  expect_equal(out$real_consumption_level_change, 40)
})

test_that("lineage panels construct real outcomes and ANCOVA levels", {
  panel <- data.frame(real_consumption_0708 = 80, real_consumption_1718 = 120)
  out <- compute_real_consumption_outcomes(panel)
  expect_equal(out$log_real_consumption_0708, log(80))
  expect_equal(out$log_real_consumption_1718, log(120))
  expect_equal(out$real_log_consumption_change, log(120) - log(80))
})

test_that("C-16 cleaner removes group subtotals and carries parent language to leaves", {
  raw <- data.frame(
    `C-16 POPULATION BY MOTHER TONGUE` = rep("C0116", 6),
    ...2 = rep("10", 6),
    ...3 = rep("01", 6),
    ...4 = rep("0000", 6),
    ...5 = rep("District - Patna  01", 6),
    ...6 = c("002000", "002004", "002007", "002999", "006000", "006001"),
    ...7 = c("2 BENGALI", "1 BENGALI", "2 CHAKMA", "2 Others", "6 HINDI", "1 HINDI"),
    ...8 = c(100, 70, 20, 10, 200, 200),
    check.names = FALSE
  )

  out <- clean_census_2001_languages(list(raw))

  expect_equal(nrow(out), 4L)
  expect_false(any(grepl("000$", out$mother_tongue_code)))
  expect_equal(unique(out$canonical_language[out$language_group_code == "002"]), "Bengali")
  expect_true(all(out$ling_degrees[out$language_group_code == "002"] == 3))
  expect_equal(sum(out$spkr_tot[out$language_group_code == "002"]), 100)
})

test_that("linguistic constructions use the full distribution and expose mapping coverage", {
  census <- data.frame(
    state_std = rep("10", 5), district_std = rep("01", 5),
    canonical_language = c("Hindi", "Urdu", "Bengali", "Tamil", "Dogri"),
    ling_degrees = c(0, 0, 3, 5, NA),
    spkr_tot = c(40, 10, 20, 20, 10),
    mother_tongue_code = sprintf("%06d", 1:5),
    stringsAsFactors = FALSE
  )

  out <- build_linguistic_distance_iv(census)

  expect_equal(out$ling_distance_nonzero_mean, 4)
  expect_equal(out$ling_share_distance_ge3, 40)
  expect_equal(out$ling_share_distance_0, 50)
  expect_equal(out$hindi_share, 40)
  expect_equal(out$urdu_share, 10)
  expect_equal(out$hindi_urdu_share, 50)
  expect_equal(out$ling_mapped_speaker_share, 90)
  expect_equal(out$ling_unmapped_speaker_share, 10)
  expect_equal(out$wavg_ling_degrees, out$ling_distance_top3_legacy)
  expect_equal(linguistic_distance_excluded_instruments(), paste0("ling_share_distance_", 1:5))
})

test_that("education exposure margins share one weighted child universe", {
  children <- data.frame(
    district_code_0708 = rep("01001", 4),
    AGE = c(5, 6, 14, 18),
    enrolled = factor(c("Yes", "Yes", "No", "Yes"), levels = c("No", "Yes")),
    MEDIUM_INSTRUCTION = c("02", "01", NA, "02"),
    weight = c(1, 1, 2, 2),
    stringsAsFactors = FALSE
  )

  out <- build_education_exposure_2007(children)

  expect_equal(out$enrollment_rate_0708, 100 * 4 / 6)
  expect_equal(out$emi_share_enrolled_0708, 75)
  expect_equal(out$emi_exposure_all_children_0708, 50)
  expect_equal(out$emi_exposure_all_children_0708 / 100,
               out$enrollment_rate_0708 / 100 * out$emi_share_enrolled_0708 / 100)
  expect_equal(out$eligible_child_weight_0708_age6_17, 3)
  expect_equal(out$eligible_child_weight_0708_age6_14, 3)
})

test_that("unknown medium is reported rather than classified as non-EMI", {
  children <- data.frame(
    district_code_0708 = rep("01001", 3), AGE = c(10, 11, 12),
    enrolled = factor(rep("Yes", 3), levels = c("No", "Yes")),
    MEDIUM_INSTRUCTION = c("02", "01", NA), weight = rep(1, 3)
  )

  out <- build_education_exposure_2007(children)

  expect_equal(out$unknown_medium_share_enrolled_0708, 100 / 3)
  expect_equal(out$emi_share_enrolled_0708, 50)
  expect_equal(out$emi_exposure_all_children_0708, 100 / 3)
})
