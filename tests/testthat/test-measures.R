test_that("2007 measures compute weighted EMIE by district", {
  edu <- list(nss0708edu_block5 = data.frame(
    State = c("Bihar", "Bihar", "Bihar"),
    District = c("Patna", "Patna", "Gaya"),
    EMI = c(1, 0, 1),
    weight = c(1, 3, 2)
  ))

  out <- build_2007_measures(edu, list(), list())

  expect_equal(out$EMIE[out$district_std == "patna"], 25)
  expect_true(all(!duplicated(out$district_panel_id)))
})

test_that("named NSS blocks never masquerade as another block", {
  block5 <- data.frame(State = "Bihar", District = "Patna", EMI = 1)
  inputs <- list(nss0708edu_block5 = block5)

  expect_equal(nrow(select_input_frame(inputs, c("nss0708edu_block3", "block3"))), 0L)
  expect_identical(select_input_frame(inputs, c("nss0708edu_block5", "block5")), block5)

  unnamed <- as_input_list(block5)
  expect_identical(select_input_frame(unnamed, "not_named"), block5)
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
  expect_false("ling_degrees" %in% names(out))
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
    emi_exposure_all_children_0708 = c(10, 120),
    ling_distance_nonzero_mean = c(1, 2)
  )

  out <- validate_district_panel(panel)
  issues <- attr(out, "district_panel_validation")

  expect_true(any(issues$check == "unique_district_units"))
  expect_true(any(issues$check == "panel_variable_ranges"))
  expect_error(validate_district_panel(panel, strict = TRUE), "district_panel_id is not unique")
})


test_that("strict district-panel validation distinguishes warnings from errors", {
  panel <- data.frame(
    district_panel_id = c("a", "b"),
    emi_exposure_all_children_0708 = c(10, 20),
    ling_distance_nonzero_mean = c(1, 2),
    state_17 = c("state", "state"),
    district_17 = c("district", "district"),
    stringsAsFactors = FALSE
  )

  expect_silent(out <- validate_district_panel(panel, strict = TRUE))
  issues <- attr(out, "district_panel_validation")
  expect_true(any(issues$severity == "warning"))
  expect_equal(nrow(district_panel_blocking_issues(issues)), 0L)

  panel$district_panel_id[[2]] <- "a"
  expect_error(
    validate_district_panel(panel, strict = TRUE),
    "district_panel_id is not unique",
    fixed = TRUE
  )
})


test_that("final analysis validation does not promote district warnings to errors", {
  panel <- data.frame(
    district_panel_id = c("a", "b"),
    emi_exposure_all_children_0708 = c(20, 100),
    ling_distance_nonzero_mean = c(1, 2),
    npeople_0708 = c(20000, 25000),
    consumption_0708 = c(1000, 1100),
    gini_cons_0708 = c(0.3, 0.31),
    consumption_1718 = c(1500, 1600),
    gini_cons_1718 = c(0.32, 0.33),
    real_log_consumption_change = log(c(1500, 1600)) - log(c(1000, 1100)),
    gini_change = c(0.02, 0.02),
    state_17 = c("state", "state"),
    district_17 = c("district", "district"),
    stringsAsFactors = FALSE
  )
  for (v in census_2001_main_controls()) {
    if (!v %in% names(panel)) panel[[v]] <- 1
  }
  cfg <- list(
    mode = "final",
    strict_district_panel_validation = TRUE,
    strict_analysis_panel_validation = TRUE
  )

  expect_silent(out <- validate_analysis_district_panel(panel, cfg))
  expect_length(attr(out, "analysis_panel_validation_failures"), 0L)
  issues <- attr(out, "district_panel_validation")
  expect_true(any(issues$severity == "warning"))
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

test_that("Hindi Census-group leaves do not inherit Hindi zero distance", {
  census <- data.frame(
    state_std = rep("10", 3),
    district_std = rep("01", 3),
    mother_tongue = c("Hindi", "Bhojpuri", "Rajasthani"),
    canonical_language = rep("Hindi", 3),
    spkr_tot = c(60, 30, 10),
    mother_tongue_code = c("006118", "006045", "006242"),
    stringsAsFactors = FALSE
  )

  out <- build_linguistic_distance_iv(census)

  expect_equal(
    linguistic_distance_degrees(
      c("Hindi", "Bhojpuri", "Rajasthani"),
      rep("Hindi", 3)
    ),
    c(0, NA, 1)
  )
  expect_equal(out$hindi_share, 60)
  expect_equal(out$hindi_urdu_share, 60)
  expect_equal(out$ling_share_distance_0, 60)
  expect_equal(out$ling_share_distance_1, 10)
  expect_equal(out$ling_unmapped_speaker_share, 30)
  expect_equal(out$ling_mapped_speaker_share, 70)
  expect_equal(out$ling_distance_nonzero_mean, 1)
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
  expect_equal(out$native_english_share, 0)
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


test_that("mapped linguistic-distance shares form a genuine composition", {
  census <- data.frame(
    state_std = "01", district_std = "001",
    spkr_tot = c(40, 30, 20, 10),
    canonical_language = c("Hindi", "Punjabi", "Bengali", "Unmapped language"),
    ling_degrees = c(0, 1, 3, NA_real_),
    stringsAsFactors = FALSE
  )
  out <- build_linguistic_distance_iv(census, list())
  mapped <- unlist(out[paste0("ling_mapped_share_distance_", 0:5)], use.names = FALSE)
  all_speaker <- unlist(out[paste0("ling_share_distance_", 0:5)], use.names = FALSE)
  expect_equal(sum(mapped), 100, tolerance = 1e-8)
  expect_equal(
    sum(all_speaker) + out$ling_unmapped_speaker_share + out$native_english_share,
    100, tolerance = 1e-8
  )
})


test_that("finalize analysis panel enforces final-mode analysis validation", {
  panel <- data.frame(
    state_code_2001 = "01", district_code_2001 = "01", district_panel_id = "2001__01__01",
    emi_exposure_all_children_0708 = 20, ling_distance_nonzero_mean = 1,
    npeople_0708 = 20000, consumption_0708 = 1000, gini_cons_0708 = 0.3,
    consumption_1718 = 1500, gini_cons_1718 = NA_real_,
    real_log_consumption_change = log(1.5), gini_change = NA_real_,
    stringsAsFactors = FALSE
  )
  controls <- data.frame(state_code_2001 = "01", district_code_2001 = "01")
  for (v in census_2001_main_controls()) controls[[v]] <- 1
  cfg <- list(mode = "final", strict_district_panel_validation = FALSE, strict_analysis_panel_validation = TRUE)

  expect_silent(
    finalized <- finalize_analysis_panel(panel, controls, cfg)
  )
  expect_true(is.na(finalized$gini_cons_1718))
  expect_true(is.na(finalized$gini_change))
})

test_that("Glottolog genealogy resolves language nodes and tree distances", {
  g <- data.frame(
    id = c("rootfam", "branch_a", "hindi", "punjabi", "otherfam", "english", "dialect"),
    family_id = c("", "rootfam", "rootfam", "rootfam", "", "otherfam", "rootfam"),
    parent_id = c("", "rootfam", "branch_a", "branch_a", "", "otherfam", "punjabi"),
    name = c("Family A", "Branch A", "Hindi", "Punjabi", "Family B", "English", "Punjabi dialect"),
    bookkeeping = FALSE,
    level = c("family", "family", "language", "language", "family", "language", "dialect"),
    iso639P3code = c("", "", "hin", "pan", "", "eng", ""),
    stringsAsFactors = FALSE
  )

  expect_true(validate_glottolog_genealogy(g))
  expect_identical(glottolog_language_node("dialect", g), "punjabi")
  expect_equal(glottolog_edge_distance("hindi", "hindi", g), 0)
  expect_equal(glottolog_edge_distance("hindi", "punjabi", g), 2)
  expect_equal(glottolog_edge_distance("punjabi", "hindi", g), 2)
  expect_equal(glottolog_edge_distance("hindi", "english", g), 5)
})

test_that("Glottolog genealogy rejects unresolved parents and cycles", {
  bad_parent <- data.frame(
    id = "hindi", family_id = "family", parent_id = "missing", name = "Hindi",
    bookkeeping = FALSE, level = "language", iso639P3code = "hin", stringsAsFactors = FALSE
  )
  expect_error(validate_glottolog_genealogy(bad_parent), "unresolved parent IDs", fixed = TRUE)

  cycle <- data.frame(
    id = c("a", "b"), family_id = "fam", parent_id = c("b", "a"), name = c("A", "B"),
    bookkeeping = FALSE, level = c("language", "language"), iso639P3code = c("aaa", "bbb"),
    stringsAsFactors = FALSE
  )
  expect_error(validate_glottolog_genealogy(cycle), "parent cycle", fixed = TRUE)
})

test_that("Glottolog bookkeeping branches are not valid distance endpoints", {
  g <- data.frame(
    id = c("book", "unclassified", "hindi", "indo"),
    family_id = c("", "book", "indo", ""),
    parent_id = c("", "book", "indo", ""),
    name = c("Bookkeeping", "Unclassified", "Hindi", "Indo-European"),
    bookkeeping = c(TRUE, FALSE, FALSE, FALSE),
    level = c("family", "language", "language", "family"),
    iso639P3code = c("", "zzz", "hin", ""),
    stringsAsFactors = FALSE
  )

  expect_true(validate_glottolog_genealogy(g))
  expect_identical(glottolog_lineage("unclassified", g), character())
  expect_true(is.na(glottolog_language_node("unclassified", g)))
  expect_true(is.na(glottolog_edge_distance("unclassified", "hindi", g)))

  cldf <- list(
    languages = data.frame(
      ID = g$id, Name = g$name, Glottocode = g$id, ISO639P3code = g$iso639P3code,
      Level = g$level, Countries = c("", "IN", "IN", ""), Family_ID = g$family_id,
      Language_ID = "", stringsAsFactors = FALSE
    ),
    names = data.frame(
      ID = 1L, Language_ID = "unclassified", Name = "Test Alias", Provider = "test",
      stringsAsFactors = FALSE
    )
  )
  aliases <- glottolog_alias_index(g, cldf)
  expect_false("unclassified" %in% aliases$language_glottocode)
})

test_that("native English is explicit rather than unresolved distance mass", {
  df <- data.frame(
    state_std = "toy", district_std = "one",
    canonical_language = c("Hindi", "Gujarati", "English", "Unknown"),
    spkr_tot = c(50, 30, 10, 10),
    ling_degrees = c(0, 1, 5, NA),
    stringsAsFactors = FALSE
  )

  out <- build_linguistic_distance_iv(df)

  expect_equal(out$ling_distance_nonzero_mean, 1)
  expect_equal(out$native_english_share, 10)
  expect_equal(out$ling_mapped_speaker_share, 80)
  expect_equal(out$ling_unmapped_speaker_share, 10)
  expect_equal(out$hindi_urdu_share, 50)
  expect_true("native_english_share" %in% linguistic_distance_language_controls())
})


test_that("Census-Glottolog candidates are exact, leaf-aware, and review-only", {
  languoids <- data.frame(
    id = c("indo", "sino", "hindi", "bhili", "bilaspuri", "mising"),
    family_id = c("", "", "indo", "indo", "indo", "sino"),
    parent_id = c("", "", "indo", "indo", "indo", "sino"),
    name = c("Indo-European", "Sino-Tibetan", "Hindi", "Bhili", "Bilaspuri", "Mising"),
    bookkeeping = FALSE,
    level = c("family", "family", "language", "language", "language", "language"),
    iso639P3code = c("", "", "hin", "bhb", "kfs", "mrg"),
    stringsAsFactors = FALSE
  )
  languages <- data.frame(
    ID = languoids$id,
    Name = languoids$name,
    Glottocode = languoids$id,
    ISO639P3code = languoids$iso639P3code,
    Level = languoids$level,
    Countries = c("", "", "IN", "IN", "IN", "IN"),
    Family_ID = languoids$family_id,
    Language_ID = "",
    stringsAsFactors = FALSE
  )
  names <- data.frame(
    ID = 1:3,
    Language_ID = c("bhili", "mising", "bilaspuri"),
    Name = c("Bhilodi", "Mishing", "Bhili"),
    Provider = "test",
    stringsAsFactors = FALSE
  )
  glottolog <- list(languoids = languoids)
  cldf <- list(languages = languages, names = names)
  census <- data.frame(
    state_std = c("01", "02", "03"),
    district_std = c("01", "01", "01"),
    mother_tongue_code = c("101001", "102001", "103001"),
    mother_tongue = c("Bhilodi", "Mishing", "No Such Language"),
    canonical_language = c("Bhili/Bhilodi", "Miri/Mishing", "Unknown"),
    spkr_tot = c(100, 50, 25),
    stringsAsFactors = FALSE
  )

  out <- build_census_glottolog_match_candidates(census, glottolog, cldf)

  bhili <- out[out$mother_tongue_code == "101001", , drop = FALSE]
  expect_identical(bhili$candidate_status, "exact_unique")
  expect_identical(bhili$language_glottocode, "bhili")
  expect_identical(bhili$term_source, "mother_tongue")
  expect_true(all(bhili$review_status == "unreviewed"))

  mising <- out[out$mother_tongue_code == "102001", , drop = FALSE]
  expect_identical(mising$candidate_status, "exact_unique")
  expect_identical(mising$language_glottocode, "mising")
  expect_identical(mising$term_source, "mother_tongue")

  missing <- out[out$mother_tongue_code == "103001", , drop = FALSE]
  expect_identical(missing$candidate_status, "no_exact_match")
  expect_true(is.na(missing$language_glottocode))
})

test_that("Census-Glottolog identity aggregation preserves national speaker mass", {
  census <- data.frame(
    state_std = c("01", "01", "02"),
    district_std = c("01", "02", "01"),
    mother_tongue_code = "101001",
    mother_tongue = "Bhilodi",
    canonical_language = "Bhili/Bhilodi",
    spkr_tot = c(10, 20, 30),
    stringsAsFactors = FALSE
  )

  out <- census_language_identities(census)

  expect_equal(out$national_speakers, 60)
  expect_equal(out$n_districts, 3)
})


test_that("reviewed Census-Glottolog crosswalk rejects unknown or bookkeeping endpoints", {
  g <- data.frame(
    id = c("indo", "hindi", "valid", "book", "invalid"),
    family_id = c("", "indo", "indo", "", "book"),
    parent_id = c("", "indo", "indo", "", "book"),
    name = c("Indo-European", "Hindi", "Valid", "Bookkeeping", "Invalid"),
    bookkeeping = c(FALSE, FALSE, FALSE, TRUE, FALSE),
    level = c("family", "language", "language", "family", "language"),
    iso639P3code = c("", "hin", "val", "", "inv"),
    stringsAsFactors = FALSE
  )
  crosswalk <- data.frame(
    mother_tongue_code = c("000001", "000002"),
    mother_tongue = c("Valid", "Invalid"),
    canonical_language = c("Valid", "Invalid"),
    language_glottocode = c("valid", "invalid"),
    family_id = c("indo", "book"),
    match_basis = "manual",
    review_status = c("accepted_manual", "unresolved"),
    stringsAsFactors = FALSE
  )

  expect_true(validate_census_glottolog_crosswalk(crosswalk, g))
  crosswalk$review_status[[2]] <- "accepted_manual"
  expect_error(validate_census_glottolog_crosswalk(crosswalk, g), "invalid genealogy")
})

test_that("Glottolog district mean excludes Hindi, Urdu, and English by construction", {
  # Production distance is anchored to Glottolog's persistent Hindi ID hind1269.
  g <- data.frame(
    id = c("indo1319", "hind1269", "urdu", "other"),
    family_id = c("", "indo1319", "indo1319", "indo1319"),
    parent_id = c("", "indo1319", "indo1319", "indo1319"),
    name = c("Indo-European", "Hindi", "Urdu", "Other"),
    bookkeeping = FALSE,
    level = c("family", "language", "language", "language"),
    iso639P3code = c("", "hin", "urd", "oth"),
    stringsAsFactors = FALSE
  )
  crosswalk <- data.frame(
    mother_tongue_code = sprintf("%06d", 1:4),
    mother_tongue = c("Hindi", "Urdu", "English", "Other"),
    canonical_language = c("Hindi", "Urdu", "English", "Other"),
    language_glottocode = c("hind1269", "urdu", "", "other"),
    family_id = c("indo1319", "indo1319", "", "indo1319"),
    match_basis = c("manual", "manual", "", "manual"),
    review_status = c("accepted_manual", "accepted_manual", "unresolved", "accepted_manual"),
    stringsAsFactors = FALSE
  )
  census <- data.frame(
    state_std = "01", district_std = "001",
    mother_tongue_code = sprintf("%06d", 1:4),
    mother_tongue = c("Hindi", "Urdu", "English", "Other"),
    canonical_language = c("Hindi", "Urdu", "English", "Other"),
    spkr_tot = c(50, 20, 10, 20),
    ling_degrees = c(0, 0, NA, 3),
    stringsAsFactors = FALSE
  )
  out <- build_linguistic_distance_iv(
    census,
    glottolog = list(languoids = g),
    glottolog_crosswalk = crosswalk
  )

  expect_equal(out$ling_glottolog_mapped_speaker_share, 100)
  expect_equal(out$ling_glottolog_unmapped_speaker_share, 0)
  expect_equal(
    out$ling_distance_glottolog_nonhindi_mean,
    glottolog_edge_distance("other", "hind1269", g)
  )
})


test_that("accepted non-Indo-European mappings apply Shastry degree five only", {
  g <- data.frame(
    id = c("indo", "otherfam", "hindi", "other"),
    family_id = c("", "", "indo", "otherfam"),
    parent_id = c("", "", "indo", "otherfam"),
    name = c("Indo-European", "Other family", "Hindi", "Other"),
    bookkeeping = FALSE,
    level = c("family", "family", "language", "language"),
    iso639P3code = c("", "", "hin", "oth"),
    stringsAsFactors = FALSE
  )
  crosswalk <- data.frame(
    mother_tongue_code = "000001",
    mother_tongue = "Other",
    canonical_language = "Other",
    language_glottocode = "other",
    family_id = "otherfam",
    match_basis = "manual",
    review_status = "accepted_manual",
    stringsAsFactors = FALSE
  )
  census <- data.frame(
    state_std = "01", district_std = "001",
    mother_tongue_code = "000001",
    mother_tongue = "Other", canonical_language = "Other",
    spkr_tot = 100, stringsAsFactors = FALSE
  )

  out <- build_linguistic_distance_iv(
    census,
    glottolog = list(languoids = g),
    glottolog_crosswalk = crosswalk
  )

  expect_equal(out$ling_share_distance_5, 100)
  expect_equal(out$ling_mapped_speaker_share, 100)
})

test_that("Indo-European extensions require historical review rather than Glottolog degree inference", {
  g <- data.frame(
    id = c("indo1319", "hind1269", "nepal"),
    family_id = c("", "indo1319", "indo1319"),
    parent_id = c("", "indo1319", "indo1319"),
    name = c("Indo-European", "Hindi", "Nepali"),
    bookkeeping = FALSE,
    level = c("family", "language", "language"),
    iso639P3code = c("", "hin", "nep"),
    stringsAsFactors = FALSE
  )
  crosswalk <- data.frame(
    mother_tongue_code = c("000001", "000002"),
    mother_tongue = c("Hindi", "Nepali"),
    canonical_language = c("Hindi", "Nepali"),
    language_glottocode = c("hind1269", "nepal"),
    family_id = "indo1319",
    iso639P3code = c("hin", "nep"),
    match_basis = "manual",
    review_status = "accepted_manual",
    stringsAsFactors = FALSE
  )
  concordance <- data.frame(
    canonical_language = "Hindi",
    distance_from_hindi = 0,
    stringsAsFactors = FALSE
  )
  historical <- list(
    ethnologue_proxy = list(
      indo_european_tree = "('Hindi [i-hin]':1,'Nepali [i-nep]':1)'Indo-Aryan [i-]':1;"
    ),
    dyen_hindi = data.frame(
      list_name = "Nepali List",
      percent_cognates_with_hindi = 64.2,
      stringsAsFactors = FALSE
    )
  )
  lexical_index <- data.frame(
    language = "Nepali", dyen_list_name = "Nepali List", kogan_code = "NEP",
    match_basis = "direct", source_note = "test", stringsAsFactors = FALSE
  )

  out <- build_shastry_extension_candidates(
    crosswalk, list(languoids = g), historical, concordance, lexical_index
  )

  expect_true(is.na(out$candidate_degree))
  expect_identical(out$candidate_basis, "figure6_lsi_lexical_review")
  expect_identical(out$ethnologue_proxy_status, "exact_mother_tongue")
  expect_equal(out$dyen_cognate_pct_hindi, 64.2)
  expect_identical(out$kogan_code, "NEP")
  expect_identical(out$review_status, "review_required")
})


test_that("Dyen parser ignores documentation examples and uses the data section", {
  lines <- c(
    "COMPARATIVE INDOEUROPEAN DATABASE COLLECTED BY ISIDORE DYEN",
    "5. THE DATA",
    "2. HISTORY OF THE DATA IN THIS FILE",
    "a 003 ANIMAL",
    "b                      207",
    "  003 01 Example         FORM",
    "both of the varieties have an unbroken history",
    "classification (in Appendices 1 and 5 and Figure 1)",
    "5. THE DATA",
    "-----------",
    "a 001 ALL",
    "b                      002",
    "  001 01 Hindi           H",
    "  001 02 Target          T",
    "a 002 ASHES",
    "b                      100",
    "  002 01 Hindi           H",
    "  002 02 Target          T",
    "a 003 BARK",
    "b                      002",
    "  003 01 Hindi           H",
    "b                      003",
    "  003 02 Target          T"
  )
  parsed <- parse_dyen_1997_lines(lines)
  parsed$lists <- unique(parsed$forms[c("list_number", "list_name")])

  expect_false(any(parsed$forms$list_name == "Example"))
  judgments <- dyen_pairwise_cognacy(parsed, "Hindi", "Target")
  expect_setequal(judgments$status, c("cognate", "doubtful", "not_cognate"))
  expect_equal(dyen_pairwise_cognate_percent(parsed, "Hindi", "Target"), 50)
})

test_that("Dyen data section uses the final exact marker after the table of contents", {
  lines <- c(
    "5. THE DATA",
    "table-of-contents material",
    "a 099 WRONG",
    "b                      002",
    "  099 01 Wrong           X",
    "5. THE DATA",
    "-----------",
    "a 001 ALL",
    "b                      002",
    "  001 01 Hindi           H"
  )

  data <- dyen_data_lines(lines)

  expect_identical(data[[1]], "a 001 ALL")
  expect_false(any(grepl("WRONG", data, fixed = TRUE)))
})

test_that("Dyen record grammar does not mistake prose for data records", {
  expect_identical(dyen_record_type("a 001 ALL"), "header")
  expect_identical(dyen_record_type("b                      207"), "subheader")
  expect_identical(dyen_record_type("c                         207  3  209"), "relationship")
  expect_identical(dyen_record_type("  003 01 Irish A         FORM"), "form")
  expect_identical(dyen_record_type("both of the varieties have an unbroken history"), "other")
  expect_identical(dyen_record_type("classification (in Appendices 1 and 5 and Figure 1)"), "other")
})

test_that("Dyen Shastry benchmarks are an explicit methodological invariant", {
  benchmark <- dyen_shastry_benchmarks()
  x <- data.frame(
    list_name = benchmark$list_name,
    percent_cognates_with_hindi = benchmark$expected_percent,
    stringsAsFactors = FALSE
  )
  expect_true(validate_dyen_shastry_benchmarks(x))
  x$percent_cognates_with_hindi[x$list_name == "Panjabi ST"] <- 70
  expect_error(validate_dyen_shastry_benchmarks(x), "do not reproduce Shastry")
})

test_that("Ethnologue proxy reader treats the downloaded CSV as tab-separated", {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    "Family\tSuccess\tComments\tTree",
    "Indo-European\tSUCCESS\tPath length proportional to the number of splits with atomic branch length = 1\t('Hindi [i-hin]':1);"
  ), path)
  out <- read_ethnologue_newick_proxy(path)
  expect_identical(names(out$table), c("Family", "Success", "Comments", "Tree"))
  expect_match(out$indo_european_tree, "Hindi", fixed = TRUE)
})


test_that("Dyen Hindi cognates have an independent diagnostic writer", {
  x <- data.frame(
    list_number = 1:2,
    list_name = c("Hindi", "Panjabi ST"),
    percent_cognates_with_hindi = c(100, 74.5),
    stringsAsFactors = FALSE
  )
  dir <- tempfile("dyen-diagnostic-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
  path <- file.path(dir, "dyen_hindi_cognates.csv")

  written <- save_dyen_hindi_cognates(x, path)

  expect_identical(normalizePath(written, mustWork = TRUE), normalizePath(path, mustWork = TRUE))
  round_trip <- utils::read.csv(written, stringsAsFactors = FALSE)
  expect_equal(round_trip$percent_cognates_with_hindi, c(100, 74.5))
})
