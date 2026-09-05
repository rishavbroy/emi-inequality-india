test_that("ST17 parser collapses repeated published language rows without double counting", {
  raw <- as.data.frame(matrix(NA_character_, nrow = 36, ncol = 10), stringsAsFactors = FALSE)
  raw[8, 1] <- "TEST DISTRICT"
  raw[9, 1] <- "All Scheduled Tribes"
  raw[10, 1] <- "Rural"
  raw[13, 2] <- "Total No. of GONDI speakers"; raw[13, 6] <- "100"
  raw[14, 2] <- "Monolinguals"; raw[14, 6] <- "60"
  raw[15, 2] <- "Monolinguals"; raw[15, 6] <- "60"
  raw[16, 2] <- "Bilinguals (including trilinguals)"; raw[16, 6] <- "40"
  raw[17, 2] <- "Bilinguals (including trilinguals)"; raw[17, 6] <- "40"
  raw[19, 1] <- "1. ENGLISH"; raw[19, 2] <- "20"
  raw[20, 1] <- "2. ENGLISH"; raw[20, 2] <- "20"; raw[20, 6] <- "Hindi"; raw[20, 8] <- "5"
  raw[21, 6] <- "Hindi"; raw[21, 8] <- "5"
  raw[22, 6] <- "Not speaking a third language"
  raw[23, 1] <- "3. HINDI"; raw[23, 2] <- "10"; raw[23, 6] <- "English"; raw[23, 8] <- "2"
  raw[24, 6] <- "Total"; raw[24, 8] <- "Females"
  raw[25, 1] <- "Urban"
  raw[28, 2] <- "Total No. of GONDI speakers"; raw[28, 6] <- "50"
  raw[29, 2] <- "Monolinguals"; raw[29, 6] <- "35"
  raw[30, 2] <- "Bilinguals (including trilinguals)"; raw[30, 6] <- "15"
  raw[32, 1] <- "1. ENGLISH"; raw[32, 2] <- "10"
  raw[34, 1] <- "Gond"

  parsed <- parse_census_1991_st17_sheet(raw, "13", "01")
  expect_equal(nrow(parsed), 2L)
  rural <- parsed[parsed$residence == "Rural", , drop = FALSE]
  expect_equal(rural$mother_tongue_speakers, 100)
  expect_equal(rural$bilingual_speakers, 40)
  expect_equal(rural$english_second_language, 20)
  expect_equal(rural$english_third_language, 2)
  expect_equal(rural$hindi_second_language, 10)
  expect_equal(rural$hindi_third_language, 5)

  total <- aggregate_census_1991_st17_residence(parsed)
  expect_equal(total$mother_tongue_speakers, 150)
  expect_equal(total$english_acquisition_speakers, 32)
  expect_equal(total$hindi_acquisition_speakers, 15)
  expect_equal(total$bilingual_share, 55 / 150)
})

test_that("ST17 parser enforces published monolingual and bilingual accounting", {
  raw <- as.data.frame(matrix(NA_character_, nrow = 20, ncol = 10), stringsAsFactors = FALSE)
  raw[8, 1] <- "TEST DISTRICT"
  raw[9, 1] <- "All Scheduled Tribes"
  raw[10, 1] <- "Rural"
  raw[13, 2] <- "Total No. of GONDI speakers"; raw[13, 6] <- "100"
  raw[14, 2] <- "Monolinguals"; raw[14, 6] <- "70"
  raw[15, 2] <- "Bilinguals (including trilinguals)"; raw[15, 6] <- "40"
  expect_error(
    parse_census_1991_st17_sheet(raw, "13", "01"),
    "must exhaust mother-tongue speakers"
  )
})

test_that("ST16 validation gates mismatched and unavailable districts", {
  st17 <- data.frame(
    state_code_1991 = c("13", "13", "04"),
    district_code_1991 = c("01", "01", "01"),
    district_name = c("A", "A", "B"),
    mother_tongue = c("Gondi", "Hindi", "Assamese"),
    mother_tongue_speakers = c(100, 20, 50),
    monolingual_speakers = c(80, 10, 40),
    bilingual_speakers = c(20, 10, 10),
    english_second_language = c(5, 1, 1), english_third_language = 0,
    hindi_second_language = c(10, 0, 2), hindi_third_language = 0,
    english_acquisition_speakers = c(5, 1, 1),
    hindi_acquisition_speakers = c(10, 0, 2),
    bilingual_share = c(.2, .5, .2),
    english_acquisition_share = c(.05, .05, .02),
    hindi_acquisition_share = c(.1, 0, .04),
    english_minus_hindi_acquisition_share = c(-.05, .05, -.02),
    stringsAsFactors = FALSE
  )
  st16 <- data.frame(
    state_code_1991 = c("13", "13"), district_code_1991 = c("01", "01"),
    district_name = c("A", "A"), mother_tongue = c("Gondi", "Hindi"),
    mother_tongue_speakers_st16 = c(100, 20), stringsAsFactors = FALSE
  )
  validation <- build_census_1991_st16_validation(st17, st16)
  expect_identical(
    validation$validation_status[validation$state_code_1991 == "13"], "exact"
  )
  expect_identical(
    validation$validation_status[validation$state_code_1991 == "04"], "st16_unavailable"
  )

  st16$mother_tongue_speakers_st16[st16$mother_tongue == "Gondi"] <- 99
  validation <- build_census_1991_st16_validation(st17, st16)
  expect_identical(
    validation$validation_status[validation$state_code_1991 == "13"],
    "speaker_counts_mismatch"
  )
})

test_that("ST language panel admits only exact ST16 validation with frozen distance mapping", {
  st17 <- data.frame(
    state_code_1991 = c("13", "13", "04"), district_code_1991 = c("01", "01", "01"),
    district_name = c("A", "A", "B"), mother_tongue = c("Gondi", "Hindi", "Assamese"),
    mother_tongue_speakers = c(100, 20, 50), monolingual_speakers = c(80, 10, 40),
    bilingual_speakers = c(20, 10, 10), english_second_language = c(5, 1, 1),
    english_third_language = 0, hindi_second_language = c(10, 0, 2), hindi_third_language = 0,
    english_acquisition_speakers = c(5, 1, 1), hindi_acquisition_speakers = c(10, 0, 2),
    bilingual_share = c(.2, .5, .2), english_acquisition_share = c(.05, .05, .02),
    hindi_acquisition_share = c(.1, 0, .04),
    english_minus_hindi_acquisition_share = c(-.05, .05, -.02), stringsAsFactors = FALSE
  )
  st16 <- data.frame(
    state_code_1991 = c("13", "13", "04"), district_code_1991 = c("01", "01", "01"),
    district_name = c("A", "A", "B"), mother_tongue = c("Gondi", "Hindi", "Assamese"),
    mother_tongue_speakers_st16 = c(100, 20, 49), stringsAsFactors = FALSE
  )
  mapping <- data.frame(label = c("Gondi", "Hindi", "Assamese"), shastry_distance = c(5, 0, 3))
  panel <- build_census_1991_st_language_panel(st17, st16, mapping)
  expect_true(all(panel$preferred_st_language_sample[panel$state_code_1991 == "13"]))
  expect_false(any(panel$preferred_st_language_sample[panel$state_code_1991 == "04"]))
  expect_true(all(panel$hindi_belt_1991[panel$state_code_1991 == "13"]))
  expect_equal(panel$shastry_distance_1991, c(5, 0, 3))
})

test_that("ST16 parser carries hierarchical workbook keys without carrying measures", {
  raw <- as.data.frame(matrix(NA_character_, nrow = 5, ncol = 12), stringsAsFactors = FALSE)
  raw[1, c(1, 3, 4, 5, 6, 7, 10)] <- c(
    "13", "01", "DISTRICT A", "All Scheduled Tribes", "GONDI", "80", "20"
  )
  raw[2, c(6, 7, 10)] <- c("HINDI", "10", "5")
  raw[3, c(3, 4, 5, 6, 7, 10)] <- c(
    "02", "DISTRICT B", "All Scheduled Tribes", "GONDI", "40", "10"
  )
  raw[4, c(5, 6, 7, 10)] <- c("Gond", "GONDI", "30", "5")

  parsed <- parse_census_1991_st16_sheet(raw)

  expect_equal(nrow(parsed), 3L)
  expect_identical(parsed$state_code_1991, c("13", "13", "13"))
  expect_identical(parsed$district_code_1991, c("01", "01", "02"))
  expect_identical(parsed$mother_tongue, c("Gondi", "Hindi", "Gondi"))
  expect_equal(parsed$mother_tongue_speakers_st16, c(100, 15, 50))
})

test_that("ST17 parser ignores the generic printed mother-tongue total row", {
  raw <- as.data.frame(matrix(NA_character_, nrow = 24, ncol = 10), stringsAsFactors = FALSE)
  raw[8, 1] <- "TEST DISTRICT"
  raw[9, 1] <- "All Scheduled Tribes"
  raw[10, 1] <- "Rural"
  raw[12, 2] <- "Total No. of  speakers"; raw[12, 6] <- "999"
  raw[13, 2] <- "Total No. of GONDI speakers"; raw[13, 6] <- "100"
  raw[14, 2] <- "Monolinguals"; raw[14, 6] <- "70"
  raw[15, 2] <- "Bilinguals (including trilinguals)"; raw[15, 6] <- "30"
  raw[17, 1] <- "1. ENGLISH"; raw[17, 2] <- "10"
  raw[20, 1] <- "Gond"

  parsed <- parse_census_1991_st17_sheet(raw, "13", "01")

  expect_equal(nrow(parsed), 1L)
  expect_identical(parsed$mother_tongue, "Gondi")
  expect_equal(parsed$mother_tongue_speakers, 100)
})

test_that("ST language coverage fails closed when preferred validation disappears", {
  coverage <- data.frame(
    n_st17_districts = 2L,
    n_exact_st16_districts = 0L,
    n_st16_mismatch_districts = 0L,
    n_st16_unavailable_districts = 2L,
    n_mapped_language_cells = 4L,
    n_unresolved_language_cells = 1L,
    n_preferred_language_cells = 0L
  )
  expect_error(
    validate_census_1991_st_language_coverage(coverage),
    "no exact ST-16 validated districts"
  )

  coverage$n_exact_st16_districts <- 1L
  coverage$n_st16_unavailable_districts <- 1L
  expect_error(
    validate_census_1991_st_language_coverage(coverage),
    "no preferred mapped language cells"
  )

  coverage$n_preferred_language_cells <- 3L
  expect_silent(validate_census_1991_st_language_coverage(coverage))
})

test_that("historical Hindi-belt definition preserves 1991 state geography", {
  expect_setequal(
    census_1991_st_hindi_belt_state_codes(),
    c("05", "08", "09", "13", "20", "21", "25", "28", "31")
  )
})


test_that("ST language diagnostic estimates a state-FE distance slope on validated cells", {
  panel <- expand.grid(
    state_code_1991 = c("09", "13", "21"),
    shastry_distance_1991 = c(0, 3, 5),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  panel$district_code_1991 <- sprintf("%02d", seq_len(nrow(panel)))
  state_effect <- c("09" = 0.01, "13" = 0.03, "21" = -0.02)
  panel$english_acquisition_share <-
    0.10 + 0.02 * panel$shastry_distance_1991 + unname(state_effect[panel$state_code_1991])
  panel$mother_tongue_speakers <- seq(100, 100 + 10 * (nrow(panel) - 1), by = 10)
  panel$preferred_st_language_sample <- TRUE
  panel$hindi_belt_1991 <- TRUE

  fit <- census_1991_st_language_fit(
    panel, "english_acquisition_share", "validated_all_states", FALSE
  )
  expect_equal(fit$estimate, 0.02, tolerance = 1e-10)
  expect_equal(fit$n_states, 3L)
  expect_true(is.finite(fit$std_error_state_clustered))
})
