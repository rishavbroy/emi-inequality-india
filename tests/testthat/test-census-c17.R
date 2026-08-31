make_census_c17_fixture <- function(second_english_under_hindi = 10) {
  rows <- list(
    c("0900", "STATE - UTTAR PRADESH 09", "001000", "ASSAMESE", "100", "60", "40", rep("", 10)),
    c("0900", "STATE - UTTAR PRADESH 09", "001000", "ASSAMESE", rep("", 3), "040000", "ENGLISH", "20", "12", "8", rep("", 5)),
    c("0900", "STATE - UTTAR PRADESH 09", "001000", "ASSAMESE", rep("", 3), "040000", "ENGLISH", rep("", 3), "006000", "HINDI", "5", "3", "2"),
    c("0900", "STATE - UTTAR PRADESH 09", "001000", "ASSAMESE", rep("", 3), "006000", "HINDI", "30", "18", "12", rep("", 5)),
    c("0900", "STATE - UTTAR PRADESH 09", "001000", "ASSAMESE", rep("", 3), "006000", "HINDI", rep("", 3), "040000", "ENGLISH", as.character(second_english_under_hindi), "6", as.character(second_english_under_hindi - 6)),
    c("0900", "STATE - UTTAR PRADESH 09", "001000", "ASSAMESE", rep("", 3), "002000", "BENGALI", "10", "6", "4", rep("", 5)),
    c("0900", "STATE - UTTAR PRADESH 09", "001000", "ASSAMESE", rep("", 3), "002000", "BENGALI", rep("", 3), "040000", "ENGLISH", "2", "1", "1")
  )
  as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
}

test_that("C-17 parser preserves its hierarchical language counts", {
  rows <- parse_census_c17_sheet(make_census_c17_fixture())

  expect_equal(nrow(rows), 21L)
  expect_setequal(unique(rows$sex), c("Persons", "Males", "Females"))
  expect_equal(unique(rows$state_code), "09")
  expect_equal(unique(rows$state_name), "UTTAR PRADESH")
  expect_equal(unique(rows$native_language), "Assamese")
  expect_silent(validate_census_c17_hierarchy(rows))
})

test_that("C-17 collapse counts each multilingual speaker once", {
  rows <- parse_census_c17_sheet(make_census_c17_fixture())
  out <- collapse_census_c17_state_languages(rows)
  persons <- out[out$sex == "Persons", , drop = FALSE]

  expect_equal(persons$native_speakers, 100)
  expect_equal(persons$multilingual_speakers, 60)
  expect_equal(persons$english_multilingual_speakers, 32)
  expect_equal(persons$hindi_multilingual_speakers, 35)
  expect_equal(persons$multilingual_share_native, 60)
  expect_equal(persons$english_share_multilingual, 100 * 32 / 60)
  expect_equal(persons$hindi_share_multilingual, 100 * 35 / 60)
})

test_that("C-17 rejects subsidiary counts above their parent", {
  raw <- make_census_c17_fixture(second_english_under_hindi = 40)
  # Keep Persons = Males + Females so this specifically exercises hierarchy.
  raw[[16]][[5]] <- "20"
  raw[[17]][[5]] <- "20"
  rows <- parse_census_c17_sheet(raw)

  expect_error(
    validate_census_c17_hierarchy(rows),
    "exceed their first-subsidiary parent",
    fixed = TRUE
  )
})

test_that("C-17 native totals reconcile exactly to C-16 state totals", {
  c17 <- collapse_census_c17_state_languages(parse_census_c17_sheet(make_census_c17_fixture()))
  c16_raw <- data.frame(
    `...1` = "C0116", `...2` = "09", `...3` = "00", `...4` = "0000",
    `...5` = "State - Uttar Pradesh 09", `...6` = "001000",
    `...7` = "Assamese", `...8` = "100",
    check.names = FALSE, stringsAsFactors = FALSE
  )
  c16 <- census_2001_state_language_totals(list(c16_raw))

  expect_silent(validate_census_c17_against_c16(c17, c16))
  c16$native_speakers <- 99
  expect_error(validate_census_c17_against_c16(c17, c16), "do not reconcile exactly")
})

test_that("shared Census language normalization is reused by C-16 labels", {
  expect_equal(normalize_census_language_label(c("001 ASSAMESE", "  english ")), c("Assamese", "English"))
  out <- clean_mother_tongue_names(data.frame(mother_tongue = "001 ASSAMESE"))
  expect_equal(out$mother_tongue, "Assamese")
})
