hces_fixture_identity <- function(second_stage_stratum = "1", sample_household = "01") {
  data.frame(
    fsu = "00001",
    sector = "1",
    state = "01",
    nss_region = "011",
    district = "17",
    stratum = "13",
    sub_stratum = "01",
    panel = "1",
    sub_sample = "1",
    fod_sub_region = "0111",
    sample_su = "01",
    sample_subdivision = "",
    second_stage_stratum = second_stage_stratum,
    sample_household = sample_household,
    stringsAsFactors = FALSE
  )
}

hces_fixture_level14 <- function(identity = hces_fixture_identity()) {
  rows <- rbind(
    transform(identity, questionnaire = "F", section = "5.1", item_code = "129", value = "100"),
    transform(identity, questionnaire = "F", section = "6.1", item_code = "169", value = "70"),
    transform(identity, questionnaire = "C", section = "10.1", item_code = "409", value = "3650"),
    transform(identity, questionnaire = "C", section = "11.4", item_code = "539", value = "99999"),
    transform(identity, questionnaire = "D", section = "13.1", item_code = "379", value = "3650")
  )
  rows$household_id <- hces_household_id(rows)
  rows
}

hces_fixture_level15 <- function(identity = hces_fixture_identity(), round = "hces_2023_24") {
  rows <- rbind(
    transform(identity, questionnaire = "F", section = "A2", household_size = "2", multiplier = "11"),
    transform(identity, questionnaire = "C", section = "B2", household_size = "4", multiplier = "99"),
    transform(identity, questionnaire = "D", section = "C2", household_size = "5", multiplier = "22")
  )
  if (identical(round, "hces_2023_24")) {
    rows$visit <- c("2", "3", "1")
  } else {
    rows$multiplier <- "55"
  }
  rows$household_id <- hces_household_id(rows)
  rows
}

test_that("HCES summary metadata encodes documented reference periods and excludes imputed rent", {
  items <- read_hces_summary_items_file(
    hces_summary_items_path(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  )
  expect_equal(nrow(items), 42L)
  expect_setequal(unique(items$reference_days), c(7L, 30L, 365L))

  rent <- items[
    items$questionnaire == "C" & items$section == "11.4" & items$item_code == "539",
    , drop = FALSE
  ]
  expect_equal(nrow(rent), 1L)
  expect_false(rent$include_in_mpce[[1L]])
})

test_that("HCES summary reconstruction monthlyizes 7/30/365-day values and excludes item 539", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  items <- read_hces_summary_items_file(
    hces_summary_items_path(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  )
  spec <- consumption_survey_spec(registry, "hces_2023_24")
  level14 <- hces_fixture_level14()
  level15 <- hces_fixture_level15()
  level15$MONTHLY_CONSUMPTION_EXP <- "999999999"

  out <- canonicalize_hces_three_visit(level14, level15, spec, items)

  # F = 100 + 70 * 30/7 = 400; C = 3650 * 30/365 = 300;
  # D = 3650 * 30/365 = 300. Imputed rent contributes zero.
  # MPCE = 400/2 + 300/4 + 300/5 = 335.
  expect_equal(out$nominal_mpce, 335)
  expect_equal(out$household_size, 2)
  expect_equal(out$nominal_household_consumption, 670)
  expect_equal(out$survey_weight, 99)
})

test_that("HCES 2023-24 chooses the third-visit multiplier independently of questionnaire", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  spec <- consumption_survey_spec(registry, "hces_2023_24")
  visits <- hces_fixture_level15()
  checked <- validate_hces_visit_rows(visits, spec)

  expect_equal(checked$weights$survey_weight, 99)

  visits$visit <- c("1", "1", "3")
  expect_error(
    validate_hces_visit_rows(visits, spec),
    "visits 1, 2, and 3 exactly once"
  )
})

test_that("HCES 2022-23 requires equal questionnaire multipliers when visit order is absent", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  spec <- consumption_survey_spec(registry, "hces_2022_23")
  visits <- hces_fixture_level15(round = "hces_2022_23")

  checked <- validate_hces_visit_rows(visits, spec)
  expect_equal(checked$weights$survey_weight, 55)

  visits$multiplier[[3L]] <- "56"
  expect_error(
    validate_hces_visit_rows(visits, spec),
    "requires equal F/C/D multipliers"
  )
})

test_that("HCES household identity preserves second-stage-stratum distinctions", {
  one <- hces_fixture_identity(second_stage_stratum = "1", sample_household = "01")
  two <- hces_fixture_identity(second_stage_stratum = "2", sample_household = "01")
  ids <- hces_household_id(rbind(one, two))
  expect_equal(length(unique(ids)), 2L)
})

test_that("HCES reconstruction rejects missing questionnaire coverage and unregistered summaries", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  items <- read_hces_summary_items_file(
    hces_summary_items_path(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  )
  spec <- consumption_survey_spec(registry, "hces_2023_24")

  visits <- hces_fixture_level15()
  expect_error(
    validate_hces_visit_rows(visits[visits$questionnaire != "D", , drop = FALSE], spec),
    "exactly one F, C, and D"
  )

  bad <- hces_fixture_level14()
  bad$item_code[[1L]] <- "998"
  expect_error(
    hces_monthly_questionnaire_expenditure(bad, items),
    "unregistered summary item"
  )
})

test_that("HCES sparse Level 14 summaries zero-fill only against existing visits", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  items <- read_hces_summary_items_file(
    hces_summary_items_path(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  )
  spec <- consumption_survey_spec(registry, "hces_2023_24")
  level14 <- hces_fixture_level14()
  level15 <- hces_fixture_level15()

  level14 <- level14[level14$questionnaire != "D", , drop = FALSE]
  out <- canonicalize_hces_three_visit(level14, level15, spec, items)

  expect_equal(out$nominal_mpce, 275)
  coverage <- summarize_hces_summary_coverage(level14, level15, spec, items)
  expect_equal(coverage$n_summary_zero_filled[coverage$questionnaire == "D"], 1L)
  expect_true(all(coverage$n_summary_zero_filled[coverage$questionnaire != "D"] == 0L))
})

test_that("HCES rejects Level 14 summaries without a matching Level 15 visit", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  items <- read_hces_summary_items_file(
    hces_summary_items_path(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  )
  spec <- consumption_survey_spec(registry, "hces_2023_24")
  level14 <- hces_fixture_level14()
  extra <- level14[level14$questionnaire == "D", , drop = FALSE]
  extra$sample_household <- "02"
  extra$household_id <- hces_household_id(extra)
  level14 <- rbind(level14, extra)

  expect_error(
    canonicalize_hces_three_visit(level14, hces_fixture_level15(), spec, items),
    "summaries absent from Level 15"
  )
})

test_that("HCES household identity permits released optional sample-design blanks", {
  identity <- hces_fixture_identity()
  identity$sample_su <- ""
  identity$sample_subdivision <- NA_character_

  id <- hces_household_id(identity)
  expect_length(id, 1L)
  expect_true(nzchar(id))
})

test_that("HCES household identity still requires FSU, second-stage stratum, and household number", {
  identity <- hces_fixture_identity()

  missing_fsu <- identity
  missing_fsu$fsu <- ""
  expect_error(hces_household_id(missing_fsu), "incomplete household identity")

  missing_sss <- identity
  missing_sss$second_stage_stratum <- ""
  expect_error(hces_household_id(missing_sss), "incomplete household identity")

  missing_hh <- identity
  missing_hh$sample_household <- ""
  expect_error(hces_household_id(missing_hh), "incomplete household identity")
})

test_that("HCES bundle derives households and QA from the same release levels", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  items <- read_hces_summary_items_file(
    hces_summary_items_path(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  )
  spec <- consumption_survey_spec(registry, "hces_2023_24")
  level14 <- hces_fixture_level14()
  level15 <- hces_fixture_level15()

  level14 <- level14[level14$questionnaire != "D", , drop = FALSE]
  households <- canonicalize_hces_three_visit(level14, level15, spec, items)
  coverage <- summarize_hces_summary_coverage(level14, level15, spec, items)

  expect_equal(households$nominal_mpce, 275)
  expect_equal(
    coverage$n_summary_zero_filled[coverage$questionnaire == "D"],
    1L
  )
  expect_equal(sum(coverage$n_households), 3L)
})
