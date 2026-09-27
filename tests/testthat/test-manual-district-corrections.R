test_that("manual corrections expose the explicit identity schema", {
  corrections <- readr::read_csv(
    file.path(Sys.getenv("EMI_PROJECT_ROOT", "."), "data", "metadata", "manual_district_corrections.csv"),
    show_col_types = FALSE
  )
  expect_true(all(c(
    "reason", "side", "state_raw", "district_raw",
    "state_corrected", "district_corrected"
  ) %in% names(corrections)))
})

test_that("manual correction application records auditable correction table", {
  path <- tempfile(fileext = ".csv")
  corrections <- data.frame(
    correction_id = "c1",
    source_dataset = "toy",
    match_year = 2007L,
    side = "source",
    state_raw = "Bihar",
    district_raw = "Patna",
    state_corrected = "Bihar",
    district_corrected = "Patna",
    correction_type = "typo",
    reason = "Toy correction for test",
    stringsAsFactors = FALSE
  )
  utils::write.csv(corrections, path, row.names = FALSE)
  tracker <- data.frame(
    source_file_id = "toy",
    source_type = "toy",
    source_state_raw = "Bihar",
    source_district_raw = "Patna",
    source_year_raw = 2007L,
    target_state_raw = "Bihar",
    target_district_raw = "Patna",
    target_year_raw = 2008L,
    stringsAsFactors = FALSE
  )

  out <- apply_manual_district_corrections(tracker, path)

  expect_equal(attr(out, "manual_corrections")$reason, "Toy correction for test")
  expect_equal(attr(out, "manual_correction_audit")$n_matching_rows_before, 1L)
})

test_that("manual correction validation requires explicit fields", {
  expect_error(
    validate_manual_corrections(data.frame(correction_id = "c1"), data.frame()),
    "Manual corrections missing columns:"
  )
})

test_that("manual corrections mutate only the declared identity side and rebuild keys", {
  path <- tempfile(fileext = ".csv")
  corrections <- data.frame(
    correction_id = "c1",
    source_dataset = "toy",
    match_year = 2007L,
    side = "source",
    state_raw = "Bihar",
    district_raw = "Patna Old",
    state_corrected = "Bihar",
    district_corrected = "Patna",
    correction_type = "typo",
    reason = "Toy correction for test",
    stringsAsFactors = FALSE
  )
  utils::write.csv(corrections, path, row.names = FALSE)
  tracker <- data.frame(
    source_file_id = "toy",
    source_type = "toy",
    source_state_raw = "Bihar",
    source_district_raw = "Patna Old",
    source_year_raw = 2007L,
    target_state_raw = "Bihar",
    target_district_raw = "Patna Old",
    target_year_raw = 2008L,
    state_status = "Bihar",
    district_notes = "Patna Old",
    stringsAsFactors = FALSE
  )
  tracker <- standardize_tracker_names(tracker)

  out <- apply_manual_district_corrections(tracker, path)

  expect_equal(out$source_district_raw, "Patna")
  expect_equal(out$source_district_key, canon("Patna"))
  expect_equal(out$target_district_raw, "Patna Old")
  expect_equal(out$target_district_key, canon("Patna Old"))
  expect_equal(out$state_status, "Bihar")
  expect_equal(out$district_notes, "Patna Old")
})

test_that("manual name-correction API rejects lineage events", {
  corrections <- data.frame(
    correction_id = "c1",
    source_dataset = "toy",
    match_year = 2007L,
    side = "source",
    state_raw = "Bihar",
    district_raw = "Patna",
    state_corrected = "Bihar",
    district_corrected = "Patna",
    correction_type = "split",
    reason = "A lineage event belongs elsewhere",
    stringsAsFactors = FALSE
  )
  tracker <- data.frame(
    source_state_raw = "Bihar",
    source_district_raw = "Patna",
    source_year_raw = 2007L,
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_manual_corrections(corrections, tracker),
    "lineage events belong in reviewed lineage sources"
  )
})
