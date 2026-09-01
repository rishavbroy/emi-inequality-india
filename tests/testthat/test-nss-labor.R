test_that("NSS64 source normalizer preserves survey design and person keys", {
  raw <- data.frame(
    key_memb = c("a", "b"), Sector = c(1, 2), Sub_Round = c(1, 2),
    Sub_sample = c(1, 2), State_Region = c(101, 102), state = c(1, 1),
    District = c(2, 3), Stratum = c(11, 12), Sub_Stratum = c(1, 1),
    FSU = c(10001, 10002), Ss_stratum = c(1, 2), Sample_hhold_No = c(1, 1),
    wgt_combined = c(100.5, 200.5), B4_c1 = c(1, 2)
  )
  out <- normalize_nss64_design(raw, "B4_c1", "test")
  expect_equal(out$state_code, c("01", "01"))
  expect_equal(out$district_code, c("02", "03"))
  expect_equal(out$nss_region, c("101", "102"))
  expect_equal(out$survey_weight, c(100.5, 200.5))
  expect_equal(out$person_key, c("a", "b"))
})

test_that("NSS64 source normalizer fails closed on duplicate people or invalid weights", {
  raw <- data.frame(
    key_memb = c("a", "a"), Sector = 1, Sub_Round = 1, Sub_sample = 1,
    State_Region = 101, state = 1, District = 2, Stratum = 11,
    Sub_Stratum = 1, FSU = c(10001, 10002), Ss_stratum = 1,
    Sample_hhold_No = 1, wgt_combined = 100, B4_c1 = c(1, 2)
  )
  expect_error(normalize_nss64_design(raw, "B4_c1", "test"), "complete and unique")
  raw$key_memb <- c("a", "b")
  raw$wgt_combined[[2]] <- 0
  expect_error(normalize_nss64_design(raw, "B4_c1", "test"), "finite and positive")
})

test_that("NSS64 Block 4 and Block 6 validation requires one common person universe", {
  block4 <- data.frame(person_key = c("a", "b"), survey_weight = c(1, 2))
  block6 <- data.frame(person_key = c("b", "a"), survey_weight = c(2, 1))
  ddi <- data.frame(file_id = c("F4", "F6"), case_count = c(2, 2))
  out <- validate_nss64_source_pair(block4, block6, ddi)
  expect_equal(out$rows, c(2, 2))
  expect_true(all(out$positive_weight_share == 1))

  block6$person_key[[2]] <- "c"
  expect_error(validate_nss64_source_pair(block4, block6, ddi), "same household members")
})

test_that("analysis Quarto renderer retries only a SIGSEGV status once", {
  statuses <- c(139L, 0L)
  calls <- 0L
  runner <- function(command, args) {
    calls <<- calls + 1L
    statuses[[calls]]
  }
  expect_message(
    expect_identical(run_analysis_quarto_render("note.qmd", runner), 0L),
    "status 139"
  )
  expect_identical(calls, 2L)

  calls <- 0L
  runner_fail <- function(command, args) {
    calls <<- calls + 1L
    1L
  }
  expect_identical(run_analysis_quarto_render("note.qmd", runner_fail), 1L)
  expect_identical(calls, 1L)
})
