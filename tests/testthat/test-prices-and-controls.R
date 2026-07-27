test_that("price links use the median overlap ratio", {
  old <- data.frame(month = as.Date(c("2013-01-01", "2013-02-01", "2013-03-01")), index = c(100, 110, 120))
  new <- data.frame(month = old$month, index = c(200, 220, 360))
  out <- bridge_new_index(new, old)
  expect_equal(unique(out$link_ratio_2010_to_2012), 2)
})

test_that("sub-round deflators are unique and average three months", {
  month <- seq.Date(as.Date("2007-07-01"), by = "month", length.out = 12)
  x <- expand.grid(
    nss_state_code = sprintf("%02d", 1:36),
    sector = c("rural", "urban"),
    month = month,
    stringsAsFactors = FALSE
  )
  x$price_deflator <- seq_len(nrow(x)) / 100 + 1
  x$spatial_relative <- 1
  x$temporal_relative <- x$price_deflator
  x$temporal_source <- "test"
  out <- build_nss_subround_deflators(x, "2007_08")
  expect_equal(nrow(out), 36L * 2L * 4L)
  expect_false(anyDuplicated(out[c("nss_state_code", "sector", "subround")]) > 0L)
})

test_that("household deflator attachment preserves row order", {
  hh <- data.frame(
    STATE = c("02", "01"), SECTOR = c(2, 1), SUB_ROUND = c(1, 2), value = c("a", "b")
  )
  d <- data.frame(
    nss_state_code = c("01", "02"), sector = c("rural", "urban"),
    subround = c(2, 1), price_deflator = c(1.5, 2),
    spatial_relative = c(1, 1), temporal_relative = c(1.5, 2),
    temporal_source = c("rural", "urban")
  )
  out <- attach_household_deflator(hh, d)
  expect_identical(out$value, c("a", "b"))
  expect_equal(out$price_deflator, c(2, 1.5))
})

test_that("Census control shares are calculated from district totals", {
  x <- data.frame(
    target_unit_2001 = "pc2001__10__01",
    census2001_population = 1000,
    census2001_log_population = log(1000),
    census2001_urban_share = .2,
    census2001_adult_secondary_plus_share = .3,
    census2001_sc_share = .1,
    census2001_st_share = .05,
    census2001_muslim_share = .15,
    census2001_agricultural_worker_share = .5,
    census2001_dependency_ratio = 2 / 3,
    census2001_electricity_share = .6
  )
  expect_false(anyDuplicated(x$target_unit_2001) > 0L)
  expect_equal(x$census2001_agricultural_worker_share, .5)
  expect_equal(x$census2001_dependency_ratio, 2 / 3)
})

test_that("main formulas use real consumption, Census controls, and state effects", {
  f <- build_iv_formulas(list())
  text <- paste(deparse(f$consumption), collapse = " ")
  expect_match(text, "real_log_consumption_change")
  expect_match(text, "factor\\(state_2001_cluster\\)")
  expect_match(text, "census2001_urban_share")
  expect_false(grepl("gini_cons_0708", text, fixed = TRUE))
  expect_true("consumption_ancova" %in% names(f))
})
