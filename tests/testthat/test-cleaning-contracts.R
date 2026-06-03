test_that("NSS 2007 education cleaner standardizes toy district columns", {
  raw <- list(block = data.frame(State = "Bihar", District = "Patna", value = 1))

  out <- clean_nss_2007_education(raw)

  expect_s3_class(out, "nss_2007_education_clean")
  expect_equal(out$block$state_std, "bihar")
  expect_equal(out$block$district_std, "patna")
  expect_equal(out$block$source_year, 2007L)
})

test_that("NSS consumption and 2017 education cleaners tolerate empty inputs", {
  cons <- clean_nss_2007_consumption(list(empty = data.frame()))$empty
  edu <- clean_nss_2017_education(list(empty = data.frame()))$empty

  expect_equal(nrow(cons), 0L)
  expect_equal(nrow(edu), 0L)
  expect_true("source_year" %in% names(cons))
  expect_true("source_year" %in% names(edu))
})

test_that("Census language cleaner parses area and mother-tongue labels", {
  raw <- list(c16 = data.frame(
    area_name = "District - Patna 2001",
    Language = "001 hindi",
    spkr_tot = 10
  ))

  out <- clean_census_2001_languages(raw)

  expect_equal(out$district_std, "patna")
  expect_equal(out$mother_tongue, "Hindi")
  expect_equal(out$source_year, 2001L)
})

test_that("district boundary cleaner handles common boundary name variants", {
  raw <- data.frame(ST_NM = "BIHAR", DT_NM = "PATNA")

  out <- clean_district_boundaries(raw)

  expect_equal(out$state_std, "bihar")
  expect_equal(out$district_std, "patna")
  expect_equal(out$source_year, 2020L)
})
