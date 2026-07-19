test_that("selection data builder returns enrolled fallback and standardized keys", {
  raw <- list(block = data.frame(State = "Bihar", District = "Patna", age = 10))

  out <- build_selection_data(raw, data.frame(), list())

  expect_true("enrolled" %in% names(out))
  expect_true(all(is.na(out$enrolled)))
  expect_equal(out$district_std, "patna")
})

test_that("NSS 2007 district metadata lookup is unique by district code", {
  metadata <- data.frame(
    name = c("district_code", "district_code", "STATE", "STATE", "region_code"),
    `ns1:catValu` = c("01001", "01001", "01", "01", "010"),
    `ns1:labl25` = c("Kupwara", "Kupwara", "Jammu & Kashmir", "Jammu & Kashmir", "Northern"),
    check.names = FALSE
  )

  lookup <- parse_2007_district_metadata(metadata)
  out <- attach_nss_2007_district_names(
    data.frame(district_code_0708 = "01001", child = 1L),
    metadata
  )

  expect_equal(nrow(lookup), 1L)
  expect_equal(nrow(out), 1L)
  expect_equal(out$district_0708, "Kupwara")
  expect_equal(out$region_0708, "Northern")
})

test_that("missingness diagnostics return a stable schema", {
  out <- diagnose_missingness(data.frame(a = c(1, NA), b = c(1, 2)), list())

  expect_s3_class(out, "emi_missingness_diagnostics")
  expect_true("missing_counts" %in% names(out))
  expect_true("a" %in% out$missing_counts$missing_var)
})

test_that("selection probit returns out-of-pipeline fallback without covariates", {
  out <- estimate_selection_probit(data.frame(enrolled = c(0, 1)), list())

  expect_equal(out$status, "out_of_active_pipeline")
  expect_equal(out$reason, "No probit covariates.")
})

test_that("selection probit fits toy glm when covariates are present", {
  selection_data <- data.frame(
    enrolled = c(0, 1, 0, 1, 0, 1),
    age = c(6, 7, 8, 9, 10, 11)
  )

  model <- estimate_selection_probit(selection_data, list())

  expect_s3_class(model, "glm")
  expect_equal(model$family$link, "probit")
})


test_that("missingness diagnostics write correlation figures and top-pair tables", {
  mat <- matrix(c(1, 0.4, -0.2, 0.4, 1, 0.8, -0.2, 0.8, 1), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("a", "b", "c")
  pairs <- missingness_correlation_pairs(mat, top_n = 2)

  expect_equal(nrow(pairs), 2L)
  expect_true(all(c("var1", "var2", "correlation", "abs_correlation") %in% names(pairs)))
})

test_that("district enrolled means compute weighted enrolled-child context", {
  df <- data.frame(
    district_code_0708 = c("001", "001", "001", "002"),
    enrolled = factor(c("Yes", "Yes", "No", "Yes"), levels = c("No", "Yes")),
    weight = c(1, 3, 10, 2),
    IS_EDU_FREE = factor(c("Yes", "No", "Yes", "Yes"), levels = c("Yes", "No")),
    ENROLLMENT_COST = c(0, 100, 999, 50)
  )

  out <- district_enrolled_means(df, vars = c("IS_EDU_FREE", "ENROLLMENT_COST"))

  expect_equal(out$dmean_num_IS_EDU_FREE[out$district_code_0708 == "001"], 0.25)
  expect_equal(out$dmean_num_ENROLLMENT_COST[out$district_code_0708 == "001"], 75)
  expect_equal(out$dmean_num_ENROLLMENT_COST[out$district_code_0708 == "002"], 50)
})

test_that("selection join dedupe collapses only identical duplicate rows", {
  identical_rows <- data.frame(id = c(1, 1), value = c("a", "a"))
  different_rows <- data.frame(id = c(1, 1), value = c("a", "b"))

  expect_equal(nrow(dedupe_selection_join_rows(identical_rows, "id")), 1L)
  expect_error(dedupe_selection_join_rows(different_rows, "id"), "non-identical rows")
})

test_that("selection child keys collapse identical rows and reject conflicts", {
  base <- data.frame(
    STATE = "01", FSU_SL_NO = "100", STRATUM = "01", SUB_STRATUM_NO = "01",
    HHID = "h1", PID = "p1", district_code_0708 = "01001", AGE = 10,
    stringsAsFactors = FALSE
  )
  identical_rows <- rbind(base, base)
  conflicting_rows <- rbind(base, transform(base, AGE = 11))

  expect_equal(nrow(enforce_selection_child_key_uniqueness(identical_rows)), 1L)
  expect_error(
    enforce_selection_child_key_uniqueness(conflicting_rows),
    "duplicate keys with non-identical rows",
    fixed = TRUE
  )
})


test_that("selection join identifiers discard incompatible haven labels", {
  skip_if_not_installed("haven")

  block5 <- data.frame(
    PID = haven::labelled(1, labels = c(child = 1)),
    AGE = 10,
    district_code = haven::labelled(101, labels = c(Patna = 101)),
    weight = haven::labelled(2, labels = c(sample_weight = 2)),
    FSU_SL_NO = haven::labelled(11, labels = c(fsu_a = 11)),
    HHID = haven::labelled(21, labels = c(household_a = 21)),
    STATE = haven::labelled(10, labels = c(Bihar = 10)),
    STRATUM = haven::labelled(1, labels = c(stratum_a = 1)),
    SUB_STRATUM_NO = haven::labelled(1, labels = c(substratum_a = 1)),
    stringsAsFactors = FALSE
  )
  block6 <- block5
  block6$PID <- haven::labelled(1, labels = c(person_one = 1))
  block6$FSU_SL_NO <- haven::labelled(11, labels = c(first_stage_unit = 11))

  normalized5 <- normalize_selection_identifiers(block5)
  normalized6 <- normalize_selection_identifiers(block6)
  joined <- safe_selection_full_join(
    normalized5,
    normalized6,
    selection_join_keys(enrolled = FALSE)
  )

  identifier_cols <- c("PID", "district_code_0708", "FSU_SL_NO", "HHID", "STATE", "STRATUM", "SUB_STRATUM_NO")
  expect_true(all(vapply(normalized5[identifier_cols], is.character, logical(1))))
  expect_type(normalized5$weight, "double")
  expect_equal(nrow(joined), 1L)
})
