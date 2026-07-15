test_that("selection data builder returns enrolled fallback and standardized keys", {
  raw <- list(block = data.frame(State = "Bihar", District = "Patna", age = 10))

  out <- build_selection_data(raw, data.frame(), list())

  expect_true("enrolled" %in% names(out))
  expect_true(all(is.na(out$enrolled)))
  expect_equal(out$district_std, "patna")
})

test_that("legacy 2007 district metadata lookup is unique by district code", {
  metadata <- data.frame(
    name = c("district_code", "district_code", "STATE", "STATE", "region_code"),
    `ns1:catValu` = c("01001", "01001", "01", "01", "010"),
    `ns1:labl25` = c("Kupwara", "Kupwara", "Jammu & Kashmir", "Jammu & Kashmir", "Northern"),
    check.names = FALSE
  )

  lookup <- parse_2007_district_metadata(metadata)
  out <- attach_legacy_district_names(
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
