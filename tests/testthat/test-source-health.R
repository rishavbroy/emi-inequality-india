test_that("source health distinguishes referenced, metadata-dispatched, and orphan functions", {
  root <- tempfile("source-health-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  dir.create(file.path(root, "data", "metadata"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  source_file <- file.path(root, "R", "functions.R")
  writeLines(c(
    "base::options(width = 80, digits = 3)",
    "used <- function(x) x + 1",
    "used_alias <- used",
    "orphan_alias <- used",
    "default_helper <- function() 1",
    "higher_order <- function(fun = used) fun()",
    "caller <- function(x = default_helper()) used(x)",
    "alias_caller <- function() used_alias(1)",
    "metadata_reader <- function(x) x",
    "inline_helper <- function() 1",
    "orphan <- function(x) x"
  ), source_file)
  qmd <- file.path(root, "note.qmd")
  writeLines(c("---", "title: test", "---", "", "Inline result: `r inline_helper()`"), qmd)
  metadata <- file.path(root, "data", "metadata", "file_manifest.csv")
  utils::write.csv(
    data.frame(reader_function = "metadata_reader", stringsAsFactors = FALSE),
    metadata, row.names = FALSE
  )

  report <- source_health_report(
    definition_paths = source_file,
    reference_paths = source_file,
    qmd_paths = qmd,
    metadata_path = metadata
  )
  status <- setNames(report$status, report$function_name)
  definition_type <- setNames(report$definition_type, report$function_name)
  expect_identical(definition_type[["used_alias"]], "alias")
  expect_identical(status[["used"]], "referenced")
  expect_identical(status[["used_alias"]], "referenced")
  expect_identical(status[["orphan_alias"]], "possible_orphan")
  expect_identical(status[["default_helper"]], "referenced")
  expect_identical(status[["higher_order"]], "possible_orphan")
  expect_identical(status[["caller"]], "possible_orphan")
  expect_identical(status[["metadata_reader"]], "metadata_reference")
  expect_identical(status[["inline_helper"]], "referenced")
  expect_identical(status[["orphan"]], "possible_orphan")
})

test_that("source health reports duplicate top-level function definitions", {
  root <- tempfile("source-health-duplicates-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  one <- file.path(root, "one.R")
  two <- file.path(root, "two.R")
  writeLines("same_name <- function() 1", one)
  writeLines("same_name <- function() 2", two)

  report <- source_health_report(
    definition_paths = c(one, two),
    reference_paths = c(one, two),
    qmd_paths = character(),
    metadata_path = file.path(root, "missing.csv")
  )
  expect_true(all(report$status == "multiple_definitions"))
})
