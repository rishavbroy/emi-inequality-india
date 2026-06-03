test_that("file_manifest has required columns", {
  manifest <- readr::read_csv(file.path(Sys.getenv("EMI_PROJECT_ROOT", "."), "data", "metadata", "file_manifest.csv"), show_col_types = FALSE)
  expect_true(all(c("file_id", "source_id", "required_for_current_pipeline", "relative_path", "reader_function") %in% names(manifest)))
  expect_equal(anyDuplicated(manifest$file_id), 0L)
})

test_that("validate_raw_files resolves manifest paths from project root", {
  root <- tempfile("emi-manifest-root-")
  dir.create(file.path(root, "data", "metadata"), recursive = TRUE)
  dir.create(file.path(root, "data", "raw"), recursive = TRUE)
  writeLines("ok", file.path(root, "data", "raw", "present.csv"))

  manifest <- data.frame(
    file_id = c("present", "missing"),
    source_id = "toy_source",
    required_for_current_pipeline = "true",
    relative_path = c("data/raw/present.csv", "data/raw/missing.csv"),
    expected_size_bytes = NA_real_,
    file_type = "csv",
    reader_function = "read_csv_short",
    target_name = "toy_target",
    notes = "",
    stringsAsFactors = FALSE
  )
  utils::write.csv(manifest, file.path(root, "data", "metadata", "file_manifest.csv"), row.names = FALSE, na = "")

  paths <- build_paths(root)
  status <- validate_raw_files(paths)

  expect_true(status$exists[status$file_id == "present"])
  expect_false(status$exists[status$file_id == "missing"])
  expect_true(all(grepl(paths$root, status$absolute_path, fixed = TRUE)))
})

test_that("missing raw data fails through file_manifest message", {
  root <- tempfile("emi-missing-root-")
  dir.create(file.path(root, "data", "metadata"), recursive = TRUE)
  manifest <- data.frame(
    file_id = "missing",
    source_id = "toy_source",
    required_for_current_pipeline = "true",
    relative_path = "data/raw/missing.csv",
    expected_size_bytes = NA_real_,
    file_type = "csv",
    reader_function = "read_csv_short",
    target_name = "toy_target",
    notes = "",
    stringsAsFactors = FALSE
  )
  utils::write.csv(manifest, file.path(root, "data", "metadata", "file_manifest.csv"), row.names = FALSE, na = "")

  expect_error(
    require_manifest_files(build_paths(root), "toy_source"),
    "data/metadata/file_manifest.csv",
    fixed = TRUE
  )
})

test_that("district boundary reader validates shapefile sidecars before reading shp", {
  root <- tempfile("emi-shp-root-")
  dir.create(file.path(root, "data", "metadata"), recursive = TRUE)
  dir.create(file.path(root, "data", "raw", "shapes"), recursive = TRUE)
  writeLines("not a real shapefile", file.path(root, "data", "raw", "shapes", "toy.shp"))

  manifest <- data.frame(
    file_id = c("toy_shp", "toy_dbf"),
    source_id = "district_boundaries_2020",
    required_for_current_pipeline = "true",
    relative_path = c("data/raw/shapes/toy.shp", "data/raw/shapes/toy.dbf"),
    expected_size_bytes = NA_real_,
    file_type = c("shp", "dbf"),
    reader_function = "sf::st_read",
    target_name = "raw_boundaries_2020",
    notes = "",
    stringsAsFactors = FALSE
  )
  utils::write.csv(manifest, file.path(root, "data", "metadata", "file_manifest.csv"), row.names = FALSE, na = "")

  expect_error(
    read_district_boundaries_2020(build_paths(root)),
    "toy.dbf",
    fixed = TRUE
  )
})
