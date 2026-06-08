test_that("file_manifest has required columns", {
  manifest <- readr::read_csv(file.path(Sys.getenv("EMI_PROJECT_ROOT", "."), "data", "metadata", "file_manifest.csv"), show_col_types = FALSE)
  expect_true(all(c("file_id", "source_id", "required_for_current_pipeline", "relative_path", "reader_function") %in% names(manifest)))
  expect_equal(anyDuplicated(manifest$file_id), 0L)
})

test_that("manifest and data_sources use canonical raw-data directories", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  manifest <- readr::read_csv(file.path(root, "data", "metadata", "file_manifest.csv"), show_col_types = FALSE)
  sources <- readr::read_csv(file.path(root, "data", "metadata", "data_sources.csv"), show_col_types = FALSE)

  canonical_dirs <- c(
    "data/raw/nss_2007_education_64",
    "data/raw/nss_2007_consumption_64",
    "data/raw/nss_2017_education_75",
    "data/raw/census_2001_mother_tongue",
    "data/raw/district_boundaries_2020",
    "data/raw/district_changes"
  )
  legacy_dirs <- c(
    "data/raw/NSS 2007-08 Participation and Expenditure in Education 64th Round",
    "data/raw/NSS 2007-08 Household Consumer Expenditure Survey 64th Round",
    "data/raw/NSS 2017-18 Household Social Consumption Education 75th Round Data July 2017 - June 2018",
    "data/raw/Indian Census 2001",
    "data/raw/District Boundaries 2020",
    "data/raw/District Changes Data"
  )
  raw_source_ids <- c(
    "nss_2007_education",
    "nss_2007_consumption",
    "nss_2017_education",
    "census_2001_mother_tongue",
    "district_boundaries_2020",
    "district_changes"
  )
  manifest <- manifest[manifest$source_id %in% raw_source_ids, , drop = FALSE]
  sources <- sources[sources$source_id %in% raw_source_ids, , drop = FALSE]

  expect_false(any(grepl(paste(legacy_dirs, collapse = "|"), manifest$relative_path)))
  expect_false(any(grepl(paste(legacy_dirs, collapse = "|"), sources$local_raw_path)))
  manifest_roots <- vapply(
    manifest$relative_path,
    function(path) any(startsWith(path, paste0(canonical_dirs, "/")) | path %in% canonical_dirs),
    logical(1)
  )
  expect_true(all(manifest_roots))
  expect_true(all(canonical_dirs %in% sources$local_raw_path))
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

test_that("raw-data preflight reports all required missing files once", {
  root <- tempfile("emi-preflight-root-")
  dir.create(file.path(root, "data", "metadata"), recursive = TRUE)
  manifest <- data.frame(
    file_id = c("missing_a", "missing_b"),
    source_id = c("toy_a", "toy_b"),
    required_for_current_pipeline = "true",
    relative_path = c("data/raw/missing-a.csv", "data/raw/missing-b.csv"),
    expected_size_bytes = NA_real_,
    file_type = "csv",
    reader_function = "read_csv_short",
    target_name = c("raw_a", "raw_b"),
    notes = "",
    stringsAsFactors = FALSE
  )
  utils::write.csv(manifest, file.path(root, "data", "metadata", "file_manifest.csv"), row.names = FALSE, na = "")

  status <- validate_raw_files(build_paths(root))

  expect_error(
    stop_if_required_files_missing(status),
    "Place these files at the listed paths",
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
