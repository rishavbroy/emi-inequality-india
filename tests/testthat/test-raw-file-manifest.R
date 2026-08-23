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
    "data/raw/census_2001/languages/C16",
    "data/raw/district_boundaries_2020",
    "data/raw/district_changes"
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

  manifest_roots <- vapply(
    manifest$relative_path,
    function(path) any(startsWith(path, paste0(canonical_dirs, "/")) | path %in% canonical_dirs),
    logical(1)
  )
  expect_true(all(manifest_roots))
  expect_true(all(canonical_dirs %in% sources$local_raw_path))
})

test_that("optional manifest rows can be requested without entering the public preflight", {
  root <- tempfile("emi-optional-root-")
  dir.create(file.path(root, "data", "metadata"), recursive = TRUE)
  dir.create(file.path(root, "data", "raw", "optional"), recursive = TRUE)
  manifest <- data.frame(
    file_id = c("required", "optional"), source_id = c("required_source", "optional_source"),
    required_for_current_pipeline = c("true", "false"),
    relative_path = c("data/raw/required.csv", "data/raw/optional"),
    expected_size_bytes = NA_real_, file_type = c("csv", "directory"),
    reader_function = "reader", target_name = "target", notes = "",
    stringsAsFactors = FALSE
  )
  utils::write.csv(manifest, file.path(root, "data", "metadata", "file_manifest.csv"), row.names = FALSE, na = "")

  public <- manifest_rows(build_paths(root))
  optional <- manifest_rows(build_paths(root), source_id = "optional_source", required_only = FALSE)

  expect_identical(public$file_id, "required")
  expect_identical(optional$file_id, "optional")
  expect_true(optional$exists)
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

test_that("multi-source missing-data messages are scalar and list paths once", {
  root <- tempfile("emi-multi-source-root-")
  dir.create(file.path(root, "data", "metadata"), recursive = TRUE)
  dir.create(file.path(root, "data", "raw"), recursive = TRUE)
  writeLines("ok", file.path(root, "data", "raw", "present-a.csv"))
  manifest <- data.frame(
    file_id = c("present_a", "missing_b"),
    source_id = c("source_a", "source_b"),
    required_for_current_pipeline = "true",
    relative_path = c("data/raw/present-a.csv", "data/raw/missing-b.csv"),
    expected_size_bytes = NA_real_,
    file_type = "csv",
    reader_function = "reader",
    target_name = "target",
    notes = "",
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    manifest,
    file.path(root, "data", "metadata", "file_manifest.csv"),
    row.names = FALSE,
    na = ""
  )

  error <- tryCatch(
    require_manifest_files(build_paths(root), c("source_a", "source_b")),
    error = identity
  )
  message <- conditionMessage(error)

  expect_match(message, "source_a, source_b", fixed = TRUE)
  expect_equal(
    lengths(regmatches(message, gregexpr("data/raw/missing-b.csv", message, fixed = TRUE))),
    1L
  )
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

test_that("headerless district carve-out reader preserves the first observation", {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    "Anantapur,100,Anantapur,75,80",
    ",,Sri Sathya Sai,25,20"
  ), path)

  out <- read_district_carveouts(path)

  expect_equal(nrow(out), 2L)
  expect_equal(out$district_1991, c("Anantapur", "Anantapur"))
  expect_equal(out$pop_1991, c(100, 100))
  expect_equal(out$district_2001[[1]], "Anantapur")
  expect_equal(out$pct_91in01, c(80, 20))
})

test_that("manifest dispatches district carve-out rows to the explicit headerless reader", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("Anantapur,100,Anantapur,75,80", "Kurnool,200,Kurnool,100,100"), path)
  row <- data.frame(
    absolute_path = path,
    file_type = "csv",
    reader_function = "read_district_carveouts",
    stringsAsFactors = FALSE
  )

  out <- read_by_manifest_row(row)

  expect_equal(nrow(out), 2L)
  expect_equal(out$district_1991[[1]], "Anantapur")
})

test_that("data source catalogs are documented and uniquely identified", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  sources <- readr::read_csv(file.path(root, "data", "metadata", "data_sources.csv"), show_col_types = FALSE)
  lineage <- readr::read_csv(file.path(root, "data", "metadata", "district_lineage_sources.csv"), show_col_types = FALSE)

  expect_equal(anyDuplicated(sources$source_id), 0L)
  expect_equal(anyDuplicated(lineage$source_id), 0L)
  expect_true(all(c(
    "source_url", "access_date", "license_or_terms_notes", "notes"
  ) %in% names(sources)))
  expect_true(all(nzchar(sources$source_name)))
  expect_true(file.exists(file.path(root, "data", "metadata", "README.md")))
  expect_true(file.exists(file.path(root, "docs", "DISTRICT_LINEAGE.md")))
})

test_that("post-period LGD history is inventory-only lineage evidence", {
  specs <- district_lineage_input_specs(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  row <- specs[specs$source_id == "lgd_changes_post_2018", , drop = FALSE]

  expect_equal(nrow(row), 1L)
  expect_false(row$load_for_diagnostic)
  expect_equal(row$reader, "inventory_only")
  expect_equal(row$role, "post_2018_validation")
})

test_that("Census download manifest is canonical acquisition metadata", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  path <- file.path(root, "data", "metadata", "census_2001_download_manifest.tsv")
  manifest <- read.delim(path, stringsAsFactors = FALSE, check.names = FALSE)

  expect_identical(names(manifest), c("table", "state_code", "relative_path", "url"))
  expect_equal(anyDuplicated(manifest$relative_path), 0L)
  expect_equal(anyDuplicated(manifest$url), 0L)
  expect_true(all(startsWith(manifest$relative_path, "data/raw/census_2001/")))
  expect_true(all(startsWith(manifest$url, "https://censusindia.gov.in/")))
  expect_true(all(file.path("data", "raw", "census_2001", "languages", "C16", sprintf("PC01_C16_%02d.xls", 1:35)) %in% manifest$relative_path))
  expect_true(all(vapply(split(manifest$state_code, manifest$table), function(x) {
    identical(sort(unique(sprintf("%02d", as.integer(x)))), sprintf("%02d", 1:35))
  }, logical(1))))
})

test_that("tracked checksum inventory is complete and current", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  metadata <- list.files(
    file.path(root, "data", "metadata"),
    pattern = "\\.(csv|tsv)$",
    full.names = TRUE
  )
  processed <- list.files(
    file.path(root, "data", "processed"),
    pattern = "\\.csv$",
    full.names = TRUE
  )
  tracked <- sort(unique(c(metadata, processed)))
  tracked <- setdiff(tracked, file.path(root, "data", "metadata", "checksums.csv"))
  relative <- substring(tracked, nchar(root) + 2L)

  checksums <- read.csv(
    file.path(root, "data", "metadata", "checksums.csv"),
    stringsAsFactors = FALSE
  )

  expect_equal(anyDuplicated(checksums$path), 0L)
  expect_setequal(checksums$path, relative)
  expect_identical(
    unname(tools::md5sum(tracked[match(checksums$path, relative)])),
    checksums$md5
  )
})

test_that("Glottolog 5.3 source bundle is versioned and complete", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  manifest <- read.csv(file.path(root, "data", "metadata", "file_manifest.csv"), stringsAsFactors = FALSE)
  sources <- read.csv(file.path(root, "data", "metadata", "data_sources.csv"), stringsAsFactors = FALSE)

  direct <- manifest[manifest$source_id == "glottolog_5_3", , drop = FALSE]
  cldf <- manifest[manifest$source_id == "glottolog_cldf_5_3", , drop = FALSE]
  expect_setequal(direct$file_id, c("glottolog53_languoids", "glottolog53_newick", "glottolog53_geo"))
  expect_identical(cldf$file_id, "glottolog53_cldf")
  rows <- rbind(direct, cldf)
  expect_true(all(tolower(as.character(rows$required_for_current_pipeline)) == "true"))
  expect_true(all(startsWith(rows$relative_path, "data/raw/glottolog_5_3/")))
  expect_equal(anyDuplicated(rows$relative_path), 0L)

  direct_source <- sources[sources$source_id == "glottolog_5_3", , drop = FALSE]
  cldf_source <- sources[sources$source_id == "glottolog_cldf_5_3", , drop = FALSE]
  expect_equal(nrow(direct_source), 1L)
  expect_equal(nrow(cldf_source), 1L)
  expect_match(direct_source$license_or_terms_notes, "CC BY 4.0", fixed = TRUE)
  expect_match(cldf_source$source_url, "18840967", fixed = TRUE)
})


test_that("historical linguistic review sources are versioned with exact local contracts", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  manifest <- read.csv(file.path(root, "data", "metadata", "file_manifest.csv"), stringsAsFactors = FALSE)
  sources <- read.csv(file.path(root, "data", "metadata", "data_sources.csv"), stringsAsFactors = FALSE)

  rows <- manifest[manifest$source_id %in% c("ethnologue_newick_proxy", "dyen_1997", "kogan_2017", "asjp_v21"), , drop = FALSE]
  expect_setequal(rows$file_id, c("ethnologue_newick_proxy", "dyen1997_raw", "kogan2017_pdf", "asjp_v21_archive"))
  expect_true(all(tolower(as.character(rows$required_for_current_pipeline)) == "true"))
  expect_equal(
    rows$expected_size_bytes[match(c("ethnologue_newick_proxy", "dyen1997_raw", "kogan2017_pdf", "asjp_v21_archive"), rows$file_id)],
    c(464107, 849705, 400620, 16238442)
  )
  expect_identical(
    rows$relative_path[rows$file_id == "asjp_v21_archive"],
    "data/raw/cognates/asjp-v21.zip"
  )
  expect_true(all(startsWith(rows$relative_path, "data/raw/")))
  expect_equal(anyDuplicated(rows$relative_path), 0L)

  proxy <- sources[sources$source_id == "ethnologue_newick_proxy", , drop = FALSE]
  expect_match(proxy$notes, "not asserted to reproduce the exact Ethnologue vintage", fixed = TRUE)
  expect_match(sources$source_url[sources$source_id == "kogan_2017"], "10.31826/jlr-2017-143-411", fixed = TRUE)
  expect_match(sources$source_url[sources$source_id == "asjp_v21"], "10.5281/zenodo.16736409", fixed = TRUE)
  expect_match(sources$license_or_terms_notes[sources$source_id == "asjp_v21"], "CC BY 4.0", fixed = TRUE)
})

test_that("DISE archive is optional but fully documented", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  manifest <- read.csv(file.path(root, "data", "metadata", "file_manifest.csv"), stringsAsFactors = FALSE)
  sources <- read.csv(file.path(root, "data", "metadata", "data_sources.csv"), stringsAsFactors = FALSE)
  row <- manifest[manifest$source_id == "dise_district_report_cards", , drop = FALSE]
  source <- sources[sources$source_id == "dise_district_report_cards", , drop = FALSE]

  expect_equal(nrow(row), 1L)
  expect_identical(tolower(as.character(row$required_for_current_pipeline)), "false")
  expect_identical(row$relative_path, "data/raw/dise_internet_archive")
  expect_equal(nrow(source), 1L)
  expect_false(as.logical(source$used_in_current_pipeline))
  expect_match(source$license_or_terms_notes, "redistribution rights are not asserted", ignore.case = TRUE)
  expect_true(file.exists(file.path(root, "docs", "DISE_TREATMENTS.md")))
})
