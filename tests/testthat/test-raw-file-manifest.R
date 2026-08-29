test_that("metadata CSV catalogs parse without field-loss warnings", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  catalogs <- c("file_manifest.csv", "data_sources.csv", "district_lineage_sources.csv")

  for (catalog in catalogs) {
    path <- file.path(root, "data", "metadata", catalog)
    parsed <- suppressWarnings(readr::read_csv(path, show_col_types = FALSE))
    issues <- readr::problems(parsed)
    expect_equal(
      nrow(issues),
      0L,
      info = if (nrow(issues)) paste(catalog, paste(utils::capture.output(print(issues)), collapse = "\n")) else catalog
    )
  }
})

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
  writeLines(c("Anantapur,100,Anantapur,100,80", "Kurnool,200,Kurnool,100,100"), path)
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

test_that("Census download manifests are canonical acquisition metadata", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  years <- c(2001L, 2011L)
  manifests <- lapply(years, function(year) {
    read.delim(
      file.path(root, "data", "metadata", sprintf("census_%d_download_manifest.tsv", year)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  for (i in seq_along(manifests)) {
    manifest <- manifests[[i]]
    expect_identical(names(manifest), c("table", "state_code", "relative_path", "url"))
    expect_equal(anyDuplicated(manifest$relative_path), 0L)
    expect_equal(anyDuplicated(manifest$url), 0L)
    expect_true(all(startsWith(
      manifest$relative_path,
      sprintf("data/raw/census_%d/", years[[i]])
    )))
    expect_true(all(startsWith(manifest$url, "https://censusindia.gov.in/")))
  }

  expected_codes <- sprintf("%02d", 1:35)
  expected_tables <- list(
    `2001` = c("C13", "C16", "C17"),
    `2011` = c(
      "C13", "B01", "B04", "B06", "B25A", "B25B", "C16", "C17",
      "D02", "D03", "D04", "D05", "D06", "D07",
      "HL04", "HL06", "HL07", "HL08", "HL09", "HL10", "HL11", "HL12", "HL13",
      "HH08", "HH10", "HH11"
    )
  )
  for (i in seq_along(manifests)) {
    manifest <- manifests[[i]]
    year <- as.character(years[[i]])
    for (table in expected_tables[[year]]) {
      rows <- manifest[manifest$table == table, , drop = FALSE]
      case_label <- paste(year, table)
      expect_equal(nrow(rows), 35L, label = case_label)
      expect_equal(
        sort(sprintf("%02d", as.integer(rows$state_code))),
        expected_codes,
        label = case_label
      )
    }
  }

  c13_2001 <- manifests[[1]][manifests[[1]]$table == "C13", ]
  c13_2011 <- manifests[[2]][manifests[[2]]$table == "C13", ]
  expect_setequal(
    basename(c13_2001$relative_path),
    sprintf("PC01_C13_%02d.xls", 1:35)
  )
  expect_setequal(
    basename(c13_2011$relative_path),
    sprintf("DDW-%02d00C-13.xls", 1:35)
  )

  expected_2011_roots <- c(
    B01 = "workers/B01", B04 = "workers/B04", B06 = "workers/B06",
    B25A = "workers/B25A", B25B = "workers/B25B",
    C16 = "languages/C16", C17 = "languages/C17",
    D02 = "migration/D02", D03 = "migration/D03", D04 = "migration/D04",
    D05 = "migration/D05", D06 = "migration/D06", D07 = "migration/D07",
    HL04 = "housing/HL04", HL06 = "housing/HL06", HL07 = "housing/HL07",
    HL08 = "housing/HL08", HL09 = "housing/HL09", HL10 = "housing/HL10",
    HL11 = "housing/HL11", HL12 = "housing/HL12", HL13 = "housing/HL13",
    HH08 = "households/HH08", HH10 = "households/HH10", HH11 = "households/HH11"
  )
  for (table in names(expected_2011_roots)) {
    rows <- manifests[[2]][manifests[[2]]$table == table, , drop = FALSE]
    expect_true(all(startsWith(
      rows$relative_path,
      file.path("data", "raw", "census_2011", expected_2011_roots[[table]])
    )), info = table)
  }
  expect_true(all(file.path(
    "data", "raw", "census_2001", "languages", "C17",
    sprintf("PC01_C17_%02d.xls", 1:35)
  ) %in% manifests[[1]]$relative_path))
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


test_that("1991 Atlas state and PCA review inputs have explicit source contracts", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  crosswalk <- readr::read_csv(
    file.path(root, "data", "metadata", "language_atlas_1991_state_crosswalk.csv"),
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
  manifest <- readr::read_csv(
    file.path(root, "data", "metadata", "file_manifest.csv"),
    show_col_types = FALSE
  )

  expected_codes <- sprintf("%02d", setdiff(2:33, 10))
  expect_equal(nrow(crosswalk), 31L)
  expect_setequal(crosswalk$state_code_1991, expected_codes)
  expect_equal(anyDuplicated(crosswalk$atlas_label_raw), 0L)
  expect_equal(anyDuplicated(crosswalk$state_code_1991), 0L)
  expect_false("10" %in% crosswalk$state_code_1991)

  pca <- manifest[manifest$file_id == "shrug_pca91_archive", , drop = FALSE]
  expect_equal(nrow(pca), 1L)
  expect_false(as.logical(pca$required_for_current_pipeline))
  expect_identical(pca$relative_path, "data/raw/shrug/census_1991/shrug-pca91-csv.zip")
  expect_equal(pca$expected_size_bytes, 42996056)

  languages <- readr::read_csv(
    file.path(root, "data", "metadata", "language_atlas_1991_languages.csv"),
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
  expect_equal(nrow(languages), 114L)
  expect_identical(as.integer(languages$atlas_column), 4:117)
  expect_equal(anyDuplicated(tolower(languages$language_1991)), 0L)
  expect_true(all(languages$review_status == "accepted"))

  sources <- readr::read_csv(
    file.path(root, "data", "metadata", "data_sources.csv"),
    show_col_types = FALSE
  )
  atlas <- sources[sources$source_id == "census_1991_language_atlas", , drop = FALSE]
  expect_equal(nrow(atlas), 1L)
  expect_true(as.logical(atlas$used_in_current_pipeline))
  expect_identical(atlas$current_or_future, "current")
  expect_identical(
    atlas$local_raw_path,
    "data/raw/census_1961-91/Language_Atlas_of_India_1991.pdf"
  )
  checksums <- readr::read_csv(
    file.path(root, "data", "metadata", "checksums.csv"),
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
  accepted <- checksums[
    checksums$path == "data/metadata/language_atlas_1991_accepted_source.csv",
    , drop = FALSE
  ]
  expect_equal(nrow(accepted), 1L)
  expect_true(nzchar(accepted$md5[[1L]]))

  cell_reviews <- readr::read_csv(
    file.path(root, "data", "metadata", "language_atlas_1991_cell_reviews.csv"),
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
  expect_setequal(
    names(cell_reviews),
    c(
      "state_code_1991", "district_code_1991", "atlas_column",
      "review_decision", "reviewed_speaker_count", "expected_page",
      "expected_raw_value", "expected_candidate_count", "expected_parse_status",
      "expected_alignment_status", "review_basis"
    )
  )
  expect_true(all(
    cell_reviews$review_decision %in%
      c("accept_extracted", "replace_count", "leave_unresolved")
  ))
  expect_equal(
    anyDuplicated(paste(cell_reviews$state_code_1991, cell_reviews$district_code_1991, cell_reviews$atlas_column)),
    0L
  )
  expect_true(all(grepl("^[0-9]+$", cell_reviews$expected_page)))
  expect_true(all(nzchar(cell_reviews$review_basis)))
  accept_extracted <- cell_reviews$review_decision == "accept_extracted"
  expect_true(all(
    is.na(cell_reviews$reviewed_speaker_count[accept_extracted]) |
      !nzchar(cell_reviews$reviewed_speaker_count[accept_extracted])
  ))

  expect_true(all(nzchar(languages$canonical_language)))
  expect_setequal(
    languages$shastry_family_class,
    c("indo_european", "non_indo_european", "special_english")
  )
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

test_that("Vanneman historical source QA is manifest-backed but values remain provenance-gated", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  manifest <- read.csv(file.path(root, "data", "metadata", "file_manifest.csv"), stringsAsFactors = FALSE)
  sources <- read.csv(file.path(root, "data", "metadata", "data_sources.csv"), stringsAsFactors = FALSE)

  rows <- manifest[manifest$source_id == "vanneman_1961_91", , drop = FALSE]
  expect_setequal(
    rows$file_id,
    c("vanneman_panel4", "vanneman_dist81", "vanneman_dist91", "vanneman_codebook")
  )
  expect_true(all(tolower(as.character(rows$required_for_current_pipeline)) == "true"))
  expect_true(all(startsWith(rows$relative_path, "data/raw/census_1961-91/vanneman_1961-91/")))

  source <- sources[sources$source_id == "vanneman_1961_91", , drop = FALSE]
  expect_equal(nrow(source), 1L)
  expect_true(as.logical(source$used_in_current_pipeline))
  expect_identical(source$current_or_future, "current")
  expect_true(all(rows$target_name == "historical_vanneman_source_qa"))
})

test_that("district carve-out wrapped-label joins distinguish word wraps from semantic hyphens", {
  expect_identical(join_district_carveout_wrapped_label("Pasumpon M. The-", "var"), "Pasumpon M. Thevar")
  expect_identical(join_district_carveout_wrapped_label("Chengalpattu-", "MGR"), "Chengalpattu-MGR")
  expect_identical(join_district_carveout_wrapped_label("Gautam Buddha", "Nagar"), "Gautam Buddha Nagar")
  expect_identical(join_district_carveout_wrapped_label("Jyotiba Phule Na-", "gar"), "Jyotiba Phule Nagar")
})

test_that("district carve-out reader repairs source-table wrapped labels structurally", {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    'Chengalpattu-,"4,653,593",Kancheepuram,51.9,100',
    'MGR,,Thiruvallur,48.1,100',
    'Pasumpon M. The-,"1,078,190",Sivaganga,100,97.74',
    'var,,,,',
    'North 24 Para-,"7,281,881",North Twenty Four,100,100',
    'ganas,,Parganas,,',
    'Bulandshahr,200,Gautam Buddha,100,44.89',
    ',,Nagar,,',
    'Next,100,Next,100,100'
  ), path)

  out <- read_district_carveouts(path)

  expect_equal(nrow(out), 6L)
  expect_equal(out$district_1991[1:2], rep("Chengalpattu-MGR", 2))
  expect_equal(out$pop_1991[1:2], rep(4653593, 2))
  expect_equal(out$district_1991[[3]], "Pasumpon M. Thevar")
  expect_equal(out$district_1991[[4]], "North 24 Paraganas")
  expect_equal(out$district_2001[[4]], "North Twenty Four Parganas")
  expect_equal(out$district_2001[[5]], "Gautam Buddha Nagar")
  expect_false(any(out$district_1991 %in% c("MGR", "var", "ganas")))
})

test_that("district carve-out reader enforces parent-share partitions", {
  valid <- tempfile(fileext = ".csv")
  writeLines(c(
    'Parent,100,Child A,60,100',
    ',,Child B,40.01,100'
  ), valid)
  expect_silent(read_district_carveouts(valid))

  incomplete <- tempfile(fileext = ".csv")
  writeLines(c(
    'Parent,100,Child A,60,100',
    ',,Child B,30,100'
  ), incomplete)
  expect_error(
    read_district_carveouts(incomplete),
    "source shares must sum to 100"
  )

  out_of_range <- tempfile(fileext = ".csv")
  writeLines('Parent,100,Child,100,100.01', out_of_range)
  expect_error(read_district_carveouts(out_of_range), "must lie in \\[0, 100\\]")
})

test_that("unexpected carve-out continuation rows fail closed", {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    'Ordinary,100,Ordinary,50,50',
    'Unexpected,,Other,50,50'
  ), path)

  expect_error(read_district_carveouts(path), "Unexpected wrapped row")
})

test_that("SHRUG 1991 baseline archives are manifest-backed optional diagnostics", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  manifest <- read.csv(file.path(root, "data", "metadata", "file_manifest.csv"), stringsAsFactors = FALSE)
  rows <- manifest[manifest$file_id %in% c("shrug_pca91_archive", "shrug_vd91_archive", "shrug_td91_archive"), , drop = FALSE]

  expect_setequal(rows$file_id, c("shrug_pca91_archive", "shrug_vd91_archive", "shrug_td91_archive"))
  expect_true(all(tolower(as.character(rows$required_for_current_pipeline)) == "false"))
  expect_true(all(startsWith(rows$relative_path, "data/raw/shrug/census_1991/")))
  expect_true(all(grepl("91-csv\\.zip$", rows$relative_path)))
})
