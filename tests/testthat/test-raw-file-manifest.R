test_that("file_manifest has required columns", {
  manifest <- readr::read_csv(file.path(Sys.getenv("EMI_PROJECT_ROOT", "."), "data", "metadata", "file_manifest.csv"), show_col_types = FALSE)
  expect_true(all(c("file_id", "source_id", "required_for_current_pipeline", "relative_path", "reader_function") %in% names(manifest)))
  expect_false(anyDuplicated(manifest$file_id))
})
