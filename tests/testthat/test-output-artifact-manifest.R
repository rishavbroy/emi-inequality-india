test_that("output artifact manifest preserves target identity and analysis links", {
  root <- tempfile("output-manifest-")
  dir.create(file.path(root, "outputs", "diagnostics", "extended", "iv"), recursive = TRUE)
  dir.create(file.path(root, "paper"), recursive = TRUE)

  registry <- data.frame(
    analysis_id = c("a1", "a2"),
    family = c("family_a", "family_b"),
    stringsAsFactors = FALSE
  )
  diagnostic <- data.frame(
    analysis_id = c("a1", "a2", "missing"),
    estimate = 1:3,
    stringsAsFactors = FALSE
  )
  csv_path <- file.path(root, "outputs", "diagnostics", "extended", "iv", "estimates.csv")
  utils::write.csv(diagnostic, csv_path, row.names = FALSE)
  manifest_path <- file.path(root, "outputs", "diagnostics", "build", "output_manifest.csv")
  dir.create(dirname(manifest_path), recursive = TRUE)
  utils::write.csv(data.frame(stale = TRUE), manifest_path, row.names = FALSE)
  writeLines("paper", file.path(root, "paper", "report.pdf"))

  meta <- data.frame(
    name = "diag_ext_estimates",
    format = "file",
    stringsAsFactors = FALSE
  )
  meta$path <- I(list(csv_path))

  out <- build_output_artifact_manifest(
    target_meta = meta,
    design_registry = registry,
    roots = c("outputs", "paper"),
    root = root
  )

  row <- out[out$path == "outputs/diagnostics/extended/iv/estimates.csv", , drop = FALSE]
  expect_equal(nrow(row), 1L)
  expect_identical(row$target_name, "diag_ext_estimates")
  expect_identical(row$output_scope, "extended_diagnostic")
  expect_equal(row$analysis_id_count, 3L)
  expect_equal(row$linked_analysis_id_count, 2L)
  expect_identical(row$analysis_families, "family_a;family_b")
  expect_true("paper/report.pdf" %in% out$path)
  expect_false("outputs/diagnostics/build/output_manifest.csv" %in% out$path)
})

test_that("output artifact manifest rejects multiple file targets claiming one path", {
  root <- tempfile("output-manifest-duplicate-")
  dir.create(file.path(root, "outputs"), recursive = TRUE)
  path <- file.path(root, "outputs", "x.csv")
  utils::write.csv(data.frame(x = 1), path, row.names = FALSE)

  meta <- data.frame(
    name = c("first", "second"),
    format = c("file", "file"),
    stringsAsFactors = FALSE
  )
  meta$path <- I(list(path, path))

  expect_error(
    build_output_artifact_manifest(
      target_meta = meta,
      design_registry = data.frame(analysis_id = character(), family = character()),
      roots = "outputs",
      root = root
    ),
    "Multiple file targets claim one output path",
    fixed = TRUE
  )
})

test_that("output artifact manifest scopes are semantic rather than filename-specific", {
  paths <- c(
    "outputs/diagnostics/build/a.csv",
    "outputs/diagnostics/public/a.csv",
    "outputs/diagnostics/extended/x/a.csv",
    "outputs/benchmarking/a.csv",
    "outputs/tables/main/a.csv",
    "outputs/figures/main/a.png",
    "paper/report.pdf",
    "docs/a.md",
    "posters/p/poster.pdf",
    "analysis/a.md",
    "application-samples/output/a.pdf"
  )
  expect_identical(
    output_artifact_scope(paths),
    c(
      "build_diagnostic", "public_diagnostic", "extended_diagnostic",
      "benchmark", "public_table", "public_figure", "paper",
      "documentation", "poster", "analysis_note", "application_sample"
    )
  )
})
