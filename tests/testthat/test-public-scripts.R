repo_file <- function(...) {
  candidates <- c(
    file.path(getwd(), ...),
    file.path(getwd(), "..", ...),
    file.path(getwd(), "..", "..", ...)
  )
  hits <- candidates[file.exists(candidates)]
  if (!length(hits)) {
    stop("Could not locate repository file: ", file.path(...), call. = FALSE)
  }
  normalizePath(hits[[1]], mustWork = TRUE)
}

test_that("public build helper scripts parse", {
  expect_silent(parse(repo_file("_targets.R")))
  expect_silent(parse(repo_file("scripts", "postprocess_public_qmds.R")))
  expect_silent(parse(repo_file("scripts", "check_required_outputs.R")))
  expect_silent(parse(repo_file("scripts", "run_targets_checked.R")))
  expect_silent(parse(repo_file("scripts", "check_rendered_text.R")))
  expect_silent(parse(repo_file("R", "application_samples", "extract_qmd_excerpts.R")))
})

test_that("report target renders the PDF artifact explicitly", {
  src <- paste(readLines(repo_file("_targets.R"), warn = FALSE), collapse = "\n")

  expect_match(src, "tar_target\\(report, render_report_pdf\\(report_values, figure_files, table_files\\), format = \"file\"\\)")
  expect_match(src, "quarto\", c\\(\"render\", \"paper/report\\.qmd\", \"--to\", \"pdf\"\\)")
  expect_match(src, "paper/report\\.pdf")
  expect_false(grepl("tar_render\\(report|tar_quarto\\(report", src, perl = TRUE))
})


test_that("public audit clean preserves extended diagnostics and benchmarks", {
  src <- paste(readLines(repo_file("scripts", "run_public_build_audit.sh"), warn = FALSE), collapse = "\n")
  makefile <- paste(readLines(repo_file("Makefile"), warn = FALSE), collapse = "\n")

  expect_match(src, "--with-extended-diagnostics", fixed = TRUE)
  expect_match(src, "--with-benchmarks", fixed = TRUE)
  expect_match(src, "rm -rf outputs/diagnostics/build outputs/diagnostics/public", fixed = TRUE)
  expect_false(grepl("rm -rf outputs/diagnostics/\\*", src))
  expect_match(makefile, "rm -rf outputs/figures/* outputs/tables/* outputs/diagnostics/build outputs/diagnostics/public", fixed = TRUE)
  expect_match(makefile, "clean-extended-diagnostics", fixed = TRUE)
  expect_match(makefile, "clean-benchmarking", fixed = TRUE)
})

test_that("targets graph separates public diagnostics, extended diagnostics, and benchmarks", {
  src <- paste(readLines(repo_file("_targets.R"), warn = FALSE), collapse = "\n")

  expect_match(src, "core_pipeline_targets <- list", fixed = TRUE)
  expect_match(src, "extended_diagnostic_targets <- list", fixed = TRUE)
  expect_match(src, "benchmark_targets <- list", fixed = TRUE)
  expect_match(src, "diag_public_spatial_autocorrelation", fixed = TRUE)
  expect_match(src, "diag_ext_missingness", fixed = TRUE)
  expect_match(src, "bench_ame_methods", fixed = TRUE)
  expect_match(src, "EMI_RUN_EXTENDED_DIAGNOSTICS", fixed = TRUE)
  expect_match(src, "EMI_RUN_BENCHMARKS", fixed = TRUE)
})

test_that("target warning metadata is written to build diagnostics", {
  strict <- paste(readLines(repo_file("scripts", "run_targets_strict.R"), warn = FALSE), collapse = "\n")
  audit <- paste(readLines(repo_file("scripts", "run_public_build_audit.sh"), warn = FALSE), collapse = "\n")

  expect_match(strict, "outputs/diagnostics/build/target_meta_after_strict_run.csv", fixed = TRUE)
  expect_match(strict, "outputs/diagnostics/build/target_warnings.csv", fixed = TRUE)
  expect_match(audit, "outputs/diagnostics/build/target_warnings.csv", fixed = TRUE)
})

test_that("postprocessor records legacy map placement and references-heading helpers", {
  src <- paste(readLines(repo_file("scripts", "postprocess_public_qmds.R"), warn = FALSE), collapse = "\n")

  expect_match(src, "Summary statistics for all of the variables in this model")
  expect_match(src, "collage_main_maps")
  expect_match(src, "We are currently unable to replicate her justification of the exclusion restriction")
  expect_match(src, "collage_iv_region_maps")
  expect_match(src, "ensure_references_heading")
})

test_that("writing sample YAML includes LaTeX table packages for raw table excerpts", {
  source(repo_file("R", "application_samples", "extract_qmd_excerpts.R"), local = TRUE)
  lines <- c(
    "---",
    "title: Test",
    "format:",
    "  pdf:",
    "    pdf-engine: xelatex",
    "---"
  )

  out <- normalize_sample_yaml(lines)

  expect_true(any(out == "  - \\usepackage{setspace}"))
  expect_true(any(out == "  - \\usepackage{threeparttable}"))
  expect_true(any(out == "  - \\usepackage{booktabs}"))
  expect_true(any(out == "  - \\usepackage{xcolor}"))
})


test_that("report raw TeX table chunks rely on TeX captions and load their dependencies", {
  src <- paste(readLines(repo_file("scripts", "postprocess_public_qmds.R"), warn = FALSE), collapse = "\n")

  expect_match(src, "\\usepackage{xcolor}", fixed = TRUE)
  expect_match(src, "\\definecolor{gray35}{gray}{0.35}", fixed = TRUE)
  expect_match(src, "\\usepackage{colortbl}", fixed = TRUE)
  expect_match(src, "\\usepackage{pdflscape}", fixed = TRUE)
  expect_match(src, "is_raw_tex", fixed = TRUE)
  expect_match(src, "knitr::asis_output(paste0", fixed = TRUE)
})

test_that("postprocessor places probit AME table after the explanatory paragraph", {
  src <- paste(readLines(repo_file("scripts", "postprocess_public_qmds.R"), warn = FALSE), collapse = "\n")

  expect_match(src, "@tbl-probit-mfx has been calculated over all observations", fixed = TRUE)
  expect_false(grepl(
    '"Average marginal effects for numeric variables and counterfactual comparisons",\\n    output_table_chunk\\("tbl-probit-mfx"',
    src,
    perl = TRUE
  ))
})


test_that("raw TeX table inclusion is emitted as an as-is block with block boundaries", {
  path <- file.path("scripts", "postprocess_public_qmds.R")
  if (!file.exists(path)) path <- file.path("..", "..", "scripts", "postprocess_public_qmds.R")
  src <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(src, "knitr::asis_output(paste0", fixed = TRUE)
  expect_match(src, "tex, ", fixed = TRUE)
})


test_that("standalone district matching note and appendix do not consume generated figure artifacts", {
  src <- paste(readLines(repo_file("scripts", "postprocess_public_qmds.R"), warn = FALSE), collapse = "\n")

  expect_false(grepl("insert_district_note_output_objects", src, fixed = TRUE))
  expect_match(src, paste0(
    "if (identical(path, \"paper/appendix.qmd\")) {\n",
    "    lines <- fix_district_note_crossrefs(lines)\n",
    "    lines <- fix_appendix_crossrefs(lines)\n",
    "  }"
  ), fixed = TRUE)
  expect_match(src, paste0(
    "if (identical(path, \"docs/district-matching.qmd\")) {\n",
    "    lines <- fix_district_note_crossrefs(lines)\n",
    "  }"
  ), fixed = TRUE)
  expect_match(src, "district carve-outs diagnostic figure generated with the main public artifacts", fixed = TRUE)
})

test_that("static QMD setup does not create fake output-file dependencies", {
  src <- paste(readLines(repo_file("scripts", "rebuild_static_qmds_from_legacy.R"), warn = FALSE), collapse = "\n")

  expect_false(grepl("report_output_files", src, fixed = TRUE))
})


test_that("postprocessor does not rewrite main-report prose", {
  src <- paste(readLines(repo_file("scripts", "postprocess_public_qmds.R"), warn = FALSE), collapse = "\n")

  expect_false(grepl("fix_final_public_prose", src, fixed = TRUE))
  expect_false(grepl("defer_unavailable_morans_i_values", src, fixed = TRUE))
  expect_false(grepl("Geospatial data intended for maps and spatial autocorrelation measures", src, fixed = TRUE))
  expect_false(grepl("collinear with the intercept in the active probit specification", src, fixed = TRUE))
  expect_false(grepl("validated district-panel geometry produced by the active tracker", src, fixed = TRUE))
  expect_false(grepl("regional linguistic divides thanks to state fixed effects", src, fixed = TRUE))
  expect_false(grepl("not comparable across all map variables", src, fixed = TRUE))
  expect_false(grepl("so we do not report Moran's I", src, fixed = TRUE))
})

test_that("report values retain restored legacy inline expressions", {
  src <- paste(readLines(repo_file("R", "output", "build_report_values.R"), warn = FALSE), collapse = "\n")

  expect_match(src, "IS_EDU_FREE", fixed = TRUE)
  expect_match(src, "m_cons_resid$p.value", fixed = TRUE)
  expect_match(src, "m_cons$p.value", fixed = TRUE)
})

test_that("legacy diagnostics and benchmarking coverage document tracks ported chunks", {
  src <- paste(readLines(repo_file("docs", "refactor", "legacy_diagnostics_benchmarking_coverage.md"), warn = FALSE), collapse = "\n")

  expect_match(src, "Chunk 8 missingness diagnostics", fixed = TRUE)
  expect_match(src, "Chunk 10 AME runtime/tuning", fixed = TRUE)
  expect_match(src, "Chunk 16 fuzzy", fixed = TRUE)
  expect_match(src, "Chunk 30 experimental spatial IV", fixed = TRUE)
  expect_match(src, "outputs/benchmarking", fixed = TRUE)
})

test_that("benchmark targets cover fuzzy matching, spatial weights, and spatial IV", {
  src <- paste(readLines(repo_file("_targets.R"), warn = FALSE), collapse = "\n")

  expect_match(src, "bench_fuzzy_matching", fixed = TRUE)
  expect_match(src, "bench_spatial_weights", fixed = TRUE)
  expect_match(src, "bench_spatial_iv_experimental", fixed = TRUE)
  expect_match(src, "save_missingness_diagnostics", fixed = TRUE)
  expect_match(src, "save_district_matching_diagnostics", fixed = TRUE)
})

test_that("optional target groups use checked targets wrapper", {
  makefile <- paste(readLines(repo_file("Makefile"), warn = FALSE), collapse = "\n")
  checked <- paste(readLines(repo_file("scripts", "run_targets_checked.R"), warn = FALSE), collapse = "\n")

  expect_match(makefile, "Rscript scripts/run_targets_checked.R --starts-with diag_ext_", fixed = TRUE)
  expect_match(makefile, "Rscript scripts/run_targets_checked.R --starts-with bench_", fixed = TRUE)
  expect_match(checked, "selected_target_names", fixed = TRUE)
  expect_match(checked, "targets::tar_make(names = all_of(selected_target_names))", fixed = TRUE)
  expect_match(checked, "Errored selected targets", fixed = TRUE)
  expect_match(checked, "quit(status = status)", fixed = TRUE)
})
