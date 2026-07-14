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
  expect_silent(parse(repo_file("R", "output", "render_analysis_notes.R")))
  expect_silent(parse(repo_file("scripts", "check_rendered_text.R")))
  expect_silent(parse(repo_file("R", "application_samples", "extract_qmd_excerpts.R")))
})

test_that("public render targets own final report and sample rendering", {
  targets <- paste(readLines(repo_file("_targets.R"), warn = FALSE), collapse = "\n")
  renderer <- paste(readLines(repo_file("R", "output", "render_public_artifacts.R"), warn = FALSE), collapse = "\n")
  samples <- paste(readLines(repo_file("R", "application_samples", "render_writing_sample.R"), warn = FALSE), collapse = "\n")

  expect_match(targets, 'tar_target(report_qmd, "paper/report.qmd", format = "file")', fixed = TRUE)
  expect_match(targets, 'tar_target(report, render_report_pdf(report_qmd, report_values, figure_files, table_files), format = "file")', fixed = TRUE)
  expect_match(targets, 'tar_target(application_sample_inputs, application_sample_input_files(), format = "file")', fixed = TRUE)
  expect_match(renderer, 'system2("quarto", c("render", report_qmd, "--to", "pdf"))', fixed = TRUE)
  expect_match(renderer, 'format = "file"', fixed = TRUE)
  expect_match(samples, "application_sample_input_files", fixed = TRUE)
  expect_false(grepl("tar_render\\(report|tar_quarto\\(report", targets, perl = TRUE))
})



test_that("public audit clean preserves extended diagnostics and benchmarks", {
  src <- paste(readLines(repo_file("scripts", "run_public_build_audit.sh"), warn = FALSE), collapse = "\n")
  makefile <- paste(readLines(repo_file("Makefile"), warn = FALSE), collapse = "\n")
  gitignore <- paste(readLines(repo_file(".gitignore"), warn = FALSE), collapse = "\n")

  expect_match(src, "--with-extended-diagnostics", fixed = TRUE)
  expect_match(src, "--with-benchmarks", fixed = TRUE)
  expect_match(src, "rm -rf outputs/diagnostics/build outputs/diagnostics/public", fixed = TRUE)
  expect_false(grepl("rm -rf outputs/diagnostics/\\*", src))
  expect_match(makefile, "rm -rf outputs/figures/* outputs/tables/* outputs/diagnostics/build outputs/diagnostics/public", fixed = TRUE)
  expect_match(makefile, "clean-extended-diagnostics", fixed = TRUE)
  expect_match(makefile, "clean-benchmarking", fixed = TRUE)
  expect_match(gitignore, "outputs/diagnostics/build/", fixed = TRUE)
  expect_match(gitignore, "outputs/diagnostics/public/", fixed = TRUE)
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
  expect_match(src, "move_references_heading_to_end")
  expect_match(src, "fix_spatial_contiguity_note")
})



test_that("postprocessor moves references after the appendix and updates spatial contiguity prose", {
  src <- paste(readLines(repo_file("scripts", "postprocess_public_qmds.R"), warn = FALSE), collapse = "\n")

  expect_match(src, "move_references_heading_to_end", fixed = TRUE)
  expect_match(src, "Results currently reflect rook contiguity", fixed = TRUE)
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
  expect_match(src, "Chunk 15", fixed = TRUE)
  expect_match(src, "Chunk 20", fixed = TRUE)
  expect_match(src, "Chunk 30 experimental spatial IV", fixed = TRUE)
  expect_match(src, "outputs/benchmarking", fixed = TRUE)
})

test_that("benchmark targets cover fuzzy matching, spatial weights, and spatial IV", {
  src <- paste(readLines(repo_file("_targets.R"), warn = FALSE), collapse = "\n")

  expect_match(src, "bench_fuzzy_matching", fixed = TRUE)
  expect_match(src, "bench_spatial_weights", fixed = TRUE)
  expect_match(src, "bench_spatial_iv_experimental", fixed = TRUE)
  expect_false(grepl("bench_full_", src, fixed = TRUE))
  expect_false(grepl("EMI_RUN_BENCHMARKS_FULL", src, fixed = TRUE))
  expect_match(src, "save_missingness_diagnostics", fixed = TRUE)
  expect_match(src, "save_district_matching_diagnostics", fixed = TRUE)
  expect_match(src, "diag_ext_instrument_exploration", fixed = TRUE)
})

test_that("optional target groups use checked targets wrapper", {
  makefile <- paste(readLines(repo_file("Makefile"), warn = FALSE), collapse = "\n")
  checked <- paste(readLines(repo_file("scripts", "run_targets_checked.R"), warn = FALSE), collapse = "\n")

  expect_match(makefile, "Rscript scripts/run_targets_checked.R --starts-with diag_ext_", fixed = TRUE)
  expect_match(makefile, "Rscript scripts/run_targets_checked.R --starts-with bench_", fixed = TRUE)
  expect_match(checked, "selected_target_names", fixed = TRUE)
  expect_match(checked, "--targets TARGET[,TARGET...]", fixed = TRUE)
  expect_match(checked, "selected_names_call", fixed = TRUE)
  expect_match(checked, "tidyselect::all_of", fixed = TRUE)
  expect_match(checked, "Errored selected targets", fixed = TRUE)
  expect_match(checked, "quit(status = status)", fixed = TRUE)
})

test_that("legacy parity script can write diagnostics without running full audit", {
  audit <- paste(readLines(repo_file("scripts", "audit_legacy_parity.py"), warn = FALSE), collapse = "\n")

  expect_match(audit, "--write-diagnostics-only", fixed = TRUE)
  expect_match(audit, "write_public_diagnostics_only", fixed = TRUE)
  expect_match(audit, "audit_iv_panel_diagnostics()", fixed = TRUE)
})


test_that("analysis notebooks render through cached targets, not unconditional Quarto loops", {
  makefile <- paste(readLines(repo_file("Makefile"), warn = FALSE), collapse = "\n")
  targets <- paste(readLines(repo_file("_targets.R"), warn = FALSE), collapse = "\n")
  renderer <- paste(readLines(repo_file("R", "output", "render_analysis_notes.R"), warn = FALSE), collapse = "\n")

  expect_match(makefile, "public-diagnostics:", fixed = TRUE)
  expect_match(makefile, "legacy-public-diagnostics:", fixed = TRUE)
  expect_match(makefile, "Rscript scripts/run_targets_checked.R --starts-with diag_public_", fixed = TRUE)
  expect_match(makefile, "Rscript scripts/run_targets_checked.R --targets analysis_markdown_files", fixed = TRUE)
  expect_match(makefile, "EMI_RENDER_ANALYSIS_NOTES=true", fixed = TRUE)
  expect_match(makefile, "rerun-analysis:", fixed = TRUE)
  expect_false(grepl("benchmarking-full:", makefile, fixed = TRUE))
  expect_false(grepl("--starts-with bench_full_", makefile, fixed = TRUE))
  expect_match(targets, "analysis_note_targets <- list", fixed = TRUE)
  expect_match(targets, "tar_target\\(\\s*analysis_qmd_files")
  expect_match(targets, "tar_target\\(\\s*analysis_runtime_input_files")
  expect_match(targets, "tar_target\\(\\s*analysis_markdown_files")
  expect_match(targets, "pattern = map(analysis_qmd_files)", fixed = TRUE)
  expect_match(targets, "format = \"file\"", fixed = TRUE)
  expect_match(renderer, "render_analysis_markdown_file", fixed = TRUE)
  expect_match(renderer, "system2(\"quarto\", c(\"render\", qmd, \"--to\", \"gfm\"))", fixed = TRUE)
  expect_false(grepl("tar_render\\(bench_", targets))
  expect_false(grepl("tar_render\\(diag_ext_", targets))
})

test_that("analysis notebooks are populated with current-output tables", {
  qmd <- paste(readLines(repo_file("analysis", "benchmarking", "spatial-iv-benchmark.qmd"), warn = FALSE), collapse = "\n")
  helper <- paste(readLines(repo_file("analysis", "_analysis_helpers.R"), warn = FALSE), collapse = "\n")

  expect_match(qmd, "spatial_iv_model_status.csv", fixed = TRUE)
  expect_match(qmd, "spatial_iv_diagnostics_summary.csv", fixed = TRUE)
  expect_match(qmd, "methodological_success", fixed = TRUE)
  expect_match(helper, "knitr::kable", fixed = TRUE)
  expect_match(helper, "No rows in analysis output", fixed = TRUE)
  expect_match(helper, "Could not read analysis output", fixed = TRUE)
})



test_that("analysis notebooks cover remaining legacy diagnostic comments", {
  files <- c(
    repo_file("analysis", "diagnostics", "missingness-diagnostics.qmd"),
    repo_file("analysis", "diagnostics", "district-matching-diagnostics.qmd"),
    repo_file("analysis", "exploratory", "instrument-exploration.qmd"),
    repo_file("analysis", "diagnostics", "district-tracker-source-diagnostics.qmd")
  )
  text <- paste(vapply(files, function(x) paste(readLines(x, warn = FALSE), collapse = "\n"), character(1)), collapse = "\n")

  expect_match(text, "Rajasthan/Southern case study", fixed = TRUE)
  expect_match(text, "merge_dfs_into_tracker", fixed = TRUE)
  expect_match(text, "instrument-strength plots", fixed = TRUE)
  expect_match(text, "emie_by_district_dotplot.png", fixed = TRUE)
  expect_match(text, "Current EMIE-by-district dotplot data", fixed = TRUE)
  expect_match(text, "max_rows = 30", fixed = TRUE)
  expect_match(text, "tracker_legacy_expected_same_name_districts.csv", fixed = TRUE)
  expect_match(text, "missingness_chi_square_tests.csv", fixed = TRUE)
  expect_match(text, "missingness_rajasthan_southern_case_study.csv", fixed = TRUE)
  expect_match(text, "district_matching_all_rows_search.csv", fixed = TRUE)
  expect_match(text, "tracker_same_name_districts_by_year.csv", fixed = TRUE)
  expect_false(grepl("analysis_render_legacy_comments", text, fixed = TRUE))
})

test_that("analysis helpers read target-backed outputs without regex filename matching", {
  helper <- paste(readLines(repo_file("analysis", "_analysis_helpers.R"), warn = FALSE), collapse = "\n")
  long_paths <- paste(readLines(repo_file("analysis", "io", "long-paths-and-8-3-filenames.qmd"), warn = FALSE), collapse = "\n")

  expect_match(helper, "analysis_rel_path", fixed = TRUE)
  expect_match(helper, "analysis_read_target", fixed = TRUE)
  expect_match(helper, "analysis_target_csv", fixed = TRUE)
  expect_match(helper, "analysis_target_side_effect_path", fixed = TRUE)
  expect_match(helper, "diag_public_spatial_autocorrelation", fixed = TRUE)
  expect_match(helper, "analysis_image", fixed = TRUE)
  expect_match(helper, "endsWith(normalized_paths", fixed = TRUE)
  expect_false(grepl('gsub("([.^$|()', helper, fixed = TRUE))
  expect_false(grepl("analysis_render_source_file", helper, fixed = TRUE))
  expect_false(grepl("analysis_render_source_file", long_paths, fixed = TRUE))
  expect_false(grepl("analysis_is_code_like", helper, fixed = TRUE))
})



test_that("analysis relative image paths collapse to a scalar Markdown path", {
  helper <- paste(readLines(repo_file("analysis", "_analysis_helpers.R"), warn = FALSE), collapse = "\n")

  expect_match(helper, "do.call(file.path, as.list(parts))", fixed = TRUE)
  expect_match(helper, "rel[[1]]", fixed = TRUE)
  expect_false(grepl("rel <- file.path(c(rep", helper, fixed = TRUE))
})



test_that("map tuning analysis notebook is removed from diagnostics layer", {
  readme_path <- repo_file("analysis", "README.md")
  root <- dirname(dirname(readme_path))
  expect_false(file.exists(file.path(root, "analysis", "exploratory", "map-tuning.qmd")))
  expect_false(file.exists(file.path(root, "analysis", "exploratory", "map-tuning.md")))
  readme <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
  deviations <- paste(readLines(repo_file("docs", "refactor", "analysis_prose_deviations.md"), warn = FALSE), collapse = "\n")
  expect_false(grepl("map-tuning.qmd", readme, fixed = TRUE))
  expect_match(deviations, "Map palette/export tuning is intentionally excluded", fixed = TRUE)
})

test_that("public audit can include analysis notes in the same log", {
  src <- paste(readLines(repo_file("scripts", "run_public_build_audit.sh"), warn = FALSE), collapse = "\n")

  expect_match(src, "--with-analysis-notes", fixed = TRUE)
  expect_match(src, "with_analysis_notes=\"false\"", fixed = TRUE)
  expect_match(src, "with_extended_diagnostics=\"true\"", fixed = TRUE)
  expect_match(src, "with_benchmarks=\"true\"", fixed = TRUE)
  expect_false(grepl("--with-benchmarking-full", src, fixed = TRUE))
  expect_match(src, "=== ANALYSIS NOTES ===", fixed = TRUE)
  expect_match(src, "make render-analysis", fixed = TRUE)
  expect_false(grepl("make clean-analysis", src, fixed = TRUE))
  expect_match(src, "Analysis notes do not request application samples", fixed = TRUE)
  expect_match(src, "manifest_roots+=(analysis)", fixed = TRUE)
})

test_that("public checks use cached targets renders instead of direct Quarto renders", {
  makefile <- paste(readLines(repo_file("Makefile"), warn = FALSE), collapse = "\n")
  sample_script <- paste(readLines(repo_file("scripts", "render_application_samples.R"), warn = FALSE), collapse = "\n")

  expect_match(makefile, "$(MAKE) pipeline-final", fixed = TRUE)
  expect_false(grepl("HOME=\\$\\(QUARTO_HOME\\) quarto render", makefile))
  expect_false(grepl("Rscript scripts/render_application_samples.R", makefile, fixed = TRUE))
  expect_match(makefile, "run_targets_checked.R --targets report", fixed = TRUE)
  expect_match(makefile, "run_targets_checked.R --targets writing_sample_pdfs,coding_sample_pdfs", fixed = TRUE)
  expect_match(sample_script, "targets::tar_make", fixed = TRUE)
  expect_false(grepl("render_writing_samples\\(", sample_script))
  expect_false(grepl("render_coding_samples\\(", sample_script))
})

test_that("analysis notebooks render only to GitHub-flavored Markdown", {
  renderer <- paste(readLines(repo_file("R", "output", "render_analysis_notes.R"), warn = FALSE), collapse = "\n")
  wrapper <- paste(readLines(repo_file("scripts", "render_analysis_notes.R"), warn = FALSE), collapse = "\n")
  qmd <- paste(readLines(repo_file("analysis", "benchmarking", "ame-benchmark.qmd"), warn = FALSE), collapse = "\n")
  archive <- paste(readLines(repo_file("scripts", "make_review_archive.sh"), warn = FALSE), collapse = "\n")

  expect_match(renderer, "--to", fixed = TRUE)
  expect_match(renderer, "gfm", fixed = TRUE)
  expect_match(wrapper, "targets::tar_make", fixed = TRUE)
  expect_match(wrapper, "analysis_markdown_files", fixed = TRUE)
  expect_match(qmd, "format: gfm", fixed = TRUE)
  expect_false(grepl("pdf: default", qmd, fixed = TRUE))
  expect_false(grepl("html: default", qmd, fixed = TRUE))
  expect_match(archive, "GitHub-flavored Markdown", fixed = TRUE)
  expect_match(archive, "-name '*.html' -o -name '*.pdf' -o -name '*.tex' -o -name '*.log'", fixed = TRUE)
})


test_that("analysis long-path note contains runnable current code analogs", {
  qmd <- paste(readLines(repo_file("analysis", "io", "long-paths-and-8-3-filenames.qmd"), warn = FALSE), collapse = "\n")
  expect_match(qmd, 'source(analysis_path("R", "io", "read_long_paths.R"))', fixed = TRUE)
  expect_match(qmd, "read_csv_short(tmp)", fixed = TRUE)
  expect_match(qmd, "get_windows_short_path(tmp)", fixed = TRUE)
  expect_match(qmd, "LongPathsEnabled", fixed = TRUE)
  expect_match(qmd, "readr:::standardise_path", fixed = TRUE)
  expect_match(qmd, "vroom", fixed = TRUE)
})

test_that("analysis notebooks contain prose/current code directly instead of legacy extraction calls", {
  qmds <- list.files(repo_file("analysis"), pattern = "[.]qmd$", recursive = TRUE, full.names = TRUE)
  text <- paste(vapply(qmds, function(x) paste(readLines(x, warn = FALSE), collapse = "\n"), character(1)), collapse = "\n")

  expect_false(grepl("analysis_render_legacy_comments", text, fixed = TRUE))
  expect_false(grepl("echo=FALSE", text, fixed = TRUE))
  expect_match(text, "analysis_target_csv", fixed = TRUE)
  expect_match(text, "current_code_analog", fixed = TRUE)
  expect_match(text, "intersect(c(\"legacy_name\", \"statistic\", \"estimate\", \"p.value\", \"legacy_note\"), names(moran))", fixed = TRUE)
  expect_match(text, "missingness_correlation_all.png", fixed = TRUE)
  expect_match(text, "missingness_logit_pseudo_r2.png", fixed = TRUE)
  expect_false(any(grepl("map-tuning", qmds, fixed = TRUE)))
  expect_false(grepl("collage_main_maps.png", text, fixed = TRUE))
  expect_false(grepl("figure_files", text, fixed = TRUE))
  expect_match(text, "execute:\n  echo: true\n  output: true", fixed = TRUE)
})

test_that("analysis notes preserve legacy prose and document deviations", {
  qmds <- list.files(repo_file("analysis"), pattern = "[.]qmd$", recursive = TRUE, full.names = TRUE)
  text <- paste(vapply(qmds, function(x) paste(readLines(x, warn = FALSE), collapse = "\n"), character(1)), collapse = "\n")
  deviations <- paste(readLines(repo_file("docs", "refactor", "analysis_prose_deviations.md"), warn = FALSE), collapse = "\n")

  expect_match(text, "Raw AME derivation is very slow", fixed = TRUE)
  expect_match(text, "Southern region of Rajasthan: People with one missing cost variable", fixed = TRUE)
  expect_match(text, "***Why the gigantic discrepancy???", fixed = TRUE)
  expect_match(text, "Deviation from legacy prose: why the source-key inventory is large", fixed = TRUE)
  expect_match(text, "source-key inventory", fixed = TRUE)
  expect_match(text, "Number of rows from fuzzy full joining", fixed = TRUE)
  expect_match(text, "All of these Moran's I stats are ridiculously, suspiciously high", fixed = TRUE)
  expect_match(text, "Don't work even when diagnostics = FALSE", fixed = TRUE)
  expect_false(grepl("For the names of all color palette", text, fixed = TRUE))
  expect_match(text, "EMIE has three peaks", fixed = TRUE)
  expect_match(text, "0-100 percentage scale", fixed = TRUE)
  expect_match(text, "Purpose of this chunk: to ensure R can identify and read in all necessary files", fixed = TRUE)
  expect_match(text, "analysis_deviation_note", fixed = TRUE)

  expect_match(deviations, "default rule is to keep legacy prose", fixed = TRUE)
  expect_match(deviations, "Chunk 10", fixed = TRUE)
  expect_match(deviations, "Chunk 8", fixed = TRUE)
  expect_match(deviations, "Chunk 20", fixed = TRUE)
  expect_match(deviations, "Chunk 29", fixed = TRUE)
  expect_match(deviations, "Chunk 30", fixed = TRUE)
})
