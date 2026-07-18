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

repo_text <- function(...) {
  paste(readLines(repo_file(...), warn = FALSE), collapse = "\n")
}

test_that("current public build helper scripts parse", {
  expect_silent(parse(repo_file("_targets.R")))
  expect_silent(parse(repo_file("scripts", "check_required_outputs.R")))
  expect_silent(parse(repo_file("scripts", "run_targets_checked.R")))
  expect_silent(parse(repo_file("scripts", "run_targets_strict.R")))
  expect_silent(parse(repo_file("scripts", "target_metadata_helpers.R")))
  expect_silent(parse(repo_file("R", "output", "render_analysis_notes.R")))
  expect_silent(parse(repo_file("scripts", "check_rendered_text.R")))
  expect_silent(parse(repo_file("scripts", "audit_outputs_final.R")))
  expect_silent(parse(repo_file("scripts", "public_output_contract.R")))
  expect_silent(parse(repo_file("scripts", "check_report_values.R")))
  expect_silent(parse(repo_file("R", "output", "public_qmd_helpers.R")))
  expect_silent(parse(repo_file("R", "output", "report_value_core.R")))
  expect_silent(parse(repo_file("R", "output", "report_value_coefficients.R")))
  expect_silent(parse(repo_file("R", "output", "report_value_selection_ame.R")))
  expect_silent(parse(repo_file("R", "output", "report_value_spatial.R")))
  expect_silent(parse(repo_file("R", "application_samples", "extract_qmd_excerpts.R")))
})

test_that("legacy regeneration and parity audit scripts are retired from active paths", {
  root <- dirname(repo_file("README.md"))
  retired <- c(
    "scripts/rebuild_static_qmds_from_legacy.R",
    "scripts/postprocess_public_qmds.R",
    "scripts/audit_legacy_parity.py",
    "scripts/run_legacy_content_audit.sh",
    "scripts/migrate_repo_structure.py",
    "scripts/rename_raw_data_from_manifest.R"
  )
  for (path in retired) expect_false(file.exists(file.path(root, path)))
  expect_true(file.exists(file.path(root, "archive/refactoring/scripts/migrate_repo_structure.py")))
  expect_true(file.exists(file.path(root, "archive/refactoring/scripts/rename_raw_data_from_manifest.R")))

  makefile <- repo_text("Makefile")
  public_audit <- repo_text("scripts", "run_public_build_audit.sh")
  final_audit <- repo_text("scripts", "audit_outputs_final.R")

  expect_false(grepl("rebuild-qmds", makefile, fixed = TRUE))
  expect_false(grepl("audit-legacy-content", makefile, fixed = TRUE))
  expect_false(grepl("legacy-public-diagnostics", makefile, fixed = TRUE))
  expect_false(grepl("rebuild_static_qmds_from_legacy", public_audit, fixed = TRUE))
  expect_false(grepl("postprocess_public_qmds", public_audit, fixed = TRUE))
  expect_false(grepl("audit_legacy_parity", public_audit, fixed = TRUE))
  expect_false(grepl("audit_legacy_parity", final_audit, fixed = TRUE))
})

test_that("public render targets own final report, notes, and sample rendering", {
  targets <- repo_text("_targets.R")
  renderer <- repo_text("R", "output", "render_public_artifacts.R")
  samples <- repo_text("R", "application_samples", "render_writing_sample.R")

  expect_match(targets, 'tar_target(report_qmd, "paper/report.qmd", format = "file")', fixed = TRUE)
  expect_match(targets, 'tar_target(district_matching_qmd, "docs/district-matching.qmd", format = "file")', fixed = TRUE)
  expect_match(targets, 'render_public_html(district_matching_qmd, dependencies = list(report_values))', fixed = TRUE)
  expect_match(targets, 'tar_target(report, render_report_pdf(report_qmd, report_values, figure_files, table_files), format = "file")', fixed = TRUE)
  expect_match(targets, 'tar_target(application_sample_inputs, application_sample_input_files(), format = "file")', fixed = TRUE)
  expect_match(renderer, 'system2("quarto", c("render", report_qmd, "--to", "pdf"))', fixed = TRUE)
  expect_match(renderer, 'render_public_html <- function', fixed = TRUE)
  expect_match(samples, "application_sample_input_files", fixed = TRUE)
  expect_false(grepl("tar_render\\(report|tar_quarto\\(report", targets, perl = TRUE))
})

test_that("public audit checks current QMDs without regenerating them", {
  src <- repo_text("scripts", "run_public_build_audit.sh")

  expect_match(src, "paper/report.qmd R chunks parse", fixed = TRUE)
  expect_match(src, "READ-ONLY SOURCE WHITESPACE CHECK", fixed = TRUE)
  expect_match(src, "audit_status.json", fixed = TRUE)
  expect_false(grepl("perl -pi", src, fixed = TRUE))
  expect_false(grepl("--require-clean", src, fixed = TRUE))
  expect_false(grepl("require_clean_git_state", src, fixed = TRUE))
  expect_false(grepl("make rebuild-qmds", src, fixed = TRUE))
  expect_false(grepl("REBUILD GENERATED QMD SOURCES", src, fixed = TRUE))
})

test_that("public audit clean preserves extended diagnostics and benchmarks", {
  src <- repo_text("scripts", "run_public_build_audit.sh")
  makefile <- repo_text("Makefile")
  gitignore <- repo_text(".gitignore")

  expect_match(src, "--with-extended-diagnostics", fixed = TRUE)
  expect_match(src, "--with-benchmarks", fixed = TRUE)
  expect_match(src, "rm -rf outputs/diagnostics/build outputs/diagnostics/public", fixed = TRUE)
  expect_match(src, "rm -f outputs/diagnostics/*.csv", fixed = TRUE)
  expect_false(grepl("rm -rf outputs/diagnostics/\\*", src))
  expect_false(grepl("legacy root-level diagnostic", src, fixed = TRUE))
  expect_match(makefile, "clean:\n\t$(MAKE) clean-renders", fixed = TRUE)
  expect_match(makefile, "clean-all: clean clean-targets", fixed = TRUE)
  expect_match(makefile, "rm -rf outputs/figures/* outputs/tables/* outputs/diagnostics/build outputs/diagnostics/public", fixed = TRUE)
  expect_match(makefile, "rm -f outputs/diagnostics/*.csv", fixed = TRUE)
  expect_match(makefile, "clean-extended-diagnostics", fixed = TRUE)
  expect_match(makefile, "clean-benchmarking", fixed = TRUE)
  expect_match(gitignore, "outputs/diagnostics/*.csv", fixed = TRUE)
  expect_match(gitignore, "outputs/diagnostics/build/", fixed = TRUE)
  expect_match(gitignore, "outputs/diagnostics/public/", fixed = TRUE)
})


test_that("current-facing code avoids refactor-era naming for active helpers", {
  ame <- repo_text("R", "selection", "compute_average_marginal_effects.R")
  fuzzy <- repo_text("R", "districts", "build_district_join_map.R")
  audit <- repo_text("scripts", "run_public_build_audit.sh")

  expect_false(grepl("legacy_terms", ame, fixed = TRUE))
  expect_false(grepl("use_legacy_schema", ame, fixed = TRUE))
  expect_match(ame, "labeled_terms", fixed = TRUE)
  expect_match(ame, "use_labeled_schema", fixed = TRUE)
  expect_false(grepl("Legacy-compatible cascading district matcher", fuzzy, fixed = TRUE))
  expect_false(grepl("legacy Rmd", fuzzy, fixed = TRUE))
  expect_match(fuzzy, "sole active district-map authority", fixed = TRUE)
  expect_false(grepl("legacy root-level diagnostic", audit, fixed = TRUE))
})

test_that("targets graph separates public diagnostics, extended diagnostics, and benchmarks", {
  src <- repo_text("_targets.R")

  expect_match(src, "core_pipeline_targets <- list", fixed = TRUE)
  expect_match(src, "extended_diagnostic_targets <- list", fixed = TRUE)
  expect_match(src, "benchmark_targets <- list", fixed = TRUE)
  expect_match(src, "diag_public_iv_panel", fixed = TRUE)
  expect_match(src, "diag_public_spatial_autocorrelation", fixed = TRUE)
  expect_match(src, "diag_public_spatial_autocorrelation_files", fixed = TRUE)
  expect_match(src, "save_spatial_autocorrelation_diagnostics(diag_public_spatial_autocorrelation), format = \"file\"", fixed = TRUE)
  expect_match(src, "diag_ext_missingness", fixed = TRUE)
  expect_match(src, "bench_ame_methods", fixed = TRUE)
  expect_match(src, "EMI_RUN_EXTENDED_DIAGNOSTICS", fixed = TRUE)
  expect_match(src, "EMI_RUN_BENCHMARKS", fixed = TRUE)
  expect_false(grepl('diag_public_spatial_autocorrelation.*tar_cue\\(mode = "always"', src))
})

test_that("target warning metadata is written to build diagnostics", {
  strict <- repo_text("scripts", "run_targets_strict.R")
  helper <- repo_text("scripts", "target_metadata_helpers.R")
  audit <- repo_text("scripts", "run_public_build_audit.sh")

  expect_match(strict, "write_target_run_metadata(meta_active, \"strict\")", fixed = TRUE)
  expect_match(helper, "target_meta_after_", fixed = TRUE)
  expect_match(helper, "outputs/diagnostics/build/target_warnings.csv", fixed = TRUE)
  expect_match(audit, "outputs/diagnostics/build/target_warnings.csv", fixed = TRUE)
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

test_that("current QMD sources load shared public rendering helpers", {
  report <- repo_text("paper", "report.qmd")
  appendix <- repo_text("paper", "appendix.qmd")
  docs_note <- repo_text("docs", "district-matching.qmd")
  helper <- repo_text("R", "output", "public_qmd_helpers.R")

  expect_match(report, "public-output-table-helper", fixed = TRUE)
  expect_match(report, "source_public_qmd_helpers", fixed = TRUE)
  expect_match(appendix, "source_public_qmd_helpers", fixed = TRUE)
  expect_match(docs_note, "source_public_qmd_helpers", fixed = TRUE)
  expect_match(helper, "render_public_tex", fixed = TRUE)
  expect_match(helper, "knitr::asis_output(paste0", fixed = TRUE)
  expect_match(report, "\\usepackage{xcolor}", fixed = TRUE)
  expect_match(report, "\\definecolor{gray35}{gray}{0.35}", fixed = TRUE)
  expect_match(report, "\\usepackage{pdflscape}", fixed = TRUE)
})

test_that("report values use current named keys", {
  report <- repo_text("paper", "report.qmd")
  docs_note <- repo_text("docs", "district-matching.qmd")
  appendix <- repo_text("paper", "appendix.qmd")
  builder <- repo_text("R", "output", "build_report_values.R")
  spatial_values <- repo_text("R", "output", "report_value_spatial.R")
  checker <- repo_text("scripts", "check_report_values.R")

  expect_false(grepl("legacy_inline_expressions", report, fixed = TRUE))
  expect_false(grepl("legacy_inline_expressions", docs_note, fixed = TRUE))
  expect_false(grepl("legacy_inline_expressions", appendix, fixed = TRUE))
  expect_false(grepl("inline_", report, fixed = TRUE))
  expect_false(grepl("m_cons_resid$p.value", appendix, fixed = TRUE))
  expect_false(grepl("m_cons$p.value", appendix, fixed = TRUE))
  expect_false(grepl("add_inline_value", builder, fixed = TRUE))
  expect_match(report, "report_value(\"ame_edu_free_pct\")", fixed = TRUE)
  expect_match(docs_note, "report_value(\"moran_iv_residual_p\")", fixed = TRUE)
  expect_match(builder, "moran_iv_residual_p", fixed = TRUE)
  expect_match(builder, "moran_consumption_growth_p", fixed = TRUE)
  expect_match(spatial_values, "spatial_p_value", fixed = TRUE)
  expect_match(checker, "public_report_value_sources", fixed = TRUE)
  expect_match(checker, "pattern <-", fixed = TRUE)
  expect_match(checker, "gregexpr(pattern", fixed = TRUE)
})

test_that("public diagnostics are generated by current R code, not legacy parity audit", {
  targets <- repo_text("_targets.R")
  diagnostic <- repo_text("R", "diagnostics", "diagnose_public_iv_panel.R")

  expect_match(targets, "save_public_iv_panel_diagnostics(district_panel, tables)", fixed = TRUE)
  expect_match(diagnostic, "current-pipeline diagnostics, not legacy-parity checks", fixed = TRUE)
  expect_false(grepl("audit_legacy_parity", targets, fixed = TRUE))
})

test_that("public-output checks share one file contract", {
  contract <- repo_text("scripts", "public_output_contract.R")
  required <- repo_text("scripts", "check_required_outputs.R")
  final <- repo_text("scripts", "check_public_final.R")
  audit <- repo_text("scripts", "audit_outputs_final.R")

  expect_match(contract, "required_public_render_inputs", fixed = TRUE)
  expect_match(contract, "required_final_documents", fixed = TRUE)
  expect_match(contract, "required_final_artifacts", fixed = TRUE)
  expect_match(contract, "spatial_moran_tests.csv", fixed = TRUE)
  expect_match(contract, "spatial_moran_mc_reference.csv", fixed = TRUE)
  expect_match(required, "required_public_render_inputs()", fixed = TRUE)
  expect_match(final, "required_final_documents(require_application_samples)", fixed = TRUE)
  expect_match(audit, "required_final_artifacts()", fixed = TRUE)
})

test_that("optional diagnostics and benchmarking targets use checked targets wrapper", {
  makefile <- repo_text("Makefile")
  strict <- repo_text("scripts", "run_targets_checked.R")
  targets <- repo_text("_targets.R")

  expect_match(makefile, "run_targets_checked.R --starts-with diag_ext_", fixed = TRUE)
  expect_match(makefile, "run_targets_checked.R --starts-with bench_", fixed = TRUE)
  expect_match(strict, "tar_make", fixed = TRUE)
  expect_match(strict, "--starts-with", fixed = TRUE)
  expect_match(targets, "if (extended_diagnostics_enabled())", fixed = TRUE)
  expect_match(targets, "selected_targets <- c(selected_targets, extended_diagnostic_targets)", fixed = TRUE)
  expect_match(targets, "if (benchmarks_enabled())", fixed = TRUE)
  expect_match(targets, "selected_targets <- c(selected_targets, benchmark_targets)", fixed = TRUE)
  expect_false(grepl("rerun.*flag", targets))
})

test_that("fuzzy matching benchmarks use the canonical district fuzzy distance helpers", {
  source_attachment <- repo_text("R", "districts", "source_attachment.R")
  benchmark <- repo_text("R", "benchmarking", "benchmark_fuzzy_matching.R")
  fuzzy_distance <- repo_text("R", "districts", "fuzzy_distance.R")

  expect_match(source_attachment, "district_source_match_methods", fixed = TRUE)
  expect_match(benchmark, "district_fuzzy_match_methods", fixed = TRUE)
  expect_match(fuzzy_distance, "stringdist::stringdist", fixed = TRUE)
  expect_false(grepl("utils::adist", source_attachment, fixed = TRUE))
})

test_that("analysis notebooks render only to GitHub-flavored Markdown", {
  renderer <- repo_text("R", "output", "render_analysis_notes.R")
  wrapper <- repo_text("scripts", "render_analysis_notes.R")
  qmd <- repo_text("analysis", "benchmarking", "ame-benchmark.qmd")
  archive <- repo_text("scripts", "make_review_archive.sh")

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
  qmd <- repo_text("analysis", "io", "long-paths-and-8-3-filenames.qmd")
  expect_match(qmd, 'source(analysis_path("R", "io", "read_long_paths.R"))', fixed = TRUE)
  expect_match(qmd, "read_csv_short(tmp)", fixed = TRUE)
  expect_match(qmd, "get_windows_short_path(tmp)", fixed = TRUE)
  expect_match(qmd, "LongPathsEnabled", fixed = TRUE)
  expect_match(qmd, "readr:::standardise_path", fixed = TRUE)
  expect_match(qmd, "vroom", fixed = TRUE)
})

test_that("analysis notebooks retain prose-preservation markers and documented deviations", {
  qmds <- list.files(repo_file("analysis"), pattern = "[.]qmd$", recursive = TRUE, full.names = TRUE)
  text <- paste(vapply(qmds, function(x) paste(readLines(x, warn = FALSE), collapse = "\n"), character(1)), collapse = "\n")
  deviations <- repo_text("archive", "refactoring", "docs", "analysis_prose_deviations.md")

  expect_match(text, "Raw AME derivation is very slow", fixed = TRUE)
  expect_match(text, "Southern region of Rajasthan: People with one missing cost variable", fixed = TRUE)
  expect_match(text, "***Why the gigantic discrepancy???", fixed = TRUE)
  expect_match(text, "All of these Moran's I stats are ridiculously, suspiciously high", fixed = TRUE)
  expect_match(text, "Don't work even when diagnostics = FALSE", fixed = TRUE)
  expect_match(text, "analysis_deviation_note", fixed = TRUE)
  expect_match(deviations, "default rule is to keep legacy prose", fixed = TRUE)
})

test_that("review archives do not carry stale root-level diagnostic CSVs", {
  archive <- repo_text("scripts", "make_review_archive.sh")

  expect_match(archive, "find \"$tmpdir/outputs/diagnostics\" -maxdepth 1 -type f -name '*.csv' -delete", fixed = TRUE)
})

test_that("targets sources only R scripts from source directories", {
  targets <- repo_text("_targets.R")

  expect_match(targets, "tar_source_r <- function", fixed = TRUE)
  expect_match(targets, "list.files(path, pattern = \"\\\\.[Rr]$\", recursive = TRUE, full.names = TRUE)", fixed = TRUE)
  expect_false(grepl('tar_source\\("R/', targets))
  root <- dirname(repo_file("README.md"))
  expect_false(file.exists(file.path(root, "R", "districts", "join_district_panel.R")))
  expect_false(grepl("join_district_panel", targets, fixed = TRUE))
})

test_that("removed placeholder scaffolds do not return as runnable APIs", {
  files <- list.files(repo_file("R"), pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
  src <- paste(vapply(files, function(path) paste(readLines(path, warn = FALSE), collapse = "\n"), character(1)), collapse = "\n")
  removed <- c(
    "diagnose_model_robustness",
    "compare_no_iv_no_controls",
    "compare_no_iv_with_controls",
    "compare_iv_no_controls",
    "compare_iv_with_controls",
    "compare_state_fe_specs",
    "make_diagnostic_tables",
    "jackknife_first_stage_by_state",
    "jackknife_first_stage_by_region",
    "summarize_weak_iv_metrics",
    "build_fd_2sls_formula",
    "build_state_fe_2sls_formula",
    "compute_linguistic_distance_variants",
    "demean_iv_within_state",
    "tidy_first_stage_results",
    "compute_partial_f_statistics",
    "compute_partial_r2",
    "clean_edu0708_households",
    "clean_edu0708_members",
    "clean_edu0708_schooling",
    "clean_edu0708_private_expenditure",
    "standardize_edu0708_weights",
    "standardize_cons0708_hhid",
    "standardize_cons0708_weights",
    "standardize_edu1718_hhid",
    "standardize_edu1718_weights",
    "join_2017_state_district_labels",
    "standardize_census_state_names",
    "standardize_census_district_names",
    "compute_mother_tongue_population_shares",
    "add_boundary_join_ids",
    "collapse_or_expand_split_districts",
    "attach_spatial_ids",
    "compute_enrollment_share_2007",
    "compute_education_freebies_ivs_2007",
    "compute_2017_controls",
    "estimate_non_iv_comparisons",
    "join_panel_to_geometry",
    "assert_unique_panel_rows",
    "run_sargan_if_applicable",
    "run_gmm_overid_if_applicable"
  )
  for (fn in removed) {
    expect_false(grepl(paste0("\\b", fn, "\\s*<-\\s*function\\b"), src, perl = TRUE))
  }
})

test_that("selected target runner uses the programmatic targets API and shared warning metadata", {
  runner <- repo_text("scripts", "run_targets_checked.R")
  helper <- repo_text("scripts", "target_metadata_helpers.R")

  expect_match(runner, "targets::tar_make(names = tidyselect::all_of(selected_target_names))", fixed = TRUE)
  expect_false(grepl("eval(parse", runner, fixed = TRUE))
  expect_match(runner, "record_target_warnings", fixed = TRUE)
  expect_match(helper, "targets" , fixed = TRUE)
  expect_match(helper, "target_warnings.csv", fixed = TRUE)
})

test_that("selected target warning scope includes rebuilt dependencies", {
  env <- new.env(parent = globalenv())
  sys.source(repo_file("scripts", "target_metadata_helpers.R"), envir = env)
  before <- data.frame(
    name = c("selected", "dependency"),
    time = as.POSIXct(c("2026-01-01", "2026-01-01"), tz = "UTC"),
    error = c(NA_character_, NA_character_),
    warnings = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  after <- before
  after$time[[2]] <- as.POSIXct("2026-01-02", tz = "UTC")
  after$warnings[[2]] <- "dependency warning"

  changed <- env$changed_target_metadata_names(before, after)

  expect_equal(changed, "dependency")
  scoped <- env$select_target_metadata(after, unique(c("selected", changed)))
  expect_setequal(scoped$name, c("selected", "dependency"))
})

test_that("target warning metadata normalizes list columns and consolidates runs", {
  env <- new.env(parent = globalenv())
  sys.source(repo_file("scripts", "target_metadata_helpers.R"), envir = env)
  meta <- data.frame(name = c("a", "b"), stringsAsFactors = FALSE)
  meta$warnings <- I(list(c("first", "second"), character()))
  meta$error <- I(list(character(), character()))
  path <- tempfile(fileext = ".csv")

  normalized <- env$normalize_target_metadata(meta)
  env$record_target_warnings(normalized, "optional", path)

  expect_equal(normalized$warnings[[1]], "first; second")
  recorded <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(recorded$name, "a")
  expect_equal(recorded$run_label, "optional")
})

test_that("audit and archive scripts carry machine-readable run status", {
  audit <- repo_text("scripts", "run_public_build_audit.sh")
  archive <- repo_text("scripts", "make_review_archive.sh")

  expect_match(audit, "audit_status.json", fixed = TRUE)
  expect_match(audit, 'write_audit_status "failed"', fixed = TRUE)
  expect_match(audit, 'write_audit_status "passed" "complete"', fixed = TRUE)
  expect_match(archive, "standalone_archive", fixed = TRUE)
  expect_match(archive, 'cp -f "$tmpdir/outputs/diagnostics/build/audit_status.json" "$tmpdir/audit_status.json"', fixed = TRUE)
})

test_that("dependency and target-worker contracts avoid unused attachment machinery", {
  description <- repo_text("DESCRIPTION")
  targets <- repo_text("_targets.R")
  root <- dirname(repo_file("README.md"))

  expect_false(grepl("tarchetypes", description, fixed = TRUE))
  expect_false(grepl("fuzzyjoin", description, fixed = TRUE))
  expect_match(description, "Suggests:", fixed = TRUE)
  expect_match(description, "pdftools", fixed = TRUE)
  expect_match(description, "testthat", fixed = TRUE)
  expect_match(targets, "packages = character()", fixed = TRUE)
  expect_false(file.exists(file.path(root, "R", "packages.R")))
})

test_that("district harmonization crosswalk is the sole tracked tracker authority", {
  targets <- repo_text("_targets.R")
  diagnostics <- repo_text("R", "diagnostics", "diagnose_district_tracker_sources.R")
  root <- dirname(repo_file("README.md"))

  expect_false(grepl("processed_district_tracker_file", targets, fixed = TRUE))
  expect_match(targets, "prepare_district_join_map(district_harmonization_crosswalk)", fixed = TRUE)
  expect_match(targets, "build_district_panel(district_join_map, measures_2007", fixed = TRUE)
  expect_match(diagnostics, "data/metadata/district_harmonization_crosswalk.csv", fixed = TRUE)
  expect_false(file.exists(file.path(root, "data", "processed", "district_tracker_2001_2007_2017_2020.csv")))
})
