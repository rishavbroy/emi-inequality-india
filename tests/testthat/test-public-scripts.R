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

repo_extended_target_text <- function() {
  pipeline_dir <- repo_file("R", "pipeline")
  files <- sort(list.files(
    pipeline_dir,
    pattern = "^extended_.*_targets\\.R$",
    full.names = TRUE
  ))
  paste(
    unlist(lapply(files, readLines, warn = FALSE), use.names = FALSE),
    collapse = "\n"
  )
}

git_attribute <- function(path, attribute) {
  root <- dirname(repo_file(".gitattributes"))
  output <- suppressWarnings(system2(
    "git",
    c(
      "-C", shQuote(root),
      "check-attr", shQuote(attribute),
      "--", shQuote(path)
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop(
      "git check-attr failed for ", path, ": ",
      paste(output, collapse = "\n"),
      call. = FALSE
    )
  }
  if (length(output) != 1L) {
    stop("git check-attr returned unexpected output for ", path, call. = FALSE)
  }
  sub("^.*: [^:]+: ", "", output[[1]])
}

test_that("rendered and archived artifacts are treated as binary by Git", {
  artifacts <- c(
    "posters/2026_predoc_conference/poster.pdf",
    "outputs/figures/main/map_emi_exposure.png",
    "docs/plan/TO-DO Research Paper ECON 623.docx"
  )

  expect_identical(
    unname(vapply(artifacts, git_attribute, character(1), attribute = "diff")),
    rep("unset", length(artifacts))
  )
})

test_that("shared Census transition is a first-class pipeline dependency", {
  targets <- repo_text("_targets.R")
  declaration <- paste0(
    "tar_target(\n    district_transition_2001_2011,\n",
    "    district_lineage$district_transition_2001_2011\n  )"
  )
  expect_match(targets, declaration, fixed = TRUE)

  nested_access <- gregexpr(
    "district_lineage$district_transition_2001_2011",
    targets, fixed = TRUE
  )[[1L]]
  expect_equal(sum(nested_access > 0L), 1L)

  for (consumer in c(
    "build_consumption_lineage_reference",
    "build_census_age_6_13_anchors",
    "build_census_d02_2011_measures",
    "build_census_d03_2011_measures",
    "build_census_d04_2011_measures",
    "build_census_d07_2011_measures",
    "build_census_2011_industry_measures",
    "build_census_2011_occupation_measures",
    "build_census_2011_housing_measures"
  )) {
    pattern <- paste0(consumer, "(")
    expect_match(targets, pattern, fixed = TRUE, info = consumer)
  }
})

test_that("current public build helper scripts parse", {
  expect_silent(parse(repo_file("_targets.R")))
  for (file in list.files(
    repo_file("R", "pipeline"),
    pattern = "^extended_.*_targets\\.R$",
    full.names = TRUE
  )) {
    expect_silent(parse(file))
  }
  expect_silent(parse(repo_file("scripts", "check_required_outputs.R")))
  expect_silent(parse(repo_file("scripts", "check_targets_process.R")))
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

test_that("audit workspace cleanup removes transient state and preserves optional outputs", {
  root <- tempfile("audit-clean-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  transient <- c(
    "outputs/diagnostics/build/build.csv",
    "outputs/diagnostics/public/public.csv",
    "outputs/diagnostics/root.csv",
    "outputs/diagnostics/extended/district_lineage_v2/stale.csv",
    "outputs/derived/district_lineage_v2/stale.gpkg"
  )
  preserved <- c(
    "outputs/diagnostics/extended/current.csv",
    "outputs/benchmarking/current.csv",
    "outputs/derived/district_lineage/current.gpkg"
  )

  for (path in c(transient, preserved)) {
    full <- file.path(root, path)
    dir.create(dirname(full), recursive = TRUE, showWarnings = FALSE)
    writeLines("fixture", full)
  }

  status <- system2(
    "bash",
    c(
      shQuote(repo_file("scripts", "clean_audit_workspace.sh")),
      shQuote(root)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(status, "status"))
  expect_false(any(file.exists(file.path(root, transient))))
  expect_true(all(file.exists(file.path(root, preserved))))
  expect_true(dir.exists(file.path(root, "outputs/diagnostics/build")))
  expect_true(dir.exists(file.path(root, "outputs/diagnostics/public")))
  expect_true(dir.exists(file.path(root, "outputs/diagnostics/extended")))
  expect_true(dir.exists(file.path(root, "outputs/benchmarking")))
})



test_that("targets graph separates public diagnostics, extended diagnostics, and benchmarks", {
  src <- repo_text("_targets.R")
  extended <- repo_extended_target_text()

  expect_match(src, "core_pipeline_targets <- list", fixed = TRUE)
  expect_match(src, 'source("R/pipeline/extended_diagnostic_targets.R")', fixed = TRUE)
  expect_match(
    src,
    "extended_diagnostic_targets <- extended_diagnostic_target_definitions()",
    fixed = TRUE
  )
  expect_match(src, "benchmark_targets <- list", fixed = TRUE)
  expect_match(src, "diag_public_iv_panel", fixed = TRUE)
  expect_match(src, "diag_public_spatial_autocorrelation", fixed = TRUE)
  expect_match(src, "diag_public_spatial_autocorrelation_files", fixed = TRUE)
  expect_match(src, "save_spatial_autocorrelation_diagnostics(diag_public_spatial_autocorrelation), format = \"file\"", fixed = TRUE)
  expect_match(extended, "extended_diagnostic_target_definitions <- function()", fixed = TRUE)
  expect_match(extended, "diag_ext_missingness", fixed = TRUE)
  expect_match(src, "bench_ame_methods", fixed = TRUE)
  expect_match(src, "bench_consumption_distribution_domains", fixed = TRUE)
  expect_match(src, "EMI_RUN_EXTENDED_DIAGNOSTICS", fixed = TRUE)
  expect_match(src, "EMI_RUN_BENCHMARKS", fixed = TRUE)
  public_spatial_line <- grep(
    "tar_target\\(diag_public_spatial_autocorrelation,",
    strsplit(src, "\n", fixed = TRUE)[[1]],
    value = TRUE
  )
  expect_length(public_spatial_line, 1L)
  expect_false(grepl('tar_cue(mode = "always")', public_spatial_line, fixed = TRUE))
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

  expect_match(report, "report_value(\"ame_edu_free_pct\")", fixed = TRUE)
  expect_match(docs_note, "report_value(\"moran_iv_residual_p\")", fixed = TRUE)
  expect_match(builder, "moran_iv_residual_p", fixed = TRUE)
  expect_match(builder, "moran_consumption_growth_p", fixed = TRUE)
  expect_match(spatial_values, "spatial_p_value", fixed = TRUE)
  expect_match(checker, "public_report_value_sources", fixed = TRUE)
  expect_match(checker, "pattern <-", fixed = TRUE)
  expect_match(checker, "gregexpr(pattern", fixed = TRUE)
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
  expect_match(contract, "multicollinearity_diagnostics.csv", fixed = TRUE)
  expect_match(contract, "anderson_rubin_candidate_designs.csv", fixed = TRUE)
  expect_match(required, "required_public_render_inputs()", fixed = TRUE)
  expect_match(final, "required_final_documents(require_application_samples)", fixed = TRUE)
  expect_match(audit, "required_final_artifacts()", fixed = TRUE)
  expect_match(audit, "Public VIF/GVIF diagnostics are unavailable", fixed = TRUE)
  expect_match(audit, "Candidate-design Anderson-Rubin diagnostic is unavailable", fixed = TRUE)
  expect_match(audit, "confidence-set inversion disagrees", fixed = TRUE)
  expect_match(audit, "disconnected or grid-truncated confidence set", fixed = TRUE)
})

test_that("public documentation and samples do not advertise superseded methods work", {
  docs_note <- repo_text("docs", "district-matching.qmd")
  samples <- repo_text("R", "application_samples", "render_writing_sample.R")

  expect_false(grepl("depending on the results of LM tests", docs_note, fixed = TRUE))
  expect_match(docs_note, "do not by themselves determine which causal spatial model", fixed = TRUE)
  expect_false(grepl("pending a validated district-geometry join", samples, fixed = TRUE))
  expect_match(samples, "weakly identified", fixed = TRUE)
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
  expect_match(targets, "legacy_comparison_targets <- list(", fixed = TRUE)
  expect_match(
    targets,
    "legacy_comparison_targets, extended_diagnostic_targets",
    fixed = TRUE
  )
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

test_that("review archives do not carry stale root-level diagnostic CSVs", {
  archive <- repo_text("scripts", "make_review_archive.sh")

  expect_match(archive, "find \"$tmpdir/outputs/diagnostics\" -maxdepth 1 -type f -name '*.csv' -delete", fixed = TRUE)
})


test_that("debug review archives retain intermediate diagnostics but exclude raw data", {
  skip_if(Sys.which("git") == "")
  skip_if(Sys.which("zip") == "")
  root <- tempfile("review-archive-intermediates-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  dir.create(file.path(root, "scripts"), recursive = TRUE)
  dir.create(
    file.path(root, "outputs", "diagnostics", "extended"),
    recursive = TRUE
  )
  dir.create(file.path(root, "outputs", "benchmarking"), recursive = TRUE)
  dir.create(file.path(root, "data", "processed"), recursive = TRUE)
  dir.create(file.path(root, "data", "raw"), recursive = TRUE)
  file.copy(
    repo_file("scripts", "make_review_archive.sh"),
    file.path(root, "scripts", "make_review_archive.sh")
  )
  writeLines("tracked", file.path(root, "README.md"))
  writeLines(
    "diagnostic",
    file.path(root, "outputs", "diagnostics", "extended", "intermediate.csv")
  )
  writeLines(
    "benchmark",
    file.path(root, "outputs", "benchmarking", "runtime.csv")
  )
  writeLines("processed", file.path(root, "data", "processed", "panel.csv"))
  writeLines("raw", file.path(root, "data", "raw", "private.csv"))

  system2("git", c("-C", shQuote(root), "init", "-q"))
  system2(
    "git",
    c("-C", shQuote(root), "add", "README.md", "scripts/make_review_archive.sh")
  )

  old_wd <- setwd(root)
  on.exit(setwd(old_wd), add = TRUE)
  output <- system2(
    "bash",
    c(
      "scripts/make_review_archive.sh",
      "--without-samples",
      "--allow-incomplete",
      "--output", "review.zip"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"))
  listing <- utils::unzip("review.zip", list = TRUE)$Name
  expect_true(
    "outputs/diagnostics/extended/intermediate.csv" %in% listing
  )
  expect_true("outputs/benchmarking/runtime.csv" %in% listing)
  expect_true("data/processed/panel.csv" %in% listing)
  expect_false("data/raw/private.csv" %in% listing)
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

test_that("selected target runner uses the programmatic targets API and shared warning metadata", {
  runner <- repo_text("scripts", "run_targets_checked.R")
  helper <- repo_text("scripts", "target_metadata_helpers.R")

  expect_match(runner, "rlang::inject", fixed = TRUE)
  expect_match(runner, "tidyselect::all_of(!!selected_target_names)", fixed = TRUE)
  expect_false(grepl("eval(parse", runner, fixed = TRUE))
  expect_match(runner, "record_target_warnings", fixed = TRUE)
  expect_match(runner, "targets::tar_progress", fixed = TRUE)
  expect_match(runner, "target_run_metadata_scope", fixed = TRUE)
  expect_match(helper, "target_metadata_selection", fixed = TRUE)
  expect_match(helper, "rlang::inject", fixed = TRUE)
  expect_match(helper, "targets" , fixed = TRUE)
  expect_match(helper, "target_warnings.csv", fixed = TRUE)
})

test_that("selected target warning scope includes executed dependencies", {
  env <- new.env(parent = globalenv())
  sys.source(repo_file("scripts", "target_metadata_helpers.R"), envir = env)
  progress <- data.frame(
    name = c("selected", "dependency", "unrelated"),
    progress = c("skipped", "completed", "skipped"),
    stringsAsFactors = FALSE
  )

  scope <- env$target_run_metadata_scope("selected", progress)

  expect_setequal(scope, c("selected", "dependency"))
  expect_false("unrelated" %in% scope)
})

test_that("programmatic metadata selection stays inside tidyselect context", {
  skip_if_not_installed("rlang")
  skip_if_not_installed("tidyselect")

  env <- new.env(parent = globalenv())
  sys.source(repo_file("scripts", "target_metadata_helpers.R"), envir = env)
  selection <- env$target_metadata_selection(c("selected", "missing"))
  columns <- data.frame(
    selected = logical(),
    unrelated = logical(),
    check.names = FALSE
  )

  resolved <- tidyselect::eval_select(selection, columns)

  expect_identical(names(resolved), "selected")
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
  expect_false(grepl("pdftools", description, fixed = TRUE))
  expect_match(description, "testthat", fixed = TRUE)
  expect_match(targets, "packages = character()", fixed = TRUE)
  expect_false(file.exists(file.path(root, "R", "packages.R")))
})

test_that("legacy tracker and panel remain optional comparison inputs", {
  target_lines <- readLines(repo_file("_targets.R"), warn = FALSE)
  targets <- paste(target_lines, collapse = "\n")
  diagnostics <- repo_text("R", "diagnostics", "diagnose_district_tracker_sources.R")
  root <- dirname(repo_file("README.md"))
  geography_start <- match(TRUE, grepl("legacy_geography_targets <- list(", target_lines, fixed = TRUE))
  comparison_start <- match(TRUE, grepl("legacy_comparison_targets <- list(", target_lines, fixed = TRUE))
  extended_start <- match(TRUE, grepl("extended_diagnostic_targets <- extended_diagnostic_target_definitions()", target_lines, fixed = TRUE))
  geography <- paste(target_lines[geography_start:(comparison_start - 1L)], collapse = "\n")
  comparison <- paste(target_lines[comparison_start:(extended_start - 1L)], collapse = "\n")

  expect_false(grepl("processed_district_tracker_file", targets, fixed = TRUE))
  expect_match(targets, "prepare_district_join_map(district_harmonization_crosswalk)", fixed = TRUE)
  expect_false(grepl("district_panel_legacy", geography, fixed = TRUE))
  expect_match(comparison, "district_panel_legacy", fixed = TRUE)
  expect_match(comparison, "strict_district_panel_validation = FALSE", fixed = TRUE)
  expect_match(comparison, "strict_analysis_panel_validation = FALSE", fixed = TRUE)
  expect_match(diagnostics, "data/metadata/district_harmonization_crosswalk.csv", fixed = TRUE)
  expect_false(file.exists(file.path(root, "data", "processed", "district_tracker_2001_2007_2017_2020.csv")))
})

test_that("new-machine setup restores the tracked renv lockfile without rewriting it", {
  skip_if(Sys.which("make") == "")
  output <- system2(
    "make",
    c("-n", "-f", shQuote(repo_file("Makefile")), "init-renv"),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_identical(attr(output, "status") %||% 0L, 0L)
  expect_true(any(grepl("renv::restore", output, fixed = TRUE)))
  expect_false(any(grepl("renv::snapshot|renv::install|renv::init", output)))

  scripts <- list.files("scripts", pattern = "\\.[Rr]$", full.names = TRUE)
  guidance <- unlist(lapply(scripts, readLines, warn = FALSE), use.names = FALSE)
  expect_false(any(grepl("make init-renv", guidance, fixed = TRUE)))
})

test_that("source syntax preflight is centralized and read-only", {
  helper <- repo_text("scripts", "check_source_syntax.sh")
  audit <- repo_text("scripts", "run_public_build_audit.sh")

  expect_match(audit, "bash scripts/check_source_syntax.sh", fixed = TRUE)
  expect_match(helper, "bash -n", fixed = TRUE)
  expect_match(helper, "ast.parse", fixed = TRUE)
  expect_match(helper, "json.loads", fixed = TRUE)
  expect_match(helper, "DESCRIPTION runtime dependencies", fixed = TRUE)
  expect_match(helper, "Rscript -", fixed = TRUE)
  expect_match(helper, "knitr::purl", fixed = TRUE)
  expect_false(grepl("py_compile", helper, fixed = TRUE))
  expect_false(grepl("renv::snapshot", helper, fixed = TRUE))
})

test_that("public build audit owns mandatory syntax and full-test gates", {
  audit <- readLines(repo_file("scripts", "run_public_build_audit.sh"), warn = FALSE)

  syntax_line <- grep("bash scripts/check_source_syntax.sh", audit, fixed = TRUE)
  test_line <- grep("  make test", audit, fixed = TRUE)
  pipeline_line <- grep('current_stage="public-final-check"', audit, fixed = TRUE)

  expect_length(syntax_line, 1L)
  expect_length(test_line, 1L)
  expect_length(pipeline_line, 1L)
  expect_lt(syntax_line, test_line)
  expect_lt(test_line, pipeline_line)
  expect_true(any(grepl('trap dump_diagnostics EXIT', audit, fixed = TRUE)))
  expect_true(any(grepl('make_debug_archive "error-${current_stage}"', audit, fixed = TRUE)))
})

test_that("target issue printer selects columns without data-frame drop warnings", {
  env <- new.env(parent = globalenv())
  sys.source(repo_file("scripts", "target_metadata_helpers.R"), envir = env)
  rows <- data.frame(
    name = "district_panel",
    error = "example failure",
    extra = "ignored",
    stringsAsFactors = FALSE
  )

  expect_warning(
    output <- capture.output(env$print_target_issues(rows, "error", "Errored targets:")),
    NA
  )
  expect_match(paste(output, collapse = "\n"), "district_panel", fixed = TRUE)
  expect_match(paste(output, collapse = "\n"), "example failure", fixed = TRUE)
  expect_false(grepl("ignored", paste(output, collapse = "\n"), fixed = TRUE))
})

test_that("conference poster is a first-class final output", {
  targets <- repo_text("_targets.R")
  renderer <- repo_text("R", "output", "render_public_artifacts.R")
  contract <- repo_text("scripts", "public_output_contract.R")
  poster <- repo_text("posters", "2026_predoc_conference", "poster.qmd")

  expect_match(targets, "tar_target(poster, render_poster_pdf", fixed = TRUE)
  expect_match(renderer, "render_poster_pdf", fixed = TRUE)
  expect_match(renderer, "render_poster_png", fixed = TRUE)
  expect_match(contract, "posters/2026_predoc_conference/poster.pdf", fixed = TRUE)
  expect_match(contract, "posters/2026_predoc_conference/RishavRoy-Education.png", fixed = TRUE)
  expect_match(poster, "poster_first_stage_specs.pdf", fixed = TRUE)
  expect_match(poster, "poster_second_stage_specs.pdf", fixed = TRUE)
  expect_match(poster, "map_linguistic_distance.pdf", fixed = TRUE)
  expect_match(poster, "map_residual_emi_exposure.pdf", fixed = TRUE)
  expect_match(poster, "map_residual_linguistic_distance.pdf", fixed = TRUE)
})

poster_renderer_test_env <- function() {
  env <- new.env(parent = globalenv())
  sys.source(repo_file("R", "output", "render_public_artifacts.R"), envir = env)
  env
}

test_that("poster renderer declares source assets under assets", {
  renderer <- poster_renderer_test_env()
  assets <- renderer$poster_required_assets()

  expect_identical(
    assets,
    c(
      "assets/uw-logo-horizontal-full-color-print.pdf",
      "assets/repo-qr.svg"
    )
  )
  expect_true(all(file.exists(vapply(assets, repo_file, character(1)))))
})

test_that("poster Typst format supplies both standard template partials", {
  poster_qmd <- repo_file("posters", "2026_predoc_conference", "poster.qmd")
  renderer <- poster_renderer_test_env()
  paths <- renderer$validate_poster_typst_templates(poster_qmd)

  expect_named(paths, c("template", "show"))
  expect_true(all(file.exists(paths)))
})

test_that("poster treatment equation uses the all-child exposure definition", {
  poster <- repo_text("posters", "2026_predoc_conference", "poster.qmd")

  expect_match(
    poster,
    "\\mathrm{EMIE}_d = \\Pr(\\text{enrolled and in EMI})_d.",
    fixed = TRUE
  )
  expect_false(grepl("\\times \\Pr", poster, fixed = TRUE))
})


test_that("poster citations resolve through the project bibliography", {
  poster <- paste(
    readLines(repo_file("posters", "2026_predoc_conference", "poster.qmd"), warn = FALSE),
    collapse = "\n"
  )
  bibliography <- readLines(repo_file("paper", "references.bib"), warn = FALSE)

  citation_groups <- regmatches(
    poster,
    gregexpr("\\[@[^]]+\\]", poster, perl = TRUE)
  )[[1]]
  citation_keys <- unique(unlist(regmatches(
    citation_groups,
    gregexpr("@[[:alnum:]_:.#$%&+?/-]+", citation_groups, perl = TRUE)
  )))
  citation_keys <- sub("^@", "", citation_keys)
  bibliography_keys <- sub(
    "^@[[:alpha:]]+\\{([^,]+),.*$",
    "\\1",
    grep("^@[[:alpha:]]+\\{[^,]+,", bibliography, value = TRUE)
  )

  expect_match(poster, "\nciteproc: true\n", fixed = TRUE)
  expect_setequal(intersect(citation_keys, bibliography_keys), citation_keys)
})


test_that("public audit caches requested diagnostics before rendering public outputs", {
  audit <- readLines(
    repo_file("scripts", "run_public_build_audit.sh"),
    warn = FALSE
  )
  geometry_line <- match(TRUE, grepl(
    "make lineage-geometry-build", audit, fixed = TRUE
  ))
  diagnostics_line <- match(TRUE, grepl(
    "make extended-diagnostics", audit, fixed = TRUE
  ))
  public_line <- match(TRUE, grepl(
    'make "$check_target"', audit, fixed = TRUE
  ))

  expect_true(all(is.finite(c(geometry_line, diagnostics_line, public_line))))
  expect_lt(geometry_line, diagnostics_line)
  expect_lt(diagnostics_line, public_line)
})

test_that("reviewed primary lineage is public and alternatives remain diagnostic", {
  target_file <- readLines(repo_file("_targets.R"), warn = FALSE)
  core_start <- match(TRUE, grepl("core_pipeline_targets <- list(", target_file, fixed = TRUE))
  extended_start <- match(
    TRUE,
    grepl(
      "extended_diagnostic_targets <- extended_diagnostic_target_definitions()",
      target_file,
      fixed = TRUE
    )
  )
  core <- paste(target_file[core_start:(extended_start - 1L)], collapse = "\n")
  extended <- repo_extended_target_text()

  expect_match(
    core,
    "tar_target(district_panel, district_panel_primary)",
    fixed = TRUE
  )
  expect_match(core, "save_processed_district_panel(district_panel)", fixed = TRUE)
  expect_false(grepl("tar_target(iv_formulas", core, fixed = TRUE))
  expect_false(grepl("tar_target(iv_models", core, fixed = TRUE))
  expect_false(grepl("tar_target(first_stage_tests", core, fixed = TRUE))
  expect_match(core, "estimate_2sls(district_panel, revised_iv_formulas, cfg)", fixed = TRUE)
  expect_match(extended, "estimate_2sls(district_panel_conservative, revised_iv_formulas, cfg)", fixed = TRUE)
  expect_match(extended, "estimate_2sls(district_panel_legacy, legacy_iv_formulas, cfg)", fixed = TRUE)
  expect_match(extended, "iv_models_conservative_legacy_spec", fixed = TRUE)
  expect_match(extended, "attach_census_2001_controls", fixed = TRUE)
  expect_match(extended, "diag_ext_lineage_downstream", fixed = TRUE)
})


test_that("source checks validate application-sample excerpt markers", {
  checker <- repo_text("scripts", "check_source_syntax.sh")
  expect_match(checker, "Rscript scripts/check_sample_specs.R", fixed = TRUE)
})

test_that("coding-sample specifications use one valid nonempty marker pair", {
  env <- new.env(parent = globalenv())
  sys.source(repo_file("R", "application_samples", "extract_code_excerpts.R"), envir = env)
  specs <- list.files(
    repo_file("application-samples", "specs"),
    pattern = "^coding-.*\\.yml$",
    full.names = TRUE
  )
  expect_gt(length(specs), 0L)

  for (spec_path in specs) {
    spec <- yaml::read_yaml(spec_path)
    for (excerpt in spec$excerpts) {
      lines <- env$extract_between_sample_markers(
        repo_file(excerpt$file),
        excerpt$id
      )
      expect_true(
        sum(nzchar(trimws(lines))) > 1L,
        info = excerpt$id
      )
    }
  }
})

test_that("poster Typst assets resolve from the repository root", {
  poster <- repo_text("posters", "2026_predoc_conference", "poster.qmd")
  template <- repo_text(
    "posters", "2026_predoc_conference", "_extensions", "poster",
    "typst-template.typ"
  )
  renderer_text <- repo_text("R", "output", "render_public_artifacts.R")
  renderer <- poster_renderer_test_env()
  targets <- repo_text("_targets.R")
  source <- paste(poster, template)
  root_relative_assets <- paste0("/", renderer$poster_required_assets())

  expect_true(all(vapply(
    root_relative_assets,
    function(path) grepl(path, source, fixed = TRUE),
    logical(1)
  )))
  expect_match(renderer_text, 'env = paste0("TYPST_ROOT=", shQuote(typst_root))', fixed = TRUE)
  expect_match(targets, "render_poster_pdf(poster_qmd, figure_files, poster_assets, paths$root)", fixed = TRUE)
  expect_false(grepl("brand-logo-images", source, fixed = TRUE))
  expect_false(grepl("outputs/derived/poster", source, fixed = TRUE))
})


test_that("poster PNG rendering supports paths containing spaces", {
  skip_if(!nzchar(Sys.which("pdftoppm")), "pdftoppm is unavailable")
  renderer <- poster_renderer_test_env()
  dir <- file.path(tempdir(), "poster preview with spaces")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  pdf <- file.path(dir, "poster source.pdf")
  png <- file.path(dir, "poster preview.png")
  grDevices::pdf(pdf, width = 2, height = 2)
  graphics::plot.new()
  graphics::text(0.5, 0.5, "poster")
  grDevices::dev.off()

  out <- renderer$render_poster_png(pdf, png, dpi = 72)

  expect_identical(out, png)
  expect_true(file.exists(png))
  expect_gt(file.info(png)$size, 0)
})


test_that("poster delivery and typography contracts remain stable during drafting", {
  renderer <- repo_text("R", "output", "render_public_artifacts.R")
  template <- repo_text("posters", "2026_predoc_conference", "_extensions", "poster", "typst-template.typ")

  # Section names and order are intentionally not asserted while the poster is
  # being drafted. Rendering, delivery naming, and shared typography remain
  # stable contracts.
  expect_match(renderer, "RishavRoy-Education.png", fixed = TRUE)
  expect_match(template, 'weight: "bold", fill: white, footer_url', fixed = TRUE)
  expect_match(template, 'weight: "bold", fill: white, footer_email_ids', fixed = TRUE)
  expect_match(template, "v(16pt, weak: true)", fixed = TRUE)
  expect_match(template, "v(1pt)", fixed = TRUE)
})

test_that("Census downloader processes both manifests and skips present files", {
  root <- tempfile("census-download-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(file.path(root, "data", "metadata"), recursive = TRUE)

  manifests <- list(
    data.frame(
      table = "C13", state_code = "01",
      relative_path = "data/raw/census_2001/age/C13/PC01_C13_01.xls",
      url = "https://censusindia.gov.in/one.xls",
      stringsAsFactors = FALSE
    ),
    data.frame(
      table = "C13", state_code = "01",
      relative_path = "data/raw/census_2011/age/C13/DDW-0100C-13.xls",
      url = "https://censusindia.gov.in/two.xls",
      stringsAsFactors = FALSE
    )
  )
  for (i in seq_along(manifests)) {
    write.table(
      manifests[[i]],
      file.path(
        root, "data", "metadata",
        sprintf("census_%d_download_manifest.tsv", c(2001, 2011)[[i]])
      ),
      sep = "\t", quote = FALSE, row.names = FALSE
    )
  }

  present <- file.path(root, manifests[[1]]$relative_path)
  dir.create(dirname(present), recursive = TRUE)
  writeLines("existing", present)

  log_path <- file.path(root, "curl.log")
  fake_curl <- file.path(root, "curl")
  writeLines(c(
    "#!/usr/bin/env bash",
    "set -euo pipefail",
    "out=''",
    "url=''",
    "while [[ $# -gt 0 ]]; do",
    "  case \"$1\" in",
    "    --output) out=\"$2\"; shift 2 ;;",
    "    --retry|--retry-delay|--connect-timeout) shift 2 ;;",
    "    --fail|--location) shift ;;",
    "    *) url=\"$1\"; shift ;;",
    "  esac",
    "done",
    "printf '%s\\n' \"$url\" >> \"$FAKE_CURL_LOG\"",
    "printf 'downloaded\\n' > \"$out\""
  ), fake_curl)
  Sys.chmod(fake_curl, mode = "0755")

  old <- Sys.getenv(c("EMI_PROJECT_ROOT", "CURL_BIN", "FAKE_CURL_LOG"), unset = NA_character_)
  on.exit({
    for (name in names(old)) {
      if (is.na(old[[name]])) {
        Sys.unsetenv(name)
      } else {
        do.call(Sys.setenv, setNames(list(old[[name]]), name))
      }
    }
  }, add = TRUE)
  Sys.setenv(EMI_PROJECT_ROOT = root, CURL_BIN = fake_curl, FAKE_CURL_LOG = log_path)

  output <- system2(
    "bash",
    shQuote(repo_file("scripts", "download_census_tables.sh")),
    stdout = TRUE,
    stderr = TRUE
  )

  expect_null(attr(output, "status"))
  expect_identical(readLines(present), "existing")
  downloaded <- file.path(root, manifests[[2]]$relative_path)
  expect_identical(readLines(downloaded), "downloaded")
  expect_identical(readLines(log_path), manifests[[2]]$url)
  expect_true(any(grepl("1 downloaded, 1 already present", output, fixed = TRUE)))
})

test_that("canonical public audit restores the locked R library before synchronization checks", {
  audit <- repo_text("scripts", "run_public_build_audit.sh")
  syntax <- repo_text("scripts", "check_source_syntax.sh")

  restore_pos <- regexpr(
    'current_stage="restore-project-library"',
    audit,
    fixed = TRUE
  )[[1L]]
  static_pos <- regexpr(
    'current_stage="static-parse-checks"',
    audit,
    fixed = TRUE
  )[[1L]]

  expect_gt(restore_pos, 0L)
  expect_gt(static_pos, restore_pos)
  expect_match(audit, 'make restore', fixed = TRUE)
  expect_match(
    syntax,
    'status <- renv::status(dev = TRUE)',
    fixed = TRUE
  )
})

test_that("lower-tail welfare runtime dependency is exercised rather than conditionally skipped", {
  description <- repo_text("DESCRIPTION")
  welfare <- repo_text("R", "measures", "build_consumption_district_welfare.R")
  tests <- repo_text("tests", "testthat", "test-consumption-district-welfare.R")

  expect_match(description, "    convey,", fixed = TRUE)
  expect_match(
    welfare,
    'need_pkg("convey", "design-based lower-tail welfare estimates")',
    fixed = TRUE
  )
  expect_false(grepl(
    'skip_if_not_installed("convey")',
    tests,
    fixed = TRUE
  ))
})

test_that("consumption welfare targets cache core and distributional work separately", {
  targets <- repo_text("_targets.R")
  welfare <- repo_text("R", "measures", "build_consumption_district_welfare.R")
  audit <- repo_text("scripts", "run_public_build_audit.sh")

  expect_false(grepl("consumption_welfare_outcomes_core", targets, fixed = TRUE))
  expect_false(grepl("consumption_welfare_outcomes_distributional", targets, fixed = TRUE))
  expect_match(targets, "consumption_welfare_outcomes", fixed = TRUE)
  expect_match(targets, "consumption_district_welfare_core_2011_12_type2", fixed = TRUE)
  expect_match(targets, "consumption_district_welfare_distributional_2011_12_type2", fixed = TRUE)
  expect_match(welfare, "consumption_welfare_registry_for_survey", fixed = TRUE)
  expect_match(welfare, "survey::svyby", fixed = TRUE)
  expect_match(welfare, "multicore = consumption_domain_multicore()", fixed = TRUE)
  expect_false(grepl("lapply(\\n    plain_chr(support$district_2001)", welfare, fixed = TRUE))
  expect_match(audit, 'EMI_CONSUMPTION_DOMAIN_CORES="${EMI_CONSUMPTION_DOMAIN_CORES:-4}"', fixed = TRUE)
  expect_match(targets, "consumption_households_lineaged_2004_05", fixed = TRUE)
  expect_false(grepl("survey_gini", repo_text("R", "benchmarking", "benchmarking_targets.R"), fixed = TRUE))
})

test_that("public audit checks the targets process before tests and pipeline execution", {
  audit <- repo_text("scripts", "run_public_build_audit.sh")
  checker <- repo_text("scripts", "check_targets_process.R")
  description <- repo_text("DESCRIPTION")
  expect_match(audit, 'current_stage="targets-process-preflight"', fixed = TRUE)
  expect_match(audit, "Rscript scripts/check_targets_process.R", fixed = TRUE)
  expect_lt(
    regexpr("Rscript scripts/check_targets_process.R", audit, fixed = TRUE)[[1L]],
    regexpr('current_stage="unit-tests"', audit, fixed = TRUE)[[1L]]
  )
  expect_lt(
    regexpr("Rscript scripts/check_targets_process.R", audit, fixed = TRUE)[[1L]],
    regexpr('current_stage="public-final-check"', audit, fixed = TRUE)[[1L]]
  )
  expect_match(description, "    ps,", fixed = TRUE)
  expect_match(checker, "targets::tar_pid()", fixed = TRUE)
  expect_match(checker, "ps::ps_is_running(ps::ps_handle(pid))", fixed = TRUE)
  expect_match(checker, "targets::tar_unblock_process()", fixed = TRUE)
  expect_match(checker, "kill ", fixed = TRUE)
  expect_false(grepl("ps::ps_kill", checker, fixed = TRUE))
})

test_that("targets process recovery never unblocks a live recorded process", {
  checker <- repo_text("scripts", "check_targets_process.R")
  live <- regexpr("if (isTRUE(process_is_running))", checker, fixed = TRUE)[[1L]]
  fail <- regexpr("quit(status = 3L)", checker, fixed = TRUE)[[1L]]
  unblock <- regexpr("targets::tar_unblock_process()", checker, fixed = TRUE)[[1L]]
  expect_gt(live, 0L)
  expect_gt(fail, live)
  expect_gt(unblock, fail)
})
